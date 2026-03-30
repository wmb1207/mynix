#!/usr/bin/env janet

# docker.janet — bhyve + vm-bhyve + Alpine Linux VM for Docker on FreeBSD
#
# Usage:
#   janet docker.janet preflight   check VT-x + kernel modules
#   janet docker.janet install     pkg install vm-bhyve, firmware, docker CLI
#   janet docker.janet init        vm-bhyve init + virtual switch + rc.conf
#   janet docker.janet create      download Alpine ISO, create + boot VM (hands off to you)
#   janet docker.janet context     after Alpine is up: inject SSH key + docker context
#   janet docker.janet start       start the docker VM
#   janet docker.janet stop        stop the docker VM
#   janet docker.janet status      show VM status + docker context

# -- config --
(def vm-name    "docker")
(def vm-dir     "/vm")
(def vm-switch  "public")
(def vm-cpus    "2")
(def vm-ram     "1G")
(def vm-disk    "20G")
(def vm-user    "root")

(def alpine-version "3.21.3")
(def alpine-iso     (string "alpine-virt-" alpine-version "-x86_64.iso"))
(def alpine-url     (string "https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/" alpine-iso))
(def alpine-sha-url (string alpine-url ".sha256"))
(def iso-path       (string vm-dir "/.iso/" alpine-iso))

# -- helpers --
(defn bold  [s] (string "\e[1m" s "\e[0m"))
(defn green [s] (string "\e[32m" s "\e[0m"))
(defn red   [s] (string "\e[31m" s "\e[0m"))
(defn cyan  [s] (string "\e[36m" s "\e[0m"))
(defn dim   [s] (string "\e[2m"  s "\e[0m"))

(defn step [s]  (print "\n" (bold (cyan (string "── " s " ──")))))
(defn ok   [s]  (print (green "  ✓ ") s))
(defn fail [s]  (print (red   "  ✗ ") s))
(defn info [s]  (print (dim   "  → ") s))
(defn note [s]  (print "\n" (bold "NOTE: ") s "\n"))

(defn sh! [args]
  (info (string/join args " "))
  (let [exit (os/execute args :p)]
    (when (not= 0 exit)
      (fail (string "command failed (exit " exit ")"))
      (os/exit exit))))

(defn sh-ok? [args]
  (= 0 (os/execute args :p {:out :pipe :err :pipe})))

(defn sh-out [args]
  (let [proc (os/spawn args :p {:out :pipe})
        out  (:read (proc :out) :all)
        _    (:wait proc)]
    (string/trimr (or out ""))))

(defn rc-conf-has? [key]
  (let [current (slurp "/etc/rc.conf")]
    (string/find key current)))

(defn rc-conf-append! [line]
  (unless (rc-conf-has? (first (string/split "=" line)))
    (info (string "rc.conf: " line))
    (let [f (file/open "/etc/rc.conf" :a)]
      (file/write f (string line "\n"))
      (file/close f))))

(defn must-be-root []
  (when (not= "0" (sh-out ["id" "-u"]))
    (fail "this command must be run as root (doas janet docker.janet ...)")
    (os/exit 1)))

# ── preflight ────────────────────────────────────────────────────────────────

(defn preflight []
  (step "preflight checks")

  # VT-x support
  (let [vmx (sh-out ["sysctl" "-n" "hw.vmm.vmx.initialized"])]
    (if (= vmx "1")
      (ok "VT-x is enabled")
      (do
        (fail "VT-x not detected — check BIOS settings (Intel Virtualization Technology)")
        (os/exit 1))))

  # vmm.ko loaded?
  (if (sh-ok? ["kldstat" "-q" "-n" "vmm"])
    (ok "vmm.ko already loaded")
    (do
      (info "loading vmm.ko...")
      (sh! ["kldload" "vmm"])
      (ok "vmm.ko loaded")))

  # if_bridge for VM networking
  (if (sh-ok? ["kldstat" "-q" "-n" "if_bridge"])
    (ok "if_bridge.ko already loaded")
    (do
      (sh! ["kldload" "if_bridge"])
      (ok "if_bridge.ko loaded")))

  (ok "all preflight checks passed")
  (note (string "To make modules persist across reboots, add to /boot/loader.conf:\n"
                "  vmm_load=\"YES\"\n"
                "  if_bridge_load=\"YES\"")))

# ── install ──────────────────────────────────────────────────────────────────

(defn install []
  (must-be-root)
  (step "installing packages")

  (sh! ["pkg" "install" "-y"
        "vm-bhyve"
        "bhyve-firmware"   # UEFI firmware (BHYVE_UEFI.fd)
        "grub2-bhyve"      # bootloader for Linux guests
        "docker"           # CLI only — daemon runs in the VM
        "docker-compose"
        ])

  (ok "packages installed"))

# ── init ─────────────────────────────────────────────────────────────────────

(defn init []
  (must-be-root)
  (step "initialising vm-bhyve")

  # create VM dir
  (unless (sh-ok? ["test" "-d" vm-dir])
    (sh! ["mkdir" "-p" vm-dir])
    (ok (string "created " vm-dir)))

  # vm-bhyve init
  (sh! ["vm" "init"])
  (ok "vm-bhyve initialised")

  # create virtual switch (bridged to your physical NIC)
  # detect first active NIC
  (let [nic (sh-out ["sh" "-c" "route get default | awk '/interface:/{print $2}'"])]
    (if (= "" nic)
      (do
        (fail "could not detect default NIC — run manually: vm switch create -t bridge -i <nic> public")
        (os/exit 1))
      (do
        (info (string "bridging switch to " nic))
        (sh! ["vm" "switch" "create" "-t" "bridge" "-i" nic vm-switch])
        (ok (string "created switch '" vm-switch "' on " nic)))))

  # rc.conf entries
  (step "configuring rc.conf")
  (rc-conf-append! (string "vm_enable=\"YES\""))
  (rc-conf-append! (string "vm_dir=\"" vm-dir "\""))
  (ok "rc.conf updated")

  # loader.conf for modules
  (let [loader "/boot/loader.conf"
        entries ["vmm_load=\"YES\"" "if_bridge_load=\"YES\""]
        current (if (sh-ok? ["test" "-f" loader]) (slurp loader) "")]
    (each e entries
      (let [key (first (string/split "=" e))]
        (unless (string/find key current)
          (let [f (file/open loader :a)]
            (file/write f (string e "\n"))
            (file/close f))
          (ok (string "loader.conf: " e))))))

  (ok "init complete"))

# ── create ───────────────────────────────────────────────────────────────────

(defn download-iso []
  (step "downloading Alpine ISO")
  (sh! ["mkdir" "-p" (string vm-dir "/.iso")])

  (if (sh-ok? ["test" "-f" iso-path])
    (ok (string alpine-iso " already downloaded"))
    (do
      (info (string "fetching " alpine-url))
      (sh! ["fetch" "-o" iso-path alpine-url])
      (ok "ISO downloaded")))

  # verify checksum
  (let [sha-file (string iso-path ".sha256")]
    (sh! ["fetch" "-o" sha-file alpine-sha-url])
    (sh! ["sha256" "-c" (string/trimr (slurp sha-file)) iso-path])
    (ok "checksum verified")))

(defn create-vm []
  (step "creating VM")

  (if (sh-ok? ["test" "-d" (string vm-dir "/" vm-name)])
    (ok (string "VM '" vm-name "' already exists"))
    (do
      (sh! ["vm" "create" "-t" "linux" "-s" vm-disk vm-name])
      (ok (string "VM '" vm-name "' created"))))

  # write VM config
  (let [conf-path (string vm-dir "/" vm-name "/" vm-name ".conf")
        conf (string
               "loader=\"grub\"\n"
               "cpu=" vm-cpus "\n"
               "memory=" vm-ram "\n"
               "network0_type=\"virtio-net\"\n"
               "network0_switch=\"" vm-switch "\"\n"
               "disk0_type=\"virtio-blk\"\n"
               "disk0_name=\"disk0.img\"\n"
               "graphics=\"no\"\n")]
    (spit conf-path conf)
    (ok (string "wrote " conf-path))))

(defn create []
  (must-be-root)
  (download-iso)
  (create-vm)

  (step "booting Alpine installer")
  (note (string
    "The VM will now boot from the Alpine ISO.\n"
    "Complete the Alpine setup interactively:\n\n"
    "  1. login as 'root' (no password)\n"
    "  2. run: setup-alpine\n"
    "     - hostname: " vm-name "\n"
    "     - networking: eth0, dhcp\n"
    "     - root password: pick something\n"
    "     - timezone: America/Argentina/Buenos_Aires\n"
    "     - disk: sda, sys (use whole disk)\n"
    "  3. after install: poweroff\n"
    "  4. run: janet docker.janet context\n"))

  (sh! ["vm" "install" vm-name iso-path]))

# ── context ──────────────────────────────────────────────────────────────────

(defn get-vm-ip []
  (let [out (sh-out ["vm" "info" vm-name])]
    (if-let [m (peg/match '(* (thru "ip-address") (thru ": ") (capture (some (+ :d ".")))) out)]
      (string/trimr (first m))
      nil)))

(defn inject-ssh-key [ip]
  (step "injecting SSH key")
  (let [key-path (string (os/getenv "HOME") "/.ssh/id_ed25519.pub")]
    (unless (sh-ok? ["test" "-f" key-path])
      (info "no ed25519 key found, generating one...")
      (sh! ["ssh-keygen" "-t" "ed25519" "-N" "" "-f"
            (string (os/getenv "HOME") "/.ssh/id_ed25519")]))
    (sh! ["ssh-copy-id" "-i" key-path (string vm-user "@" ip)])
    (ok "SSH key injected")))

(defn setup-docker-in-vm [ip]
  (step "installing Docker in Alpine VM")
  (let [remote (fn [cmd] (sh! ["ssh" (string vm-user "@" ip) cmd]))]
    (remote "apk update && apk add docker docker-cli-compose")
    (remote "rc-update add docker boot")
    (remote "service docker start")
    # allow docker socket access without sudo
    (remote "chmod 666 /var/run/docker.sock")
    (ok "Docker running in VM")))

(defn create-docker-context [ip]
  (step "creating docker context")
  (sh! ["docker" "context" "create" vm-name
        "--description" "Alpine bhyve VM"
        "--docker" (string "host=ssh://" vm-user "@" ip)])
  (sh! ["docker" "context" "use" vm-name])
  (ok (string "docker context '" vm-name "' set as default")))

(defn context []
  (step "post-install setup")
  (let [ip (get-vm-ip)]
    (when (nil? ip)
      (fail (string "could not get IP for VM '" vm-name "' — is it running? (janet docker.janet start)"))
      (os/exit 1))
    (ok (string "VM IP: " ip))
    (inject-ssh-key ip)
    (setup-docker-in-vm ip)
    (create-docker-context ip)
    (print)
    (ok "done! test with: docker run --rm hello-world")))

# ── day-to-day ───────────────────────────────────────────────────────────────

(defn start []
  (sh! ["vm" "start" vm-name])
  (ok (string "VM '" vm-name "' started")))

(defn stop []
  (sh! ["vm" "stop" vm-name])
  (ok (string "VM '" vm-name "' stopped")))

(defn status []
  (step "VM status")
  (sh! ["vm" "list"])
  (step "docker context")
  (sh! ["docker" "context" "ls"]))

# ── main ─────────────────────────────────────────────────────────────────────

(defn usage []
  (print "\nusage: janet docker.janet <command>\n")
  (each [cmd desc]
        [["preflight" "check VT-x + kernel modules"]
         ["install"   "pkg install vm-bhyve, firmware, docker CLI"]
         ["init"      "vm-bhyve init + virtual switch + rc.conf"]
         ["create"    "download Alpine ISO, create VM, boot installer"]
         ["context"   "after Alpine install: inject SSH key + docker context"]
         ["start"     "start the docker VM"]
         ["stop"      "stop the docker VM"]
         ["status"    "show VM status + docker context"]]
    (printf "  %-12s %s\n" cmd desc))
  (print))

(defn main [_ & args]
  (case (get args 0)
    "preflight" (preflight)
    "install"   (install)
    "init"      (init)
    "create"    (create)
    "context"   (context)
    "start"     (start)
    "stop"      (stop)
    "status"    (status)
    (usage)))
