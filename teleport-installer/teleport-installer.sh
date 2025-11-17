#!/bin/bash

set -e

TELEPORT_DOMAIN=ssh.42m.work
TELEPORT_VERSION="$(curl https://$TELEPORT_DOMAIN/v1/webapi/automaticupgrades/channel/default/version | sed 's/v//')"

if [[ "Darwin" == $(uname -s) ]] ; then # Mac
	curl -O "https://cdn.teleport.dev/teleport-${TELEPORT_VERSION}.pkg"
	sudo installer -pkg "teleport-${TELEPORT_VERSION}.pkg" -target /
elif [[ -f '/usr/bin/apt' ]] ; then # apt: Ubuntu or debian
	source /etc/os-release

	if [ "$ID" == "pop" ]; then
		ID=ubuntu # debian if is debian
	fi

	echo "ID $ID"

	curl https://apt.releases.teleport.dev/gpg -o /usr/share/keyrings/teleport-archive-keyring.asc

	echo "deb [signed-by=/usr/share/keyrings/teleport-archive-keyring.asc] https://apt.releases.teleport.dev/${ID?} ${VERSION_CODENAME?} stable/${TELEPORT_VERSION?}" | tee /etc/apt/sources.list.d/teleport.list >/dev/null

	apt-get update
else # Linux AMD binary
    if [[ "x86_64" == $(arch) ]] ; then
        ARCH='amd64'
    else
        ARCH='arm64'
    fi
	TELEPORT_PACKAGE="teleport-v$TELEPORT_VERSION-linux-$ARCH-bin.tar.gz"
	TELEPORT_PACKAGE_URI="https://cdn.teleport.dev/$TELEPORT_PACKAGE"

	if [ ! -f "/tmp/$TELEPORT_PACKAGE" ] ;
	then
		echo "Downloading the package"
		cd /tmp
		wget "$TELEPORT_PACKAGE_URI"
		tar xvfz "$TELEPORT_PACKAGE"
	fi

	if [ -f "/tmp/teleport/install" ] ;
	then
		sudo /tmp/teleport/install
		rm -rf /tmp/teleport "/tmp/$TELEPORT_PACKAGE"
	fi
fi
