{ sshKeys, ... }:

{
  users.users.wmb.openssh.authorizedKeys.keys = sshKeys;
}
