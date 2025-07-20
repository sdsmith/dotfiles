#
# Mount workspace devdrive to WSL and make it accessible to windows.
#

$ErrorActionPreference = "Stop"

$vhd_path = "E:\workspace.vhdx"
$win_mount_path = "E:\workspace"
$wsl_mount_path = "/mnt/wsl/workspace"

if ((Get-DiskImage -ImagePath "E:\workspace.vhdx" | Select-Object -Property Attached).Attached) {
    # Use COM to simulate explorer.exe right-click -> Eject
    $sshDRIVES_folder_constant = 0x11
    $shell = New-Object -ComObject Shell.Application
    $drive = $shell.NameSpace($sshDRIVES_folder_constant).ParseName("W:")
    $drive.InvokeVerb("Eject")
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}
elseif (wsl mount | select-string $wsl_mount_path) {
    # mounted in wsl, nothing to do
    # TODO: confirm it's in the expected path
    exit 0
}

wsl --mount --vhd $vhd_path --bare
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}
wsl -u root -e sh "/home/sdsmith/.dotfiles/wsl/mount_win_dev_drive_vhdx.sh" "sdsmith"
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

subst W: "\\wsl.localhost\Ubuntu-24.04\mnt\wsl\workspace"