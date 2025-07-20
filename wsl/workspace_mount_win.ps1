# NOTE: keep the windows and linux path the same so editors don't have to change configs

$ErrorActionPreference = "Stop"

$vhd_path = "E:\workspace.vhdx"
# $win_mount_path = "E:\workspace"
$wsl_mount_path = "/mnt/wsl/workspace"


if (wsl mount | select-string $wsl_mount_path) {
    # mounted in wsl, unmount
    wsl --unmount $vhd_path
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}

# Use COM to simulate explorer.exe right-click -> Mount
$shell = New-Object -ComObject Shell.Application
$folder = $shell.Namespace((Split-Path $vhd_path))
$file = $folder.ParseName((Split-Path $vhd_path -Leaf))

Write-Host "Mounting VHDX via Shell COM..."
$file.InvokeVerb("Mount")
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

Write-Host "Create symlink in WSL..."
# f-it and just try to delete it both ways. one will work
wsl -u root rmdir /mnt/wsl/workspace
wsl -u root rm /mnt/wsl/workspace
wsl -u root ln -s /mnt/w /mnt/wsl/workspace