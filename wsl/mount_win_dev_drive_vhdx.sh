#!/bin/sh

# Run as root. Requires sudo for `mount` and `chown`.

USER=$1
MOUNT_PATH="/mnt/wsl/workspace"

if [ -d ${MOUNT_PATH} ]; then
    rmdir ${MOUNT_PATH}
elif [ -L ${MOUNT_PATH} ]; then
    rm ${MOUNT_PATH}
fi

mkdir -p ${MOUNT_PATH}
mount /dev/disk/by-uuid/2AA70BF947F82552 ${MOUNT_PATH}
chown ${USER}:${USER} ${MOUNT_PATH}
