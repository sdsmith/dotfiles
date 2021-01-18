#!/usr/bin/env bash
set -e

if [ $# -ne 1 ]; then
    echo "Usage: setup_pyre.sh <repository_path>"
    exit 1
fi

# # Prompt for sudo
# [ "$UID" -eq 0 ] || exec sudo bash "$0" "$@"

sudo apt install python3 python3-pip watchman

# Create venv in `env` in current dir
python3 -m venv env

source env/bin/activate
pip3 install wheel
sudo python3 setup.py bdist_wheel # WAR: pyre-check setup failure
pip3 install pyre-check

pyre init
deactivate
