if [[ $UID != 0 ]]; then
    echo "Please run this script with sudo:"
    echo "sudo $0 $*"
    exit 1
fi

apt-get update
apt-get install -y emacs tmux mosh

# LAMP
apt-get install -y apache2 mysql-server php
