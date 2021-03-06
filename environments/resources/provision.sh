#!/bin/sh

set -e          # Exit on error

echo 'Provisioning Environment'


if which smtp-sink > /dev/null; then
    echo '  smtp-sink is already installed'
else
    echo '  Updating apt-get repositories'
    sudo apt-get -y -qq update > /dev/null
    echo '  Installing postfix'
    sudo DEBIAN_FRONTEND=noninteractive apt-get -y -qq install postfix > /dev/null 2>&1
    echo '  Stopping postfix'
    sudo /etc/init.d/postfix stop > /dev/null
fi

killall -9 smtp-sink > /dev/null 2>&1 || true
HOSTNAME=`hostname -f`
echo '  Starting smtp-sink on '$IP':'$SMTP_PORT' with hostname '$HOSTNAME
smtp-sink -h $HOSTNAME $IP:$SMTP_PORT 5000 &


echo 'Environment has been provisioned.'
