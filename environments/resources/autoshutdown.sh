#!/bin/sh

set +e     # Don't exit on error

shutdown -c > /dev/null 2>&1
shutdown -H +$1 &
echo "Auto shutdown in $1 minutes"
