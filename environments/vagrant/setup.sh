#!/bin/sh

VAGRANTSTATUS=$(vagrant status)

# If vagrant is running already, reprovision it
if echo "$VAGRANTSTATUS" | egrep -q "running" ; then
  vagrant provision
else
  vagrant up --provision
fi
