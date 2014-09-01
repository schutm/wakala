#!/bin/sh

sudo cp -Rp env /env
sudo cp -Rp ../resources /resources

/resources/run_with_env.sh /env /resources/provision.sh
