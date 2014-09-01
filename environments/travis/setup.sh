#!/bin/sh

sudo cp -Rp env /env
sudo cp -Rp ../resources /resources

/resources/provision.sh
