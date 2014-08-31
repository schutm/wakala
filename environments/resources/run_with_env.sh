#!/bin/sh

for file in $1/*
do
    env_var=${file##*/}
    eval "export $env_var=`cat $1/$env_var`"
done

shift
$@
