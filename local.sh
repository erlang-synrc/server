#!/bin/bash

LOCAL_IP=${1:-"192.168.1.16"}

./rebar get-deps
./rebar compile
./release.sh
./configure.sh $1

