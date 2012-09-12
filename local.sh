#!/bin/bash

LOCAL_IP=${1:-"192.168.0.17"}

./rebar get-deps
./rebar compile
./release.sh
./configure.sh $1 app@srv5.kakaranet.com

