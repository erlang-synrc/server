#!/bin/bash

./rebar get-deps
./rebar compile
./release.sh
./configure.sh $1 $2

