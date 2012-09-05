#!/bin/bash

LOCAL_IP=${1:-"192.168.1.16"}

./rebar compile
cd rels/app
../../rebar -f generate
cd node/etc
./configure -dba zealot_riak -app app -game game -web web -mq-user guest -mq-pass guest
cd ../..
cd ../game
../../rebar -f generate
cd node/etc
./configure -game game -game-port 9000 -web web -mnesia-init
cd ../..
cd ../web
../../rebar -f generate
cd node/etc
./configure -ip $LOCAL_IP -app app -web web -web-port 8000 -game game -srv $LOCAL_IP -srv-port 9000 -mq-user guest -mq-pass guest -fb-app-id 274618369298354 -fb-app-secret c7824599489d4dd24eee251c20174959 -jspack full -csspack full
cd ../../../..
