#!/bin/bash

LOCAL_IP=${1:-"`hostname -i`"}
APP=${2:-"app2@`hostname -f`"}

cd rels/app/node/etc
./configure -dba nsm_riak -app $APP -game game -web web2 -mq-user guest -mq-pass guest
cd ../../../game/node/etc
./configure -app $APP -game game2 -game-port 9001 -game-host $LOCAL_IP -web web2 -mnesia-init -mq-user guest -mq-pass guest -sync true
cd ../../../web/node/etc
./configure -ip $LOCAL_IP -app $APP -web web2 -web-port 8001 -game game2 -srv $LOCAL_IP -srv-host $LOCAL_IP -srv-port 9001 -mq-user guest -mq-pass guest -fb-app-id 274618369298354 -fb-app-secret c7824599489d4dd24eee251c20174959 -jspack full -csspack full -sync true
cd ../../../..
