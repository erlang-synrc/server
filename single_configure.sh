#!/bin/bash

LOCAL_IP=${1:-"`hostname -i`"}
APP=${2:-"app@`hostname -f`"}

cd rels/app/node/etc
./configure -dba nsm_riak -app $APP -game game -web web -mq-user guest -mq-pass guest
cd ../../../game/node/etc
./configure -app $APP -game game -game-port 9000 -game-host $LOCAL_IP -web web -mnesia-init -mq-user guest -mq-pass guest -sync true
cd ../../../web/node/etc
./configure -ip $LOCAL_IP -app $APP -web web -web-port 8000 -game game -srv $LOCAL_IP -srv-host $LOCAL_IP -srv-port 9000 -mq-user guest -mq-pass guest -fb-app-id 154227314626053 -fb-app-secret cf9d49958ee536dd75f15bf8ca541965 -jspack full -csspack full -sync true
cd ../../../..
