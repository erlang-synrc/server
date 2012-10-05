#!/bin/bash

LOCAL_IP=${1:-"127.0.0.1"}
APP=${2:-"app@synrc.com"}

cd rels/app/node/etc
./configure -dba nsm_riak -app $APP -game game -web web -mq-user guest -mq-pass guest 
cd ../../../game/node/etc
./configure -app $APP -game game -game-port 9000 -game-host $LOCAL_IP -web web -mnesia-init -mq-user guest -mq-pass guest
cd ../../../web/node/etc
./configure -ip $LOCAL_IP -app $APP -web web -web-port 8000 -game game -srv $LOCAL_IP -srv-host $LOCAL_IP -srv-port 9000 -mq-user guest -mq-pass guest -fb-app-id 274618369298354 -fb-app-secret c7824599489d4dd24eee251c20174959 -jspack full -csspack full
cd ../../../..

