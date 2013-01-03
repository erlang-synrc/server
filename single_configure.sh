#!/bin/bash

LOCAL_IP=${1:-"`hostname -i`"}

if [ "`hostname -f`" == "srv5.kakaranet.com" ]; then
    POOL=5000000
else
    POOL=4000000
fi

cd rels/app/node/etc
./configure -pool $POOL
cd ../../../game/node/etc
./configure -game-host $LOCAL_IP -sync true -pool $POOL
cd ../../../public/node/etc
#./configure -ip $LOCAL_IP -sync false -pool $POOL
#cd ../../../web/node/etc
./configure -ip $LOCAL_IP -sync false -pool $POOL
cd ../../../..
chmod -R o+rX apps
