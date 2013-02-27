#!/bin/bash

NODE=${1:-"public"}
BIN="rels/$NODE/node/bin/ns_node"

if [ "$NODE" == "all" ]; then
   rels/app/node/bin/ns_node stop
   rels/game/node/bin/ns_node stop
   rels/public/node/bin/ns_node stop
else
   $BIN stop
fi

