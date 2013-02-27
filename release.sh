#!/bin/bash

NODE=${1:-"public"}

function release_node {
    rm -rf rels/$1/node/lib
    rm -rf rels/$1/node/data
    rm -rf rels/$1/node/log
    rm -rf rels/$1/node/releases
    cd rels/$1
    ../../rebar -f generate
    cd ../..
}

if [ "$NODE" == "all" ]; then
   echo "Releasing all nodes..."
   #release_node app
   #release_node game
   #release_node web
   release_node public
else
   echo "Releasing node $NODE..."
   release_node $NODE
fi

