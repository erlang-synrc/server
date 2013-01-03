#!/bin/bash

function release_node {
    rm -rf rels/$1/node/lib
    rm -rf rels/$1/node/data
    rm -rf rels/$1/node/log
    rm -rf rels/$1/node/releases
    cd rels/$1
    ../../rebar -f generate
    cd ../..
}

release_node app
release_node game
release_node public
#release_node web
