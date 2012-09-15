#!/bin/bash

rels/app/node/bin/ns_node stop
rels/game/node/bin/ns_node stop
sleep 10
rels/web/node/bin/ns_node stop
