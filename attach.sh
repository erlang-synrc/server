#!/bin/sh

NODE=${1:-"web"}
BIN="rels/$NODE/node/bin/ns_node"

$BIN attach

