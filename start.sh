#!/bin/bash

FILES=rels/public/node/lib/nsp_srv-1/priv/static/files
rm -rf $FILES
ln -s /mnt/glusterfs/kakafiles $FILES

rels/app/node/bin/ns_node start
rels/game/node/bin/ns_node start
rels/web/node/bin/ns_node start
rels/public/node/bin/ns_node start

chmod -R o+rX apps

