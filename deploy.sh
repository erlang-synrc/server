#!/bin/sh

NODE=`hostname -a`

cp prod/$NODE/app.config rels/app/node/etc/app.config
cp prod/$NODE/game.config rels/game/node/etc/app.config
cp prod/$NODE/web.config rels/web/node/etc/app.config

cp prod/$NODE/app.vm.args rels/app/node/etc/vm.args
cp prod/$NODE/game.vm.args rels/game/node/etc/vm.args
cp prod/$NODE/web.vm.args rels/web/node/etc/vm.args

cp prod/$NODE/webmachine.config rels/web/node/etc/webmachine.config
