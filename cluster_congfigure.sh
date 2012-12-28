#!/bin/sh

NODE=`hostname -a`

cp prod/$NODE/app.config rels/app/node/etc/app.config
cp prod/$NODE/game.config rels/game/node/etc/app.config
cp prod/$NODE/web.config rels/web/node/etc/app.config
cp prod/$NODE/public.config rels/public/node/etc/app.config

cp prod/$NODE/app.vm.args rels/app/node/etc/vm.args
cp prod/$NODE/game.vm.args rels/game/node/etc/vm.args
cp prod/$NODE/web.vm.args rels/web/node/etc/vm.args
cp prod/$NODE/public.vm.args rels/public/node/etc/vm.args

cp prod/$NODE/webmachine.config rels/public/node/etc/webmachine.config

cp prod/$NODE/app.snmp.agent.conf rels/app/node/etc/snmp/agent/agent.conf
cp prod/$NODE/app.snmp.standard.conf rels/app/node/etc/snmp/agent/standard.conf
cp prod/$NODE/app.snmp.usm.conf rels/app/node/etc/snmp/agent/usm.conf

cp prod/$NODE/game.snmp.agent.conf rels/game/node/etc/snmp/agent/agent.conf
cp prod/$NODE/game.snmp.standard.conf rels/game/node/etc/snmp/agent/standard.conf
cp prod/$NODE/game.snmp.usm.conf rels/game/node/etc/snmp/agent/usm.conf

cp prod/$NODE/web.snmp.agent.conf rels/web/node/etc/snmp/agent/agent.conf
cp prod/$NODE/web.snmp.standard.conf rels/web/node/etc/snmp/agent/standard.conf
cp prod/$NODE/web.snmp.usm.conf rels/web/node/etc/snmp/agent/usm.conf

cp prod/$NODE/public.snmp.agent.conf rels/public/node/etc/snmp/agent/agent.conf
cp prod/$NODE/public.snmp.standard.conf rels/public/node/etc/snmp/agent/standard.conf
cp prod/$NODE/public.snmp.usm.conf rels/public/node/etc/snmp/agent/usm.conf

FILES=rels/public/node/lib/nsp_srv-1/priv/static/files
rm -rf $FILES
ln -s /mnt/glusterfs/kakafiles $FILES
