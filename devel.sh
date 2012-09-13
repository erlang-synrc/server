#!/bin/bash
## Start this skript after releases created.
## Then start sync in web node with sync:go().
## Enjoy.

PWD=`pwd`

SRV=$PWD/apps/nsm_srv
GEM=$PWD/apps/nsg_games
SES=$PWD/apps/nsg_session
GS=$PWD/apps/nsg_srv
WEB=$PWD/apps/nsw_srv
DB=$PWD/apps/nsm_db

SRV_REL=rels/app/node/lib/nsm_srv-1
DB_REL=rels/app/node/lib/nsm_db-1
WEB_REL=rels/web/node/lib/nsw_srv-1
GEM_REL=rels/game/node/lib/nsg_games-1
SES_REL=rels/game/node/lib/nsg_session-1
GS_REL=rels/game/node/lib/nsg_srv-1
SRV_REL_IN_WEB=rels/web/node/lib/nsm_srv-1

WEB_REL_PRIV=$WEB_REL/priv
WEB_REL_EBIN=$WEB_REL/ebin
WEB_PRIV=$WEB/priv
WEB_EBIN=$WEB/ebin

SRV_REL_EBIN=$SRV_REL/ebin
SRV_EBIN=$SRV/ebin
SRV_INCLUDE=$SRV/include

DB_REL_EBIN=$DB_REL/ebin
DB_EBIN=$DB/ebin
DB_INCLUDE=$DB/include

GEM_REL_EBIN=$GEM_REL/ebin
GEM_EBIN=$GEM/ebin
GEM_INCLUDE=$GEM/include

SES_REL_EBIN=$SES_REL/ebin
SES_EBIN=$SES/ebin
SES_INCLUDE=$SES/include

GS_REL_EBIN=$GS_REL/ebin
GS_EBIN=$GS/ebin
GS_INCLUDE=$GS/include

rm -rf $WEB_REL_EBIN $WEB_REL_PRIV $SRV_REL_EBIN $SRV_REL_IN_WEB $GEM_REL_EBIN $SES_REL_EBIN $GS_REL_EBIN $DB_REL_EBIN

## WEB node
ln -s $WEB_EBIN $WEB_REL_EBIN
ln -s $WEB_PRIV $WEB_REL_PRIV
## APP node
ln -s $SRV_EBIN $SRV_REL_EBIN
ln -s $DB_EBIN $DB_REL_EBIN
## GAME node
ln -s $GEM_EBIN $GEM_REL_EBIN
ln -s $SES_EBIN $SES_REL_EBIN
ln -s $GS_EBIN $GS_REL_EBIN

## needed for make sync work
mkdir -p $SRV_REL_IN_WEB/ebin
ln -s $SRV/include $SRV_REL_IN_WEB/include

echo "-pa ./lib/nsm_srv-1/ebin" >>  rels/web/node/etc/vm.args