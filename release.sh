#!/bin/bash

rm -rf rels/app/node/lib
rm -rf rels/app/node/log
rm -rf rels/app/node/releases
rm -rf rels/game/node/lib
rm -rf rels/game/node/log
rm -rf rels/game/node/releases
rm -rf rels/web/node/lib
rm -rf rels/web/node/log
rm -rf rels/web/node/releases

cd rels/app
../../rebar -f generate
cd ../game
../../rebar -f generate
cd ../web
../../rebar -f generate
cd ../..
