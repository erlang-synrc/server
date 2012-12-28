#!/bin/bash

rm -rf rels/app/node/lib
rm -rf rels/app/node/data
rm -rf rels/app/node/log
rm -rf rels/app/node/releases
rm -rf rels/game/node/lib
rm -rf rels/game/node/data
rm -rf rels/game/node/log
rm -rf rels/game/node/releases
rm -rf rels/web/node/lib
rm -rf rels/web/node/log
rm -rf rels/web/node/data
rm -rf rels/web/node/releases
rm -rf rels/public/node/lib
rm -rf rels/public/node/log
rm -rf rels/public/node/data
rm -rf rels/public/node/releases

cd rels/app
../../rebar -f generate
cd ../game
../../rebar -f generate
cd ../web
../../rebar -f generate
cd ../public
../../rebar -f generate
cd ../..
