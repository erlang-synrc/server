#!/bin/bash

cd rels/app
../../rebar -f generate
cd ../game
../../rebar -f generate
cd ../web
../../rebar -f generate
cd ../..
