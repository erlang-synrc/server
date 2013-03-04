#!/bin/bash

cd apps/nsp_srv/priv/static/css

git checkout kindex.min.css
git checkout kmatchmaker.min.css
git checkout ktournaments.min.css
git checkout krules_okey.min.css
git checkout kgifts.min.css
git checkout kwall.min.css
git checkout kmembers.min.css
git checkout kgroups.min.css
git checkout kcontact.min.css
git checkout kprofile.min.css
git checkout kinfo.min.css

git pull

./cssfix.sh kindex.min.css
./cssfix.sh kmatchmaker.min.css
./cssfix.sh ktournaments.min.css
./cssfix.sh krules_okey.min.css
./cssfix.sh kgifts.min.css
./cssfix.sh kwall.min.css
./cssfix.sh kmembers.min.css
./cssfix.sh kgroups.min.css
./cssfix.sh kcontact.min.css
./cssfix.sh kprofile.min.css
./cssfix.sh kinfo.min.css
