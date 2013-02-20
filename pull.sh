#!/bin/bash

cd apps/nsp_srv/priv/static/css

git checkout kindex.min.css
git checkout kmatchmaker.min.css
git checkout ktournaments.min.css
git checkout krules_okey.min.css
git checkout kgifts.min.css
git checkout kdashboard.min.css
git checkout kview_user.min.css
git checkout kview_group.min.css
git checkout kview_all_users.min.css
git checkout kview_members.min.css

git pull

./cssfix.sh kindex.min.css
./cssfix.sh kmatchmaker.min.css
./cssfix.sh ktournaments.min.css
./cssfix.sh krules_okey.min.css
./cssfix.sh kgifts.min.css
./cssfix.sh kdashboard.min.css
./cssfix.sh kview_user.min.css
./cssfix.sh kview_group.min.css
./cssfix.sh kview_all_users.min.css
./cssfix.sh kview_members.min.css
