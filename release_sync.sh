#!/bin/bash

# sync release awareness by maxim@synrc.com

wd=`pwd`

declare -A sources
declare -A apps

for src in `ls -d apps/*`; do 
    app=(`echo $src | tr '/' ' '`)
    sources[${app[1]}]="1"
done

for dir in `ls -d rels/*/node/lib/ns?_*`; do
    lib=(`echo $dir | tr '-' ' ' | tr '/' ' '`)
    ap=${lib[4]}
    if [ "${sources[$ap]}" = "1" ]; then
        apps[$dir]="apps/$ap"
    fi
done

for key in ${!apps[@]}; do 
    echo "release sync processing $key -> ${apps[$key]}";
    rm -rf "$key/ebin"
    rm -rf "$key/priv"
    if [ -d "${apps[$key]}/ebin" ]; then
         ln -s  "$wd/${apps[$key]}/ebin" "$wd/$key/ebin"
    fi
    if [ -d "${apps[$key]}/priv" ]; then
         ln -s  "$wd/${apps[$key]}/priv" "$wd/$key/priv"
    fi
done
