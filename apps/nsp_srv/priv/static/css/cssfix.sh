#!/bin/bash

host=`hostname -s`
static="url("

if [ ${host}  == "srv1" ]
then
  static="url(\/\/static1.kakaranet.com"
elif [ $host  == "srv2" ]
then
  static="url(\/\/static2.kakaranet.com"
elif [ $host  == "srv3" ]
then
  static="url(\/\/static3.kakaranet.com"
fi

declare -A replacements=(
["values=..10000,01000,00100,00010.."]=" values=\\\'1 0 0 0 0, 0 1 0 0 0, 0 0 1 0 0, 0 0 0 1 0\\\'"
["values=..0.33330.33330.3333000.33330.33330.3333000.33330.33330.33330000010.."]=" values=\\\'0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0 0 0 1 0\\\'"
["svgxmlns"]="svg xmlns"
["filterid"]="filter id"
["feColorMatrixtype"]="feColorMatrix type"
[")}"]=");}"
["hidden}"]="hidden;}"
)
replacements+=(["url("]="${static}")


conditions=$(for key in "${!replacements[@]}"; do echo -n "s/${key}/${replacements[${key}]}/g;"; done)

sed -i "${conditions}" $1
