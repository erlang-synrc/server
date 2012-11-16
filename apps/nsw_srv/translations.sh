#!/bin/sh

mkdir .gettexttmp
INC=""
for i in ../*/include/; do
    INC="-I $i $INC"
done
for i in ../../deps/*/include/; do
    INC="-I $i $INC"
done
INC="-I include/ $INC"

PA=""
for i in ../*/ebin/; do
    PA="-pa $i $PA"
done
for i in ../../deps/*/ebin/; do
    PA="-pa $i $PA"
done
PA="-pa ebin/ $PA"

#rm priv/lang/kakaweb/epot.dets
export GETTEXT_TMP_NAME="kakaweb" GETTEXT_DIR="translations" GETTEXT_DEF_LANG="en"
erlc -o .gettexttmp +gettext $PA $INC src/*.erl src/elements/*.erl src/actions/*.erl
erl $PA -eval "gettext_compile:epot2po(), ok = application:start(sasl), ok = application:start(gettext), gettext:recreate_db()." -noshell

mv translations/lang/kakaweb/en/gettext.po translations/lang/kakaweb/en/gettext.po.iso
sed -e s/charset=iso-8859-1/charset=UTF-8/ translations/lang/kakaweb/en/gettext.po.iso > translations/lang/kakaweb/en/gettext.po
rm -rf .gettexttmp

# cd priv/lang/custom/tr/ ; ./update.sh
#echo "Now you can run 'poedit priv/lang/custom/tr/gettext.po'"
#echo "And then './gettext_refresh.sh'"
