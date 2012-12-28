#!/bin/sh

msgcomm --more-than=0 gettext.po ../../../../translations/lang/kakaweb/en/gettext.po > ttmp
msguniq -u --no-location --strict ttmp > gettext.po
rm ttmp
