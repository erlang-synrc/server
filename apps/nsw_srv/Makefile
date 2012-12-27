JS_FILES=priv/static/js/kakara.js
CSS_FILES=priv/static/css/kakara.css priv/static/css/kindex.css
ROOT_PATH=priv/static/
#########################################################################
MINIFIED_JS_FILES=$(JS_FILES:.js=.min.js)
COMBINED_JS_FILES=$(JS_FILES:.js=.full.js)
GZIPPED_JS_FILES=$(MINIFIED_JS_FILES:.js=.js.gz)
MINIFIED_CSS_FILES=$(CSS_FILES:.css=.min.css)
COMBINED_CSS_FILES=$(CSS_FILES:.css=.full.css)
GZIPPED_CSS_FILES=$(MINIFIED_CSS_FILES:.css=.css.gz)
# Since JS files are not ready to be checked with jslint, skip it for now
JS_JUICER=-s
CSS_JUICER=

all: packjs fulljs gzipjs packcss fullcss gzipcss

clean: cleanjs cleancss

packjs: $(MINIFIED_JS_FILES)

fulljs: $(COMBINED_JS_FILES)

gzipjs: $(GZIPPED_JS_FILES)

packcss: $(MINIFIED_CSS_FILES)

fullcss: $(COMBINED_CSS_FILES)

gzipcss: $(GZIPPED_CSS_FILES)

cleanjs:
	rm -f $(MINIFIED_JS_FILES) $(COMBINED_JS_FILES) $(JS_FILES:.js=.deps)

cleancss:
	rm -f $(MINIFIED_CSS_FILES) $(COMBINED_CSS_FILES) $(CSS_FILES:.css=.deps)

%.deps : %.js
	juicer list $< | (echo -n "$@ $*.min.js $*.full.js:";  perl -ne 'print $$1 if m/^\s(\s.+)/') > $@

%.deps : %.css
	juicer list $< | (echo -n "$@ $*.min.css $*.full.css:";  perl -ne 'print $$1 if m/^\s(\s.+)/') > $@

%.gz : %
	gzip -9c $< > $@

include $(JS_FILES:.js=.deps)
include $(CSS_FILES:.css=.deps)

%.min.js : %.js
	    juicer merge ${JS_JUICER} -i --force $<

%.full.js : %.js
	    juicer merge ${JS_JUICER} -m none -i --force $< -o $@

%.min.css : %.css
	    juicer merge -b -d ${ROOT_PATH} -c none ${CSS_JUICER} -i --force $<

%.full.css : %.css
	    juicer merge -b -d ${ROOT_PATH} -c none ${CSS_JUICER} -m none -i --force $< -o $@

