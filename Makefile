all: code res

code:
	./rebar co

res: web

web:
	cd apps/nsw_srv; ${MAKE}

