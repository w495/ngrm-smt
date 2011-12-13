# default

all:
	$(call make_ebin)

debug:
	(cd src && $(MAKE) debug) || exit 1

clean_all:
	(cd deps/*; 	$(MAKE) clean)
	(cd deps/*/src; $(MAKE) clean)
	(cd src; 	$(MAKE) clean)

clean:
	(cd src; 	$(MAKE) clean)

make_ebin =  \
	(mkdir -p ebin) && \
	(cd deps/* && 		$(MAKE) $(1)) && \
	(cd deps/*/src && 	$(MAKE) $(1)) && \
	(cd src && 		$(MAKE) $(1)) || exit 1
