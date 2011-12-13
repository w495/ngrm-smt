# default

release:
	$(call make_ebin)

debug:
	(cd src && $(MAKE) debug) || exit 1

clean:
	(cd src;$(MAKE) clean)

make_ebin =  \
	(mkdir -p ebin) && \
	(cd src && $(MAKE) $(1)) || exit 1
