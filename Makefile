SCHEME=scheme --libdirs nanopass-framework-scheme

.PHONY: repl
repl:
	$(SCHEME) --script silly-church.scm

.PHONY: scheme
scheme:
	$(SCHEME)
