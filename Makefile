SCHEME=scheme --libdirs nanopass-framework-scheme

.PHONY: repl
repl:
	$(SCHEME) --script repl.scm

.PHONY: scheme
scheme:
	$(SCHEME)
