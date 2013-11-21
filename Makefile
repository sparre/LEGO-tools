PROJECT=lego_tools

GENERATED_EXECUTABLES=bin/build_mpd_file \
                      bin/fractal_landscape \
                      bin/pgm_to_ldraw \
                      bin/split_ldraw_file

GENERATED_SOURCES=

EXECUTABLES=$(GENERATED_EXECUTABLES)

all: build metrics

build: fix-whitespace $(GENERATED_SOURCES)
	gnatmake -p -P $(PROJECT)

test:
#test: build
#	@./tests/build
#	@./tests/run

install: build test
	install -t ${HOME}/bin/ $(EXECUTABLES)

clean:
	gnatclean -P $(PROJECT)
	find . -name "*~" -type f -print0 | xargs -0 -r /bin/rm
	rm -f **/*.o **/*.ali
	if [ ! -z "$(GENERATED_SOURCES)" ]; then rm -f $(GENERATED_SOURCES); fi
	rmdir bin || true
	rmdir obj || true

distclean: clean
	rm -f $(GENERATED_EXECUTABLES)
	rmdir bin || true
	rmdir obj || true

fix-whitespace:
	@find src tests -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'

metrics:
	@gnat metric -P $(PROJECT)
	@echo Worst complexity in file: $$(basename $$(grep complexity obj/*metr* | sort -n -t: -k3 | tail -n1 | cut -d: -f1) .metrix)
	@echo
