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

test: build
	@./tests/build
	@./tests/run

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
	@echo Bodies with excessive cyclomatic complexity:
	@echo
	@egrep ' body | cyclomatic complexity ' obj/*.metrix | egrep -B1 ' cyclomatic complexity +: +(1[1-9]|[2-9][0-9]|[0-9]{3})' | egrep ' body ' | perl -lpe 's!^obj/!   !; s![.]metrix: *! : !; s![(].+ at lines +([0-9]+): !(lines $$1..!'
	@echo

