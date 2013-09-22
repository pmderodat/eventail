GPRBUILD=gprbuild
GPRCLEAN=gprclean

# Binary program name
PGMNAME=eventail
PGMPATH=bin/$(PGMNAME)

PARALLEL_BUILD=0
BUILD_MODE=dev

all: $(PGMPATH)

.PHONY: $(PGMPATH) clean distclean

clean:
	$(GPRCLEAN) -Peventail -c

distclean:
	$(GPRCLEAN) -Peventail

$(PGMPATH):
	$(GPRBUILD) -Peventail \
	    -p -j$(PARALLEL_BUILD) \
	    -XBUILD_MODE=$(BUILD_MODE) \
	    $(EXTRA_ADAFLAGS) \
	    $(PGMNAME)
