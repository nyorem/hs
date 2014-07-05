SRC=$(wildcard *.hs)
APPS=$(SRC:%.hs=%.out)
APPS_WITHOUT_EXT=$(SRC:%.hs=%)

all: $(APPS)

%.out: %.hs
	ghc --make $^

.PHONY: clean
clean:
	rm -rf *.hi *.o 2> /dev/null
	rm -rf $(APPS_WITHOUT_EXT) 2> /dev/null
