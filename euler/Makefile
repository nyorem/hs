SOURCE_FILES = $(shell ls *.hs)
BUILD_RULES = $(subst .hs,.build,$(SOURCE_FILES))
RUN_RULES = $(subst .hs,.run,$(SOURCE_FILES))
EXECUTABLES = $(subst .hs,,$(SOURCE_FILES))

all: $(BUILD_RULES)

%.build: %.hs
	@stack ghc $<

%.run: %.build
	@echo "Executing $(basename $<)"
	@./$(basename $<)

run: $(RUN_RULES)

clean:
	@rm -fv *.hi *.o

proper: clean
	@rm -fv $(EXECUTABLES)
