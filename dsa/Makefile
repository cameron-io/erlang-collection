args := 20 50
scripts := quicksort bubblesort binarysearch

.PHONY: all quicksort bubblesort binarysearch

all: $(scripts)

quicksort: src/quicksort.escript
	escript $< $(args)

bubblesort: src/bubblesort.escript
	escript $< $(args)

binarysearch: src/binarysearch.escript
	escript $< 11 128
