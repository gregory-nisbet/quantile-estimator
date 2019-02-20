SOURCES = quantile.ml quantile.mli test_quantile.ml
PACKS = oUnit
RESULT = test_quantile

test: native-code

check: test

-include OCamlMakefile
