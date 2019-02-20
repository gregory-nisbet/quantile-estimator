all : all.stamp

test : test.stamp
	./test_quantile

check: test

all.stamp: ./quantile.ml ./quantile.mli ./test_quantile.ml
	$(RM) all.stamp
	$(MAKE) -f quantile.mk all
	touch all.stamp

test.stamp: ./quantile.ml ./quantile.mli ./test_quantile.ml 
	$(RM) test.stamp
	$(MAKE) -f test.mk test
	touch test.stamp

clean:
	$(MAKE) -f quantile.mk clean
	$(MAKE) -f test.mk clean
	$(RM) *.stamp
