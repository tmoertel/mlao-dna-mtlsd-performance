# This a Makefile.  Run "make" from the command line to have the
# GNU Make program read this file and use the rules within it to
# perform the statistical analyses and generate the resulting
# charts and other outputs.
#
# Tom Moertel <tom@mlao.org>
# 2009-11-07


analysis = mtlsd-performance-and-efficiency.R
charts := $(shell bin/find_charts.pl $(analysis))


default: all
.PHONY: default

.PHONY: all
all: $(charts)

$(charts): $(analysis)
	./$(analysis)

.PHONY: clean
clean:
	rm -f $(charts)
