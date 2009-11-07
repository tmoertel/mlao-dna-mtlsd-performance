# This a Makefile.  Run "make" from the command line to have the
# GNU Make program read this file and use the rules within it to
# perform the statistical analyses and generate the resulting
# charts and other outputs.
#
# Tom Moertel <tom@mlao.org>
# 2009-11-07


all_charts = $(mtlsdperf_charts)

# MTLSD performance and educational efficiency
mtlsdperf_analysis = mtlsd-performance-and-efficiency.R
mtlsdperf_charts = mtlsd-2009-pssa-advanced-math-histogram.png \
                   mtlsd-2009-pssa-advanced-reading-histogram.png \
                   mtlsd-2009-pssa-scores-vs-tuition.png \
                   mtlsd-2009-pssa-scores-vs-tuition.pdf



default: all
.PHONY: default

.PHONY: all
all: $(all_charts)

$(mtlsdperf_charts): $(mtlsdperf_analysis)
	./$(mtlsdperf_analysis)

.PHONY: clean
clean:
	rm -f $(all_charts)
