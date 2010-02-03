#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of Performance and Educational Efficiency of
## the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2008-09 PSSA data
##
## Analyis by Tom Moertel <tom@mlao.org>.
## 2009-11-01
##
## This analysis is an R program:  http://www.r-project.org/
##=============================================================================
##=============================================================================


options(digits = 2)


require("ggplot2")


sd.abbreviations <- c("MTL", "USC", "Other")
sd.colours <- c("blue", "red", "darkgray")


##=============================================================================
## PERFORMANCE ANALYSIS
##
## I review performance, as measured by 2008-09 PSSA reading and math results.
##=============================================================================

## Source:  PA Dept. of Education 2008-2009 PSSA Results
## Source:  http://www.pde.state.pa.us/a_and_t/lib/a_and_t/PSSA_Results_Math_and_Reading_School_2009.xls

pssa <- read.csv("data/PSSA_Results_Math_and_Reading_School_2009.csv",
                 skip = 3,
                 na.strings = "#NULL!")

pssa.all <- subset(pssa, Group == "All students")
pssa.all <-
  transform(pssa.all,
            Grade = factor(Grade, labels=paste("Grade",sort(unique(Grade)))),
            SD = ifelse(District == "MT LEBANON SD",
              "MTL",
              ifelse(District == "UPPER SAINT CLAIR SD",
                     "USC",
                     "Other")))
pssa.all <- within(pssa.all, {
  SD <- factor(SD, levels = sd.abbreviations)
})

qplot(X..Advanced.Math,
      main = "How Mt. Lebanon compares in the 2009 PSSA tests: advanced math",
      xlab = "Percentage of students demonstrating advanced math skills",
      ylab = "Count of schools",
      data = pssa.all,
      geom = "histogram",
      binwidth = 1,
      fill = SD,
      facets = Grade ~ . ) +
  scale_fill_manual(name = "School District",
                    value = sd.colours)

ggsave(file="mtlsd-2009-pssa-advanced-math-histogram.png",
       width=8, height=10, dpi=100)



qplot(X..Advanced.Reading,
      main = "How Mt. Lebanon compares in the 2009 PSSA tests: advanced reading",
      xlab = "Percentage of students demonstrating advanced reading skills",
      ylab = "Count of schools",
      data = pssa.all,
      geom = "histogram",
      binwidth = 1,
      fill = SD,
      facets = Grade ~ . ) +
  scale_fill_manual(name = "School District",
                    value = sd.colours)

ggsave(file="mtlsd-2009-pssa-advanced-reading-histogram.png",
       width=8, height=10, dpi=100)


pssa.amath <- sort(pssa.all$X..Advanced.Math)
pssa.amath.ecdf <- ecdf(pssa.amath)(pssa.amath)
df <- data.frame(pssa.amath, pssa.amath.ecdf)
qplot(pssa.amath, pssa.amath.ecdf, data=df, geom="step")

by.rank <- function(pssa.all) {
  pssa.rankings <-
    with(subset(pssa.all, SD != "Other"),
         data.frame(Grade = Grade, SD = SD, School = School,
                    Avg.Rank = (adv.math.ecdf(X..Advanced.Math) +
                                adv.math.ecdf(X..Advanced.Reading)) / 2,
                    Adv.Math.Rank = adv.math.ecdf(X..Advanced.Math),
                    Adv.Reading.Rank = adv.reading.ecdf(X..Advanced.Reading)
                    ))
  avg.ranking.order <-
    local({
      x <- pssa.rankings
      order(x$Grade, - x$Avg.Rank)
    })
  pssa.rankings <- pssa.rankings[avg.ranking.order,]
  pssa.rankings
}


by.rank <- function(df) {
  with(df, local({
    M.ecdf <- ecdf(X..Advanced.Math)(X..Advanced.Math)
    R.ecdf <- ecdf(X..Advanced.Math)(X..Advanced.Reading)
    df2 <- data.frame(Grade = Grade, SD = SD, School = School,
                      Avg.Rank = (M.ecdf + R.ecdf) / 2,
                      Adv.Math.Rank = M.ecdf,
                      Adv.Reading.Rank = R.ecdf)
    subset(df2, SD != "Other")
  }))
}
dlply(pssa.all, .(Grade), by.rank)



pssa.all.x <-
  ddply(pssa.all, "Grade", function(df) {
    df$math.ecdf <- ecdf(df$X..Advanced.Math)(df$X..Advanced.Math)
    df$read.ecdf <- ecdf(df$X..Advanced.Reading)(df$X..Advanced.Reading)
    df
  })
pssa.all.x.interest <-subset(pssa.all.x, SD != "Other")

local({
  d <- ddply(pssa.all.x, .(Grade), function(df) {
    xs <- df$X..Advanced.Math
    usxs <- sort(unique(xs))
    data.frame(X..Advanced.Math = usxs, math.ecdf = ecdf(xs)(usxs))
  })
  ggplot(aes(X..Advanced.Math, math.ecdf), data = pssa.all.x.interest) +
    facet_grid(Grade ~ .) +
    geom_step(data = d) +
    geom_point(aes(colour = SD), data = pssa.all.x.interest) +
    scale_colour_manual(name = "School District",
                        value = sd.colours[-length(sd.colours)]) +
    geom_text(aes(label = School),
              hjust = -0.10, vjust = 0.5, size = 2, angle = -60)
})


local({
  d <- ddply(pssa.all.x, .(Grade), function(df) {
    xs <- df$X..Advanced.Reading
    usxs <- sort(unique(xs))
    data.frame(X..Advanced.Reading = usxs, read.ecdf = ecdf(xs)(usxs))
  })
  ggplot(aes(X..Advanced.Reading, read.ecdf), data = pssa.all.x.interest) +
    facet_grid(Grade ~ .) +
    geom_step(data = d) +
    geom_point(aes(colour = SD), data = pssa.all.x.interest) +
    scale_colour_manual(name = "School District",
                        value = sd.colours[-length(sd.colours)]) +
    geom_text(aes(label = School),
              hjust = -0.10, vjust = 0.5, size = 2, angle = -60)
})


##=============================================================================
## EFFICIENCY ANALYSIS
##
## Now I load tuition data from the PA Dept. of Ed:
## http://www.pde.state.pa.us/school_acct/cwp/view.asp?a=182&q=76814#tuitionrates
##
## I will contrast the performance of each school with its tuition.
## More efficient schools will offer higher performance for lower tuition.
##=============================================================================

tuition.rates <- read.csv("data/pa-tuition-rates-2008.csv", skip=5, header=T)


pssa.vs.tuition <-
  merge(pssa.all, tuition.rates, sort=T,
        by.x = c("District"),
        by.y = c("School.District"))
pssa.vs.tuition <- within(pssa.vs.tuition,
                          Tuition <- (Elementary + Secondary) / 2)


pssa.all.renamed <-
  within(pssa.all, {
    Advanced.Reading <- X..Advanced.Reading; X..Advanced.Reading <- NULL
    Advanced.Math <- X..Advanced.Math; X..Advanced.Math <- NULL
    Proficient.Reading <- X..Proficient.Reading; X..Proficient.Reading <- NULL
    Proficient.Math <- X..Proficient.Math; X..Proficient.Math <- NULL
  })

pssa.all.melted <-
  melt(pssa.all.renamed,
       id=c("Grade", "SD", "District", "School"),
       measure=c("Advanced.Math", "Advanced.Reading",
         "Proficient.Math", "Proficient.Reading"))

pssa.all.melted.vs.tuition <-
  merge(pssa.all.melted, tuition.rates, sort=T,
        by.x = c("District"),
        by.y = c("School.District"))
pssa.all.melted.vs.tuition <- within(pssa.all.melted.vs.tuition,
                                     Tuition <- (Elementary + Secondary) / 2)

plot.ed.eff <-
qplot(value, Tuition, data=pssa.all.melted.vs.tuition,
      main = paste(sep="\n",
        "Mt. Lebanon School District Educational Efficiency",
        "Performance vs. Tuition"),
      xlab =  paste(sep="\n",
        "Percentage of students testing at the given level (higher is better)",
        "Source: 2008-2009 PSSA results"),
      ylab = "Tuition (lower is better)",
      alpha = I(0.25),
      colour = SD) +
  geom_point(data=subset(pssa.all.melted.vs.tuition, SD != "Other")) +
  facet_grid(Grade ~ variable, margins=T) +
  scale_y_continuous(formatter="dollar") +
  scale_colour_manual(name = "School District",
                      value = sd.colours)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.png",
       plot = plot.ed.eff,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.pdf",
       plot = plot.ed.eff,
       width=8.5, height=11)
