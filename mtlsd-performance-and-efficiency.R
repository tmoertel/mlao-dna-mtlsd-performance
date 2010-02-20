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


sd.abbreviations <- c("MTL", "USC", "BP", "Other")
sd.colours <- c("blue", "red", "green", "darkgray")


##=============================================================================
## PERFORMANCE ANALYSIS
##
## I review performance, as measured by 2008-09 PSSA math, reading, writing,
## and science results.
##=============================================================================

## Source:  PA Dept. of Education 2008-2009 PSSA Results
## Source:  http://www.pde.state.pa.us/a_and_t/lib/a_and_t/PSSA_Results_Math_and_Reading_School_2009.xls

pssa.mr <- read.csv("data/PSSA_Results_Math_and_Reading_School_2009.csv",
                    skip = 3,
                    na.strings = "#NULL!")

pssa.w <- read.csv("data/PSSA_Results_Writing_School_2009.csv",
                   skip = 3,
                   na.strings = "#NULL!")

pssa.s <- read.csv("data/PSSA_Results_Science_School_2009.csv",
                   skip = 3,
                   na.strings = "#NULL!")

pssa <- merge(merge(pssa.mr, pssa.w, all=T), pssa.s, all=T)

pssa.all <- subset(pssa, Group == "All students")
pssa.all <-
  transform(pssa.all,
            Grade = factor(Grade, labels=paste("Grade",sort(unique(Grade)))),
            SD = ifelse(District == "MT LEBANON SD",
              "MTL",
              ifelse(District == "UPPER SAINT CLAIR SD",
                     "USC",
                     ifelse(District == "BETHEL PARK SD",
                            "BP",
                            "Other"))))
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

qplot(X..Advanced.Writing,
      main = "How Mt. Lebanon compares in the 2009 PSSA tests: advanced writing",
      xlab = "Percentage of students demonstrating advanced math skills",
      ylab = "Count of schools",
      data = subset(pssa.all, !is.na(X..Advanced.Writing)),
      geom = "histogram",
      binwidth = 1,
      fill = SD,
      facets = Grade ~ . ) +
  scale_fill_manual(name = "School District",
                    value = sd.colours)

qplot(X..Advanced.Science,
      main = "How Mt. Lebanon compares in the 2009 PSSA tests: advanced science",
      xlab = "Percentage of students demonstrating advanced science skills",
      ylab = "Count of schools",
      data = subset(pssa.all, !is.na(X..Advanced.Science)),
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


# Plot advanced-math ECDF
pssa.amath <- sort(pssa.all$X..Advanced.Math)
pssa.amath.ecdf <- ecdf(pssa.amath)(pssa.amath)
df <- data.frame(pssa.amath, pssa.amath.ecdf)
qplot(pssa.amath, pssa.amath.ecdf, data=df, geom="step")


# Rank schools by average percentile

rank.on <- function(var) {
  X <- substitute(var)
  by.rank <- eval(bquote(function(df) {
    with(df, local({
      V.ecdf <- ecdf(.(X))(.(X))
      df2 <- data.frame(Grade = Grade, SD = SD, School = School,
                        Rank = V.ecdf)
      df2 <- subset(df2, SD != "Other")
      df2[order(df2$Rank, decreasing=T),]
    }))
  }))
  df <- eval(bquote(subset(pssa.all, !is.na(.(X)))))
  ddply(df, .(Grade), by.rank)
}


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
    Advanced.Writing <- X..Advanced.Writing; X..Advanced.Writing <- NULL
    Advanced.Science <- X..Advanced.Science; X..Advanced.Science <- NULL
  })

pssa.all.melted <-
  melt(pssa.all.renamed,
       id=c("Grade", "SD", "District", "School"),
       measure=c("Advanced.Math", "Advanced.Reading",
                 "Advanced.Writing", "Advanced.Science"))

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
  geom_point(data=subset(pssa.all.melted.vs.tuition,
                         SD != "Other" & SD != "MTL")) +
  geom_point(data=subset(pssa.all.melted.vs.tuition, SD == "MTL")) +
  facet_grid(Grade ~ variable, margins=T) +
  scale_x_continuous(breaks=c(25, 50, 75)) +
  scale_y_continuous(formatter="dollar", breaks=c(10000,15000)) +
  scale_colour_manual(name = "School District",
                      value = sd.colours)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.png",
       plot = plot.ed.eff,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.pdf",
       plot = plot.ed.eff,
       width=8.5, height=11)
