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


interest.alpha <- 0.5
other.alpha <- 0.25
other.color <- "darkgrey"
other.SD <- "Other"

school.districts.of.interest <- local({
  x <- matrix(ncol=4, byrow=T,
              c("MT LEBANON SD",        "MTL",    "blue",      interest.alpha,
                "UPPER SAINT CLAIR SD", "USC",    "red",       interest.alpha,
                "BETHEL PARK SD",       "BP",     "green",     interest.alpha,
                NA,                     other.SD, other.color, other.alpha))
  x <- as.data.frame(x)
  names(x) <- c("District", "SD", "color", "alpha")
  x
})



##=============================================================================
## Prepare the data for analysis
##
## Here I load the data sets and prepare them for my analysis.  I use
## data from the following sources:
##
## PA Dept. of Education 2008-2009 PSSA Results
## http://www.pde.state.pa.us/portal/server.pt/community/school_assessments/7442
## http://www.portal.state.pa.us/portal/server.pt/gateway/PTARGS_0_123031_595946_0_0_18/PSSA_Results_Math_and_Reading_School_2009.xls
## http://www.portal.state.pa.us/portal/server.pt/gateway/PTARGS_0_123031_595965_0_0_18/PSSA_Results_Writing_School_2009.xls
## http://www.portal.state.pa.us/portal/server.pt/gateway/PTARGS_0_123031_595954_0_0_18/PSSA_Results_Science_School_2009.xls
##=============================================================================

## First, I read in the data sets

load.data.set <- function(name) {
  df <- read.csv(name, skip = 3, na.strings = "#NULL!")
  subset(df, Group == "All students")
}


pssa.mr <- load.data.set("data/PSSA_Results_Math_and_Reading_School_2009.csv")
pssa.w  <- load.data.set("data/PSSA_Results_Writing_School_2009.csv")
pssa.s  <- load.data.set("data/PSSA_Results_Science_School_2009.csv")


## Next, I merge the data sets into a single comprehensive data set

pssa <- merge(merge(pssa.mr, pssa.w, all=T), pssa.s, all=T)


## Next, I restrict the data set to all-student data (that is, I
## remove the data that has been partitioned by demographic
## characteristics) and add a new column "SD" to indicate whether a
## particular row of data refers to a school district of interest --
## Mt. Lebanon, Upper St. Clair, or Bethel Park -- and columns to
## indicate the colors and transparency (alpha) levels to use in
## plotting each district.

pssa.all <- local({
  df <- merge(pssa, school.districts.of.interest, all.x=T)
  within(df, {
    SD <- factor(SD, levels = school.districts.of.interest$SD)
    Grade <- factor(Grade, labels = paste("Grade", sort(unique(Grade))))
    others <- is.na(SD)
    color[others] <- other.color
    alpha[others] <- other.alpha
    SD[others] <- other.SD
  })
})


## Next, I extract those columns in which I am interested, mainly the
## scores for advanced levels of performance

pssa.all.renamed <-
  within(pssa.all, {
    Reading <- X..Advanced.Reading; X..Advanced.Reading <- NULL
    Math    <- X..Advanced.Math;    X..Advanced.Math <- NULL
    Writing <- X..Advanced.Writing; X..Advanced.Writing <- NULL
    Science <- X..Advanced.Science; X..Advanced.Science <- NULL
  })

pssa.all.melted <-
  melt(pssa.all.renamed,
       id=c("Grade", "District", "SD", "School"),
       measure=c("Math", "Reading", "Writing", "Science"))

pssa.all.melted.ecdf <-
  ddply(pssa.all.melted, .(Grade, variable), function(df) {
    xs <- na.omit(df$value)
    usxs <- sort(unique(xs))
    usxs.ecdf <- if (length(xs) > 0) ecdf(xs)(usxs) else c()
    data.frame(value = usxs, value.ecdf = 100 * usxs.ecdf)
  })

pssa.interest.melted.ecdf <-
  merge(subset(pssa.all.melted, SD != "Other"), pssa.all.melted.ecdf)

plot.rankings <-
qplot(value, value.ecdf,
      main = paste(sep="\n",
        "How Mt. Lebanon compares to other Pennsylvania schools",
        "Source: 2008-09 PSSA Tests (Advanced Skills)"),
      xlab = "Percentage of students demonstrating advanced skills",
      ylab = "Rank among Pennsylvania schools",
      data = pssa.all.melted.ecdf,
      colour = I(other.color),
      geom = "step",
      binwidth = 1,
      facets = Grade ~ variable
      ) +
  geom_point(aes(colour = SD, alpha = rep(0.5, length(SD))),
             data = pssa.interest.melted.ecdf) +
  geom_text(aes(label = School), data = pssa.interest.melted.ecdf,
            hjust = -0.10, vjust = 0.5, size = 2, angle = -60) +
  scale_colour_manual(name = "School District",
                      values = school.districts.of.interest$color,
                      breaks = school.districts.of.interest$SD) +
  scale_x_continuous(breaks=seq(20, 100, 20)) +
  scale_y_continuous(breaks=seq(20, 100, 20)) +
  scale_alpha(legend = F)

print(plot.rankings)

ggsave(file="mtlsd-2009-pssa-rankings.png",
       plot = plot.rankings,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-rankings.pdf",
       plot = plot.rankings,
       width=8.5, height=11)


mk.performance.histogram <- function(variable) {
  variable.s <- substitute(variable)
  variable.n <- sub(".*\\.", "", deparse(variable.s))
  f <- eval(bquote(function(df) {
    qplot(.(variable.s),
          main = paste(sep="\n",
            "How Mt. Lebanon compares to other Pennsylvania schools",
            paste("Advanced", variable.n, "(2008-09 PSSA)")),
          xlab = paste(
            "Percentage of students demonstrating advanced skills in",
            paste("Advanced", variable.n)),
          ylab = "Count of schools",
          data = df,
          geom = "histogram",
          binwidth = 1,
          fill = SD,
          facets = Grade ~ . ) +
            scale_y_log10() +
      scale_fill_manual(name = "School District",
                        values = school.districts.of.interest$color,
                        breaks = school.districts.of.interest$SD)  }))
  f(df)
}

## rank.on(X..Advanced.Math)



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
        "Percentage of students demonstrating advanced skills",
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
                      breaks = school.districts.of.interest$SD,
                      values = school.districts.of.interest$color)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.png",
       plot = plot.ed.eff,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.pdf",
       plot = plot.ed.eff,
       width=8.5, height=11)
