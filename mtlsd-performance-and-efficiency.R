#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of Performance and Educational Efficiency of
## the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2008-09 PSSA data
##
## Analysis by the Mt. Lebanon Accountability Organization
## Tom Moertel <tom@mlao.org>
## http://www.mlao.org/
##
## 2009-11-01
## Revised 2010-02-20
##
## This analysis is an R program:  http://www.r-project.org/
##=============================================================================
##=============================================================================


options(digits = 2)

require("ggplot2")

##=============================================================================
## Configuration
##=============================================================================


## Graphical preferences

interest.alpha <- 0.5
other.alpha <- 0.25
other.color <- "darkgrey"
other.SD <- "Other"


## Which schools to highlight

school.districts.of.interest <- local({
  x <- matrix(ncol=4, byrow=T,
              c("MT LEBANON SD",        "MTL",    "blue",      interest.alpha,
                "UPPER SAINT CLAIR SD", "USC",    "red",       interest.alpha,
                "BETHEL PARK SD",       "BTHL PK","green",     interest.alpha,
                "NORTH ALLEGHENY SD",   "N ALG",  "brown",     interest.alpha,
                "FOX CHAPEL AREA SD",   "FOX CH", "orange",    interest.alpha,
                "QUAKER VALLEY SD",     "QKR VLY","purple",    interest.alpha,
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

merge.pssa <- function(x, y)
  merge(x, y, all = T, by = c("District", "School", "Grade"))
pssa <- merge.pssa(merge.pssa(pssa.mr, pssa.w), pssa.s)


## Next, I restrict the data set to all-student data (that is, I
## remove the data that has been partitioned by demographic
## characteristics) and add a new column "SD" to indicate whether a
## particular row of data refers to a school district of interest --
## Mt. Lebanon, Upper St. Clair, etc. -- and columns to indicate the
## colors and transparency (alpha) levels to use in plotting each
## district.

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
## scores for advanced levels of performance.

pssa.all.renamed <-
  within(pssa.all, {
    Reading <- X..Advanced.Reading
    Math    <- X..Advanced.Math
    Writing <- X..Advanced.Writing
    Science <- X..Advanced.Science
  })

## ## Use the following scheme instead of the one above to replicate the
## ## district's use of the combiniation of "proficient" and "advanced"
## ## as their benchmark.
##
## pssa.all.renamed <-
##   within(pssa.all, {
##     Reading <- X..Advanced.Reading + X..Proficient.Reading
##     Math    <- X..Advanced.Math    + X..Proficient.Math
##     Writing <- X..Advanced.Writing + X..Proficient.Writing
##     Science <- X..Advanced.Science + X..Proficient.Science
##   })


## Now, I "melt" the data of interest into an indexed data frame,
## ready for use in summary graphics and statistics,

pssa.all.melted <-
  melt(pssa.all.renamed,
       id=c("Grade", "District", "SD", "School"),
       measure=c("Math", "Reading", "Writing", "Science"))


## I reduce the data to its ECDF (empirical cumulative distribution
## function) for the variables of interest.  A variable's ECDF relates
## its values in an absolute sense to their relative rankings.

pssa.all.melted.ecdf <-
  ddply(pssa.all.melted, .(Grade, variable), function(df) {
    xs <- na.omit(df$value)  # ignore schools that didn't participate
    usxs <- sort(unique(xs))
    usxs.ecdf <- if (length(xs) > 0) ecdf(xs)(usxs) else c()
    data.frame(value = usxs, value.ecdf = 100 * usxs.ecdf)
  })


## I now extend the subset of the per-school statistics that are
## "interesting" -- Mt. Lebanon, Upper St. Clair, etc. -- with their
## ECDFs.

pssa.interest.melted.ecdf <-
  merge(subset(pssa.all.melted, SD != "Other"), pssa.all.melted.ecdf)

write.csv(pssa.interest.melted.ecdf,
          file = "pssa-school-rankings-vs-comparables.csv",
          row.names = F)


##=============================================================================
## Summary graphic:  School Performance Rankings
##
## This graphic summarizes the performance of schools in both absolute
## and relative terms, relating the portion of students who tested at
## the advanced level at each school to that school's relative rank
## among all Pennsylvania schools.
## =============================================================================

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
  geom_point(aes(colour = SD), alpha = 0.5, data = pssa.interest.melted.ecdf) +
  geom_text(aes(label = School), data = pssa.interest.melted.ecdf,
            hjust = -0.10, vjust = 0.5, size = 2, angle = -60) +
  scale_colour_manual(name = "School District",
                      values = school.districts.of.interest$color,
                      breaks = school.districts.of.interest$SD) +
  scale_x_continuous(breaks=seq(20, 100, 20)) +
  scale_y_continuous(breaks=seq(20, 100, 20)) +
  scale_alpha(legend = F)

ggsave(file="mtlsd-2009-pssa-rankings.png",
       plot = plot.rankings,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-rankings.pdf",
       plot = plot.rankings,
       width=8.5, height=11)

ggsave(file="mtlsd-2009-pssa-rankings-T.pdf",  # tabloid size
       plot = plot.rankings,
       width=11, height=17)

ggsave(file="mtlsd-2009-pssa-rankings-P.pdf",  # poster size
       plot = plot.rankings,
       width=24, height=36)



##=============================================================================
## Summary graphic:  School Efficiency (Performance vs. Tuition)
##
## I will contrast the performance of each school with its tuition.
## More efficient schools will offer higher performance for lower tuition.
##=============================================================================

## Now I load tuition data from the PA Dept. of Ed:
## http://www.pde.state.pa.us/school_acct/cwp/view.asp?a=182&q=76814#tuitionrates

tuition.rates <- read.csv("data/pa-tuition-rates-2008.csv", skip=5, header=T)


## Now I merge the tuition data into the existing, melted performance data

pssa.all.melted.vs.tuition <-
  merge(pssa.all.melted, tuition.rates, sort=T,
        by.x = c("District"),
        by.y = c("School.District"))
pssa.all.melted.vs.tuition <- within(pssa.all.melted.vs.tuition,
                                     Tuition <- (Elementary + Secondary) / 2)


## And I prepare the plot.

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
  geom_point(alpha = 0.5,
             data = subset(pssa.all.melted.vs.tuition, SD != "Other")) +
  facet_grid(Grade ~ variable, margins=T) +
  scale_x_continuous(breaks=c(25, 50, 75)) +
  scale_y_continuous(formatter="dollar", breaks=c(10000,15000)) +
  scale_colour_manual(name = "School District",
                      breaks = school.districts.of.interest$SD,
                      values = school.districts.of.interest$color) +
  scale_alpha(legend = F)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.png",
       plot = plot.ed.eff,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition.pdf",
       plot = plot.ed.eff,
       width=8.5, height=11)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition-T.pdf",  # tabloid size
       plot = plot.ed.eff,
       width=11, height=17)

ggsave(file="mtlsd-2009-pssa-scores-vs-tuition-P.pdf",  # poster size
       plot = plot.ed.eff,
       width=24, height=36)
