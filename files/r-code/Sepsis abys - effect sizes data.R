library(meta)      # Used for function metagen
library(grid)

setwd("../forest-plots")

#Arm-based data
#DID NOT LOOK AT EFFECTIVE ABYS
#WARNING numbers wobble between original pubications and author contacts by Sterling
dataframe <- read.table(textConnection('
  Study,       Year, PMID,     OR,   CI.l, CI.u,  Size, Outcome,    group, Analysis,      Recommended, style
  "Bloos",     2014, 24589043, 1.23, 0.81, 1.85,  1011, "Mortality", 1,   "Adjusted",      "75%",       "normal"
  "Kumar",     2006, 16625125, 1.67, 1.12, 2.48,  2731, "Mortality", 1,   "Adjusted",     "100%",       "normal"
  "Ferrer",    2009, 19696442, 1.43, 1.14, 1.78,  2796, "Mortality", 1,   "Not adjusted", "100%",       "normal"
  "Puskarich", 2011, 21572327, 1.81, 0.74, 4.44,   291, "Mortality", 1,   "Adjusted",     "100%",       "normal"
  "Galeski",   2011, 20048677, 3.33, 3.45, 7.69,   261, "Mortality", 1,   "Adjusted",     "100%",       "normal"
  "Ferrer-a",  2014, 24717459, 1.07, 0.95, 1.20, 17990, "Mortality", 2,   "Not adjusted", "Unknown %",       "normal"
  "Yokota",    2014, 25375775, 1.13, 0.68, 1.85,  1279, "Mortality", 1,   "Not adjusted", "Unknown %",       "normal"
  "Ryoo",      2015, 25651372, 1.23, 0.69, 2.22,   426, "Mortality", 1,   "Adjusted",     "Unknown %",       "normal"
'), header=TRUE, sep=",",strip.white=TRUE)
# Not usable
#"Whiles",    2017, 28169944, 1.05, 1.03, 1.07,  3929, "Mortality", 1,   "Adjusted",     "Cohort",  "normal"
# Reported in publication
#"Puskarich", 2011, 21572327, 1.81, 0.74, 4.44,   291, "Mortality", 1,   "Adjusted", "Cohort",  "normal"
# Bloos - patients not requiring surgical source control
#"Bloos",     2014, 24589043, 1.45, 0.83, 2.56,  1011, "Mortality", 1,   "Adjusted", "Cohort",  "normal"
# Reported in Sterling
#"Kumar",     2006, 12345678, 7.33, 5.44, 9.97,  2731, "Mortality", 1,   "Not adjusted", "Cohort",  "normal"
#"Puskarich", 2011, 21572327, 0.77, 0.35, 1.68,   291, "Mortality", 1,   "Not adjusted", "Cohort",  "normal"
#"Galeski",   2011, 20048677, 1.65, 0.93, 2.89,   261, "Mortality", 1,   "Not adjusted", "Cohort",  "normal"
#"Bloos",     2014, 24589043, 1.06, 0.74, 1.51,  1011, "Mortality", 1,   "Not adjusted", "Cohort",  "normal"
#"Ryoo",      2015, 25651372, 1.09, 0.64, 1.86,   426, "Mortality", 1,   "Not adjusted", "Cohort",  "normal"
dataframe$Study <- paste(dataframe$Study, ", ",dataframe$Year, sep="")

#Invert ratios
dataframe$OR   <-1/dataframe$OR
dataframe$CI.l <-1/dataframe$CI.l
dataframe$CI.u <-1/dataframe$CI.us

# Sort
dataframe <- dataframe[order(dataframe$Recommended,dataframe$Year),]

#Ln transformations
dataframe$OR.Ln <- log(dataframe$OR)
dataframe$CI.l.Ln <- log(dataframe$CI.l)
dataframe$CI.u.Ln <- log(dataframe$CI.u)

# OR conversion to effect size
# From:
# Chinn, 2000. From http://pubmed.gov/11113947
# Replicating Chinn using her data
effect.size <- log(1.32)/1.81
SE <-(log(5.68) - log(0.31))/2/1.96/1.81
# Now using our data
dataframe$CI.Ln_Width <- dataframe$CI.u.Ln - dataframe$CI.l.Ln
dataframe$SE.Ln <- dataframe$CI.Ln_Width/2/1.96 
dataframe$effect.size <- dataframe$OR.Ln#/1.81
dataframe$effect.size.SE <- dataframe$SE.Ln#/1.81
#SD from SE: http://handbook.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm

# Meta-analysis from library meta
analyticmethod = "Random effects model"
hartung = TRUE
if (hartung){analyticmethod = paste(analyticmethod," (Hartung-Knapp)")}
#meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", backtransf = TRUE, studlab=meta1$Study)

#Timing
meta1 <- dataframe
meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", byvar=Analysis, data=meta1, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=meta1$Study)
# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above
forest(meta1, sort=meta1$Year,print.p=FALSE, xlim=c(0.2,5), xlab="Odds ratio for mortality", label.left="Favors early antibiotics", leftcols=c("studlab","Size"), just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, text.random=analyticmethod,text.random.w=analyticmethod, print.Q.subgroup=FALSE)
grid.text("Antibiotics given within one hour and mortality", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))

#Choice
meta1 <- dataframe
meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", byvar=Recommended, data=meta1, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=meta1$Study)
# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above
forest(meta1, sort=meta1$Year,print.p=FALSE, xlim=c(0.2,5), xlab="Odds ratio for mortality", label.left="Favors antibiotics", leftcols=c("studlab","Size"), just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, text.random=analyticmethod,text.random.w=analyticmethod, print.Q.subgroup=FALSE, label.test.subgroup.random = "Subgroup effect: ")
grid.text("Antibiotic choice and mortality", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
