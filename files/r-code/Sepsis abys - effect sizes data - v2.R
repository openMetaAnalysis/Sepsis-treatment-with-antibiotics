#This file is best used within R Studio
#---------------------------------
version
citation(package = "base", lib.loc = NULL, auto = NULL)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
#windows(600, 600, pointsize = 12) # Size 600x600
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
#---------------------------------
library(stringr)
library(plyr)
library(metafor)
library (meta) # metamean
library(grid)
#library(Rcmdr)
library(boot) #inv.logit
library(dominanceanalysis) # Helps interpret LM by estimating contriutions to R2 from each predictor
library("rqdatatable") # natutal_join (replaces NA values in a dataframe merge)
#---------------------------------
# For troubleshooting
options(error=NULL)
library(tcltk) 
# msgbox for troubleshooting: 
#tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
#browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q
#---------------------------------

# Grab data
file.filter <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
#filename = choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
filename = file.choose()
data<- read.table(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
dataframe <- data
dataframe$Study <- paste(dataframe$Study, ", ",dataframe$Year, sep="")

# Remove any studies?
# dataframe.data   <- dataframe.data[dataframe.cases(clinic.data), ]
dataframe <- dataframe[!dataframe$Population == 'Adequate Abx',]
dataframe <- dataframe[!dataframe$Population == 'All',]
# Choose one of the next two lines if you to assess abys
dataframe <- dataframe[dataframe$Aby.adequate == 'Yes',]
dataframe <- dataframe[!dataframe$Aby.adequate == 'Yes',]

dataframe <- dataframe[!dataframe$OR.type == 'Continuous',]
# Invert ratios?
#dataframe$OR   <-1/dataframe$OR
#dataframe$CI.l <-1/dataframe$CI.l
#dataframe$CI.u <-1/dataframe$CI.u

# Sort
#Dont do this when replicating Sterling
dataframe <- dataframe[order(dataframe$Year),]

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
dataframe$CI.Ln_Width <- abs(dataframe$CI.u.Ln - dataframe$CI.l.Ln)
dataframe$SE.Ln <- dataframe$CI.Ln_Width/2/1.96 
dataframe$effect.size <- dataframe$OR.Ln#/1.81
dataframe$effect.size.SE <- dataframe$SE.Ln#/1.81
#SD from SE: http://handbook.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm

# Meta-analysis from library meta
analyticmethod = "Random effects model"
hartung = FALSE
if (hartung){analyticmethod = paste(analyticmethod," (Hartung-Knapp)")}
meta1 <- metagen(dataframe$effect.size, dataframe$effect.size.SE, sm="OR", byvar=Population, data=dataframe, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=dataframe$Study)
# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above
forest(meta1, sort=meta1$Year, 
       leftcols=c("studlab","Size"), xlab="Odds ratio for mortality",colgap.forest.left = "20mm",
       print.p=FALSE, xlim=c(0.2,5), digits.addcols = 0, just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, text.random=analyticmethod,text.random.w=analyticmethod, print.Q.subgroup=FALSE)
# Pick title line to run
Title <- "Antibiotics given within one hour and mortality"
Title <- "Antibiotics given within one hour and mortality\n(antibiotics deemed 'adequate')"
Footer <- "Notes:"
grid.text(Title, 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text(Footer, 0.05, 0.05, gp=gpar(cex=1),hjust=0)
# Pick filename line to run
plotname = "Forest plot - Sepsis and antibiotics"
plotname = "Forest plot - Sepsis and adequate antibiotics"
rstudioapi::savePlotAsImage(
  paste(plotname," - ",current.date,".png",sep=""),
  format = "png", width = 1000, height = 800)

