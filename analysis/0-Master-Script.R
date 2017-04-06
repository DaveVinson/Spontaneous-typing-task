
####
#### this script assumes you have applied sheetGenerate.R for each data subfolder
####

#rootFolder = '~/Dropbox/webstuff/plot-summary/analysis/final-to-share/data/amt.replication.mar.2016/' # replication (AMT)
#rootFolder = '~/Dropbox/webstuff/plot-summary/analysis/final-to-share/data/sona.apr.2016/' # second study (SONA / with genre)
rootFolder = '/Users/Dave/Dropbox/Dale/typing/data_scripts/data/sona.dec.2015-dwv/' # first SONA study 
setwd(rootFolder)

library(lme4)
library(stargazer)
library(psych)
library(dplyr)
library(MuMIn) #for pseudo r2
load('keystrokes.RData') #generated for each dataset.
load('digraph_classes.RData')
digraph <-as.factor(c(as.character(oneletter),as.character(onefinger),
                      as.character(onehand),as.character(twohand)))
uniques = read.csv("unique_words.txt",sep="\t",header=T)

source('../../Participant-Details.R') # prints details in console
source('../../Clean-Data-Sheet.R') # prints details in consolec
#this table is older, I hand coded due to some new variables changes. 
#source('../../Table-Sample-Data.R') # stores data sample as .tex in data folder
#run through this independently now. The tables that are generated require some hand coding of variables. 
source('../../Results-Main-Stats.R') # stores coefficients / details as .tex in data folder, as some figs
source('../../contentlenfig.R') #figure 3
source('../../functlenfig.R') #figure 4
#older
#source('../../Figure-1.R') # stores figure as pdf in data folder
#source('../../Figure-2.R') # stores figure as pdf in data folder; does extra stats, too on resampling
