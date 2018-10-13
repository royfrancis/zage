# ZAGE SHINYAPP
# 2018 Roy Mathew Francis
# FUNCTIONS.R

# for custom font
# dir.create('r-lib')
# download.file('https://cran.r-project.org/src/contrib/extrafontdb_1.0.tar.gz','r-lib/extrafontdb_1.0.tar.gz')
# .libPaths(c('r-lib',.libPaths()))
# install.packages('r-lib/extrafontdb_1.0.tar.gz',type='source',repos=NULL)
# library(extrafont)
# font_import(pattern="Lato",,prompt=FALSE)
# loadfonts()

library(Cairo)
library(shiny)
library(plyr)
library(dplyr)
#library(kableExtra)
library(formattable)
library(tidyr)
library(RColorBrewer)
library(shinythemes)
library(highcharter)
library(rmarkdown)
library(knitr)
library(markdown)
library(DT)
library(ggplot2)
library(shinyBS)
library(RNASeqPower)

# INITIALISE -------------------------------------------------------------------

# read and prepare instrument info
# dfr_ins <- read.delim("ins.txt",header=T,stringsAsFactors=F)
# dfr_ins$read_type <- ifelse(dfr_ins$read_type==1,"SE","PE")
# dfr_ins$protocol <- paste0(dfr_ins$ins,"_",dfr_ins$read_type,"_",dfr_ins$read_length)
# saveRDS(dfr_ins,"dfr_ins.Rds")
dfr_ins <- readRDS("dfr_ins.Rds")
choices_protocol <- sort(unique(dfr_ins$protocol))

# read and prepare organism info
#s <- read.delim("sizes.txt",header=F,stringsAsFactors=F)
#colnames(s) <- c("name","sci_name","dna","cdna","cat1","cat2")
#s$label <- paste0(s$name," (",s$sci_name,")")
#saveRDS(s,"dfr_org.Rds")
dfr_org <- readRDS("dfr_org.Rds")
choices_org <- split(dfr_org$label,factor(dfr_org$cat1))

fn_version <- function(){list(appversion="v1.0.2",appdate="13-Oct-2018",datadate=dfr_ins$last_updated[1])}
fnv <- fn_version()

dfr_ins$last_updated <- NULL

choices_pa <- list(`Number of samples in each group`=c("n"="n"),
                   `Biological coefficient of variation`=c("cv"="cv"),
                   `Relative expression effect (like fold-change)`=c("effect"="effect"),
                   `False positive rate (like p-value)`=c("alpha"="alpha"),
                   `Fraction of true positives`=c("power"="power"))

# Validate equality between two input values
# Prints message if input1 is not equal to input2
#
fn_validate_equal <- function(input1,input2,message)
{
  if(all(input1 != input2)) print(message)
}
