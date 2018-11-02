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

# check packages

pkgs <- c("Cairo","shiny","plyr","dplyr","formattable","tidyr","RColorBrewer",
          "shinythemes","highcharter","rmarkdown","knitr","markdown","DT",
          "ggplot2","shinyBS","RNASeqPower","shinyWidgets")
chk <-  c(!pkgs %in% installed.packages()[,1])
if(any(chk)) stop(paste0("Following package(s) not installed: ",paste0(pkgs[chk],collapse=", "),"."))

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
library(shinyWidgets)

# INITIALISE -------------------------------------------------------------------

# read and prepare instrument info
# dfr_ins <- read.delim("ins.txt",header=T,stringsAsFactors=F)
# dfr_ins$read_type <- ifelse(dfr_ins$read_type==1,"SE","PE")
# dfr_ins$protocol <- paste0(dfr_ins$ins,"_",dfr_ins$read_type,"_",dfr_ins$read_length)
# dfr_ins <- dfr_ins %>% arrange(ins,min_unit,read_type,read_length)
# saveRDS(dfr_ins,"dfr_ins.Rds")
dfr_ins <- readRDS("dfr_ins.Rds")

# remote data
# check if remote read reading works
remote_path <- "https://www.dropbox.com/s/ae2khbyoklyfa5t/ins.txt?dl=1"
r <- tryCatch(
  read.delim(remote_path,header=T,stringsAsFactors=F,nrows=1),
  error=function(er){"error"})

if(is.data.frame(r))
{
  if((ncol(r)==9) && (all(colnames(r) %in% colnames(dfr_ins)))) {
    if(as.Date(r$last_updated[1]) > as.Date(dfr_ins$last_updated[1])) {
      dfr_ins <- read.delim(remote_path,header=T,stringsAsFactors=F)
      dfr_ins$read_type <- ifelse(dfr_ins$read_type==1,"SE","PE")
      dfr_ins$protocol <- paste0(dfr_ins$ins,"_",dfr_ins$read_type,"_",dfr_ins$read_length)
      dfr_ins <- dfr_ins %>% arrange(ins,min_unit,read_type,read_length)
      #saveRDS(dfr_ins,"dfr_ins.Rds")
      message("ins.txt successfully read and saved from remote location.")
    }else{message("ins.txt not read from remote location: Date not changed.")}
  }else{message("ins.txt not read from remote location: Parsing error.")}
}else{message("ins.txt not read from remote location: Link error.")}

#choices_protocol <- split(dfr_ins$protocol,dfr_ins$ins)
#choices_protocol_opts <- split(choices_protocol_opts,dfr_ins$ins)
choices_protocol <- dfr_ins$protocol

# color formatting
col_ins <- RColorBrewer::brewer.pal(12,"Paired")
col_read_type <- RColorBrewer::brewer.pal(8,"Set2")
col_read_length <- RColorBrewer::brewer.pal(8,"Dark2")

if(length(col_ins) < length(levels(factor(dfr_ins$ins)))) warning("Number of colours less than number of instruments.")
if(length(col_read_type) < length(levels(factor(dfr_ins$read_type)))) warning("Number of colours less than number of read types.")
if(length(col_read_length) < length(levels(factor(dfr_ins$read_length)))) warning("Number of colours less than number of read lengths.")

names(col_ins) <- levels(factor(dfr_ins$ins))
names(col_read_type) <- levels(factor(dfr_ins$read_type))
names(col_read_length) <- levels(factor(dfr_ins$read_length))

color_bar_ins1 <- formatter("span",style=function(x) style(
  color="white",font.weight="bold",
  border.radius="4px",padding.left="3px",padding.right="3px",
  background=col_ins[factor(dfr_ins$ins)]))
color_bar_read_type1 <- formatter("span",style=function(x) style(
  color="white",font.weight="bold",
  border.radius="4px",padding.left="3px",padding.right="3px",
  background=col_read_type[factor(dfr_ins$read_type)]))
color_bar_read_length1 <- formatter("span",style=function(x) style(
  color="white",font.weight="bold",
  border.radius="4px",padding.left="3px",padding.right="3px",
  background=col_read_length[factor(dfr_ins$read_length)]))

# dropdown selection choices
choices_protocol_opts <- paste0(color_bar_ins1(dfr_ins$ins),"<span style='padding:3px;'>",
                                color_bar_read_type1(dfr_ins$read_type),"<span style='padding:3px;'>",
                                color_bar_read_length1(dfr_ins$read_length))

# read and prepare organism info
#s <- read.delim("sizes.txt",header=F,stringsAsFactors=F)
#colnames(s) <- c("name","sci_name","dna","cdna","cat1","cat2")
#s$label <- paste0(s$name," (",s$sci_name,")")
#saveRDS(s,"dfr_org.Rds")
dfr_org <- readRDS("dfr_org.Rds")
choices_org <- split(dfr_org$label,factor(dfr_org$cat1))

# FUNCTIONS --------------------------------------------------------------------

fn_version <- function(){list(appversion="v1.0.3",appdate="02-Nov-2018",datadate=dfr_ins$last_updated[1])}
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
