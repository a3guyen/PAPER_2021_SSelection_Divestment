rm(list=ls())
options(java.parameters="-Xmx6g")
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
setwd("C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/RawData/UNCTAD_FDI_data")
s<-Sys.time() 
#------
# Read 1 excel file
df <- read_excel("ABW.xls", sheet = "inflows")
ncol(df) #13
df <- df%>% filter(!is.na(df[,13]))
df$host <- "ABW"
names(df)[1] <- "source"
names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
df <- df[-1,]
df$series <- "inflows_unctad"
df$datatype <- "mirror"
alldata <- df

# Reading through all excel files:
filelist <- list.files(pattern ="*.xls") 
length(filelist)
file <- "PYF.xls"
# filelist <- filelist[1:3]
for (file in filelist) {
    df<- read_excel(file, sheet = "outflows")
    if (ncol(df)==13) {
        # df <- df%>% filter(!is.na(df[,13]))
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "host"
        df$source<- substr(file, 1,3)
        df$series <- "outflows_un"
        df$datatype <- "mirror"
    }
    else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$host <- paste( df$...4, df$...5 )
        df$host <- trimws(gsub("NA", "", df$host), which = "both")
        # df <- df%>% filter(!is.na(df[,18]), host !="")
        df <-  df[,c(7:19)]
        df$source<- substr(file, 1,3)
        df$series <- "outflows_un"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    alldata <- rbind(alldata, df)
    
    
    df <- read_excel(file, sheet = "outstock")
    if (ncol(df)==13) {
        # df <- df%>% filter(!is.na(df[,13]))
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "host"
        df$source<- substr(file, 1,3)
        df$series <- "outstock_un"
        df$datatype <- "mirror"
    }
    else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$host <- paste( df$...4, df$...5 )
        df$host <- trimws(gsub("NA", "", df$host), which = "both")
        # df <- df%>% filter(!is.na(df[,18]), host !="")
        df <-  df[,c(7:19)]
        df$source<- substr(file, 1,3)
        df$series <- "outstock_un"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    alldata <- rbind(alldata, df)
    
    
    df <- read_excel(file, sheet = "inflows")
    if (ncol(df)==13) {
        # df <- df%>% filter(!is.na(df[,13]))
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "source"
        df$host<- substr(file, 1,3)
        df$series <- "inflows_un"
        df$datatype <- "mirror"
    }
   else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$source <- paste( df$...4, df$...5 )
        df$source <- trimws(gsub("NA", "", df$source), which = "both")
        # df <- df%>% filter(!is.na(df[,18]), source !="")
        df <-  df[,c(7:19)]
        df$host<- substr(file, 1,3)
        df$series <- "inflows_un"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    alldata <- rbind(alldata, df)
    
    instock <- read_excel(file, sheet = "instock")
    if (ncol(df)==13) {
        # df <- df%>% filter(!is.na(df[,13]))
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "source"
        df$host<- substr(file, 1,3)
        df$series <- "instock_un"
        df$datatype <- "mirror"
    }
   else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$source <- paste( df$...4, df$...5 )
        df$source <- trimws(gsub("NA", "", df$source), which = "both")
        # df <- df%>% filter(!is.na(df[,18]), source !="")
        df <-  df[,c(7:19)]
        df$host<- substr(file, 1,3)
        df$series <- "instoc_un"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    alldata <- rbind(alldata, df)
}
alldata <- distinct(alldata) %>% filter(!is.na(source), !is.na(host))

Sys.time() -s