rm(list=ls())
options(java.parameters="-Xmx6g")
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
path <- "C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/"

setwd(paste0(path,"RawData/UNCTAD_FDI_data"))
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
dfin <- df

df <- read_excel("ABW.xls", sheet = "outflows")
ncol(df) #13
df <- df%>% filter(!is.na(df[,13]))
df$source <- "ABW"
names(df)[1] <- "host"
names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
df <- df[-1,]
df$series <- "outflows_unctad"
df$datatype <- "mirror"
dfout <- df

# Reading through all excel files:
filelist <- list.files(pattern ="*.xls") 
length(filelist)
file <- "PYF.xls"
# filelist <- filelist[1:3]
for (file in filelist) {
    df<- read_excel(file, sheet = "outflows")
    if (ncol(df)==13) {
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "host"
        df$source<- substr(file, 1,3)
        df$series <- "outflow_unctad"
        df$datatype <- "mirror"
    }
    else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$host <- paste( df$...4, df$...5 )
        df$host <- trimws(gsub("NA", "", df$host), which = "both")
        df <-  df[,c(7:19)]
        df$source<- substr(file, 1,3)
        df$series <- "outflow_unctad"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    dfout <- rbind(dfout, df)
    
    
    df <- read_excel(file, sheet = "outstock")
    if (file =="QAT.xls") {df <- df[,1:18]}
    if (ncol(df)==13) {
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "host"
        df$source<- substr(file, 1,3)
        df$series <- "outstock_unctad"
        df$datatype <- "mirror"
    }
    else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$host <- paste( df$...4, df$...5 )
        df$host <- trimws(gsub("NA", "", df$host), which = "both")
        df <-  df[,c(7:19)]
        df$source<- substr(file, 1,3)
        df$series <- "outstock_unctad"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    dfout <- rbind(dfout, df)
    
    
    df <- read_excel(file, sheet = "inflows")
    if (ncol(df)==13) {
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "source"
        df$host<- substr(file, 1,3)
        df$series <- "inflow_unctad"
        df$datatype <- "mirror"
    }
   else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$source <- paste( df$...4, df$...5 )
        df$source <- trimws(gsub("NA", "", df$source), which = "both")
        df <-  df[,c(7:19)]
        df$host<- substr(file, 1,3)
        df$series <- "inflow_unctad"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    dfin <- rbind(dfin, df)
    
    df <- read_excel(file, sheet = "instock")
    if (ncol(df)==13) {
        names(df)[2:13] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        names(df)[1] <- "source"
        df$host<- substr(file, 1,3)
        df$series <- "instock_unctad"
        df$datatype <- "mirror"
    }
   else if (ncol(df)==18) {
        
        names(df)[7:18] <- c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012")
        df$source <- paste( df$...4, df$...5 )
        df$source <- trimws(gsub("NA", "", df$source), which = "both")
        df <-  df[,c(7:19)]
        df$host<- substr(file, 1,3)
        df$series <- "instock_unctad"
        df$datatype <- "origin"
    } else {
        print(file)
    }
    dfin <- rbind(dfin, df)
}


#Change some iso codes to match world bank and oecd's iso code list
dfin$host <- ifelse(dfin$host == "BIO", "IOT",dfin$host ) #British Indian Ocean Territory
dfout$source <- ifelse(dfout$source == "BIO", "IOT",dfout$source) 
dfin$host <- ifelse(dfin$host == "ROU", "ROM",dfin$host ) #Romania
dfout$source <- ifelse(dfout$source == "ROU", "ROM",dfout$source)
#Clean the final table
dfin <- distinct(dfin) %>% filter(!is.na(source), !is.na(host), source !="", host !="")
dfout <- distinct(dfout) %>% filter(!is.na(source), !is.na(host), source !="", host !="")
dfin <- dfin %>% filter(!grepl("Millions|Table|Reporting|Note|Source", source), !grepl("Millions|Table|Reporting|Note|Source", host))
dfout<- dfout %>% filter(!grepl("Millions|Table|Reporting|Note|Source", host), !grepl("Millions|Table|Reporting|Note|Source", source))
# Getting a full list of country name and iso code to match back to alldata table
str(dfin)
countryname <- dfin %>% select(source) 
names(countryname)[1] <- "Country"
df <- dfout %>% select(host) 
names(df)[1] <- "Country"
countryname <- rbind(countryname,df) %>% distinct() #210 country names


isocode <- dfin %>% select(host) 
names(isocode)[1] <- "iso"
df <- dfout %>% select(source) 
names(df)[1] <- "iso"
isocode <- rbind(isocode,df) %>% distinct() #206 codes
#---------
setwd(paste0(path,"RawData"))
df<- read_excel("Iso3CountryCode.xlsx", sheet = 1)

isocode <-  merge(isocode, df, by.x = "iso", by.y = "Isocode", all.x = T) # This table in is complete since all iso has matching country names
countryname <- merge(countryname, df, by.x = "Country", by.y = "Country", all.x = T) #this one need some manual enter
Manualiso <- countryname %>% filter(is.na(Isocode))
write.csv(Manualiso, "ManualIso.csv")
df <- read_excel("Manualiso-added.xlsx", sheet = 1)
countryname <- merge(countryname, df, by.x = "Country", by.y = "Country", all.x = T)
countryname$Isocode <- ifelse(is.na(countryname$Isocode.x), countryname$Isocode.y, countryname$Isocode.x)
#---------
str(dfin)
names(dfin)[1] <- 'Country'
dfin <- merge(dfin, countryname[,c(1,4)], by.x = 'Country', by.y = 'Country', all.x = T)
colnames(dfin)[which(names(dfin) == "Isocode")] <- "source"
dfin <- dfin %>% select(-Country) 
dfin <- dfin%>% filter(!is.na(source))

str(dfout)
names(dfout)[1] <- 'Country'
dfout <- merge(dfout, countryname[,c(1,4)], by.x = 'Country', by.y = 'Country', all.x = T)
colnames(dfout)[which(names(dfout) == "Isocode")] <- "host"
dfout <- dfout %>% select(-Country) %>% filter(!is.na(host))

alldf <- rbind(dfin, dfout)
length(unique(alldf$source)) #218
length(unique(alldf$host)) #219

# mirror vs origin inconsistent https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Ftwec.12827&file=twec12827-sup-0001-Appendix.docx
#Thus, exclude mirror
alldf <- alldf %>% filter(datatype != "mirror") %>% select(-datatype)
df <- reshape(alldf, timevar = "year", 
              times =c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012"), 
              varying = c("y2001", "y2002", "y2003", "y2004","y2005", "y2006", "y2007", "y2008","y2009", "y2010", "y2011", "y2012"),
              v.names = "value",
              idvar = c("host", "source",  "series"), direction = "long" )
df <- df[,1:5]
df$year <- gsub("y", "", df$year)
df$year <- as.numeric(as.character(df$year))
df$value <- as.numeric(as.character(df$value))
df <- as.data.frame(df)
df <- reshape(df, idvar = c("host", "source",  "year"), timevar = c("series"), direction = "wide")
names(df) <- gsub("value.", "", names(df))
length(unique(df$host)) #200
length(unique(df$source)) #191
unctad <-df

setwd(paste0(path,"DerivedData"))
save(unctad, file='2_UNCTAD.RData')



Sys.time() -s ## 23 secs