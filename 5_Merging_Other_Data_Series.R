rm(list=ls())
options(java.parameters="-Xmx6g")
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
path <- "C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/"
paste0(path, "DerivedData")
setwd(paste0(path, "DerivedData"))
#--- Load FDI data
fdi <- read.csv("4_FDIdata.csv")
#---Get pen world table 9.1 data-----------
pwt <- read.xlsx(paste0(path,"RawData/PWT91/pwt91.xlsx"), sheet = "Data")
names(pwt)
length(unique(pwt$countrycode)) #182 
summary(pwt$year) #1950-2017
#---- Get World Development Indicator data -----
wdi <- read.csv(paste0(path,"/RawData/WDI/WDIData.csv"))
wdi <- wdi[,-1]
indicatorlist <-  wdi %>% select(Indicator.Name) %>% distinct()
write.csv(indicatorlist, "5_WDIIndicatorList.csv", na="", row.names = F)






wdi <- wdi %>% filter(Indicator.Name %in% c("Inflation, consumer prices (annual %)", "GDP (current US$)", "GDP (constant 2010 US$)" ))
wdi <- wdi %>% select( - Indicator.Code, -X)
wdi<- reshape(wdi, timevar = "year", times =1960:2020, varying = list(3:63), v.names = "value", idvar = c("Country.Code", "Indicator.Name"),
              direction = "long")
wdi <- as.data.frame(wdi)[,1:4]
str(wdi)
wdi<- reshape(wdi, idvar = c("Country.Code", "year"), timevar = "Indicator.Name", direction = "wide" )
names(wdi)[3:5] <- c("gdp2010usd", "gdpcurrentusd", "cpi")
#---- Merging with FDI----------
df <- merge(fdi, pwt[,c("countrycode","year", "delta")], by.x = c("host", "year"), by.y = c("countrycode", "year"), all.x = T)
colnames(df)[which(names(df)=="delta")] <- 'delta_h'
df <- merge(df, wdi, by.x = c("host", "year"), by.y = c("Country.Code" , "year"), all.x = T)
colnames(df)[which(names(df)=="gdp2010usd")] <- 'gdp2010usd_h'
colnames(df)[which(names(df)=="gdpcurrentusd")] <- 'gdpcurrentusd_h'
colnames(df)[which(names(df)=="cpi")] <- 'cpi_h'



s<-Sys.time() 
