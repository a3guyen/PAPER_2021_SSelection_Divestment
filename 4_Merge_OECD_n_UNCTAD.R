rm(list=ls())
options(java.parameters="-Xmx6g")
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
path <- "C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/"

setwd(paste0(path, "DerivedData"))
s<-Sys.time() 
load("2_UNCTAD.RData")
load("3_OECD.RData")
str(oecd)
str(unctad)
unctad <- unctad[,1:7]
df <- merge(unctad, oecd, by.x = c("host", "source", "year"), by.y = c("host", "source", "Year"), all = T)

all <- df

# The data is from 1985 to 2018: oecd3 (1985-2013), oecd4 (2005-2018), unctad(2001-2012) => Let get the data for 2001 to 2018 only
# We have 6 series for flows, 6 series for stock. Choosing by this priority order: 
# Inward is preferred to outward, more observations is preferred, oecd4 is preferred to oecd3 which is preferred to unctad

#============================= Getting data without perpertuary inventory method =======================================
df$pair <- paste0(df$host, df$source)
names(df)
# Get flow series first
df$inflow <- df$inflow_oecd4
df <- df %>% mutate(inflow = ifelse(is.na(inflow), inflow_oecd3, inflow))
df <- df %>% mutate(inflow = ifelse(is.na(inflow), inflow_unctad, inflow))
df$outflow <- df$outflow_oecd4
df <- df %>% mutate(outflow = ifelse(is.na(outflow), outflow_oecd3, outflow))
df <- df %>% mutate(outflow = ifelse(is.na(outflow), outflow_unctad, outflow))
df <- df %>% group_by(pair) %>% mutate(countin = sum((is.na(inflow)|inflow==0) & year >2000)) 
df <- df %>% group_by(pair) %>% mutate(countout = sum((is.na(outflow)|outflow==0)& year >2000))
df$flownocal <- df$inflow
df <- df %>% mutate(flownocal = ifelse(countin > countout, outflow, flownocal))

# Get stock series 
df$instock <- df$instock_oecd4
df <- df %>% mutate(instock = ifelse(is.na(instock), instock_oecd3, instock))
df <- df %>% mutate(instock = ifelse(is.na(instock), instock_unctad, instock))
df$outstock <- df$outstock_oecd4
df <- df %>% mutate(outstock = ifelse(is.na(outstock), outstock_oecd3, outstock))
df <- df %>% mutate(outstock = ifelse(is.na(outstock), outstock_unctad, outstock))
df <- df %>% group_by(pair) %>% mutate(countin = sum((is.na(instock)|instock==0)& year >2000))
df <- df %>% group_by(pair) %>% mutate(countout = sum((is.na(outstock)|outstock==0)& year >2000))
df$stocknocal <- df$instock
df <- df %>% mutate(stocknocal = ifelse(countin > countout, outstock, stocknocal))
allfdi <- df
# Filter
# df <- df %>% group_by(pair) %>% mutate(countflow = sum((is.na(flow)|flow==0) & year >2000))
# df <- df %>% group_by(pair) %>% mutate(countstock = sum((is.na(stock)|stock==0)& year >2000))
# df <- df %>% filter(!(countstock ==18 & countflow==18)) # Remove all pairs that have only NA and missing values over the period 2001-2018
length(unique(df$host)) #243
length(unique(df$source)) #243


#============================= Getting data with perpertuary inventory method =======================================
#---Get pen world table 9.1 data-----------
pwt <- read.xlsx(paste0(path,"RawData/PWT91/pwt91.xlsx"), sheet = "Data")
names(pwt)
length(unique(pwt$countrycode)) #182 
summary(pwt$year) #1950-2017
#---- Get World Development Indicator data -----
wdi <- read.csv(paste0(path,"/RawData/WDI/WDIData.csv"))
length(unique(wdi$Indicator.Name)) #1440 data series
wdi <- wdi[,-1]
wdi <- wdi %>% filter(Indicator.Name %in% c("Inflation, consumer prices (annual %)", "GDP (current US$)", "GDP (constant 2010 US$)" ))
wdi <- wdi %>% select( - Indicator.Code, -X)
wdi<- reshape(wdi, timevar = "year", times =1960:2020, varying = list(3:63), v.names = "value", idvar = c("Country.Code", "Indicator.Name"),
               direction = "long")
wdi <- as.data.frame(wdi)[,1:4]
str(wdi)
wdi<- reshape(wdi, idvar = c("Country.Code", "year"), timevar = "Indicator.Name", direction = "wide" )
names(wdi)[3:5] <- c("gdp2010usd", "gdpcurrentusd", "cpi")
#---- Merging with FDI----------
df <- allfdi %>% select(-contains("oecd"))
df <- df %>% select(-contains("unctad"))
df <- df %>% select(-contains("count"))
df <- merge(df, pwt[,c("countrycode","year", "delta")], by.x = c("host", "year"), by.y = c("countrycode", "year"), all.x = T)
colnames(df)[which(names(df)=="delta")] <- 'delta_h'
df <- merge(df, wdi, by.x = c("host", "year"), by.y = c("Country.Code" , "year"), all.x = T)
colnames(df)[which(names(df)=="gdp2010usd")] <- 'gdp2010usd_h'
colnames(df)[which(names(df)=="gdpcurrentusd")] <- 'gdpcurrentusd_h'
colnames(df)[which(names(df)=="cpi")] <- 'cpi_h'
#-----Calculating perpertuary inventory methods - FDI -------
df <- df %>% arrange(pair, year )
df <- df %>% mutate(deflator = gdp2010usd_h/gdpcurrentusd_h)
df <- df %>% mutate(rinflow = inflow*deflator, routflow = outflow*deflator, rinstock = instock*deflator,routstock = outstock*deflator, delta1=1-delta_h)
df <- df %>% group_by(pair) %>% mutate(laginstock = lag(rinstock, order_by = year), lagoutstock = lag(routstock, order_by = year))
sum(!is.na(df$rinflow)) #157700 => 176770  12% estimated
sum(!is.na(df$rinstock)) #151520 => 154235 2%
sum(!is.na(df$routflow)) #112270 => 128269 14%
sum(!is.na(df$routstock)) #117126 => 118662 1%
names(df)
df <- df %>% mutate(rinstock = ifelse(is.na(rinstock), rinflow+delta1*laginstock, rinstock), 
                    routstock = ifelse(is.na(routstock), routflow+delta1*lagoutstock, routstock),
                    rinflow = ifelse(is.na(rinflow), rinstock - delta1*laginstock, rinflow),
                    routflow = ifelse(is.na(routflow), routstock - delta1*lagoutstock, routflow)
                    )

sum(!is.na(df$rinflow)) #176770  12% estimated
sum(!is.na(df$rinstock)) #154235 2%
sum(!is.na(df$routflow)) #128269 14%
sum(!is.na(df$routstock)) #118662 1%

df <- df %>% group_by(pair) %>% mutate(countin = sum((is.na(rinflow)|rinflow==0) & year >2000)) 
df <- df %>% group_by(pair) %>% mutate(countout = sum((is.na(routflow)|routflow==0)& year >2000))
df$rflowCal <- df$rinflow
df <- df %>% mutate(rflowCal = ifelse(countin > countout, routflow, rflowCal))
df <- df %>% group_by(pair) %>% mutate(countin = sum((is.na(rinstock)|rinstock==0)& year >2000))
df <- df %>% group_by(pair) %>% mutate(countout = sum((is.na(routstock)|routstock==0)& year >2000))
df$rstockCal <- df$rinstock
df <- df %>% mutate(rstockCal = ifelse(countin > countout, routstock, rstockCal))
df <- df %>% mutate(flowCal = ifelse(is.na(flownocal), rflowCal/deflator, flownocal))
df <- df %>% mutate(stockCal = ifelse(is.na(stocknocal), rstockCal/deflator, stocknocal))
temp <- df

#---- Getting final df ----
df <- temp %>% select(host, source, pair, year, stocknocal, stockCal, flownocal, flowCal) %>% filter(year > 2000)
is.num <- sapply(df, is.numeric)
df[is.num] <- lapply(df[is.num],round, 4)
df$testflow <- ifelse(is.na(df$flowCal)|df$flowCal==0,1,0)
df$teststock <- ifelse(is.na(df$stockCal)|df$stockCal==0,1,0)
df <- df %>% group_by(pair) %>% mutate(testflow=sum(testflow), teststock = sum(teststock))
df <- df %>% filter(testflow<9 & teststock<9)
hist(df$testflow)
hist(df$teststock)
write.csv(df, "4_FDIdata.csv", na="", row.names = F)

Sys.time() -s ## 23 secs
