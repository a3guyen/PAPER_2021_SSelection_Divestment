rm(list=ls())
options(java.parameters="-Xmx6g")
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
path <- "C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/"

setwd(paste0(path,"RawData/OECD_FDI_data"))
s<-Sys.time() 
#------
# ------------------------------------------- OECD Benchmark Definiition 4th Edition ----------------------------
csv <- read.csv("FDI_CTRY_ECO_HIST_22112020003946508.csv")
names(csv)[1] <- "IsoReporting"

str(csv)
# R1 in OECD4 is BLX in OECD3 => need to change this
csv$IsoReporting <- ifelse(csv$IsoReporting=="R1", "BLX", csv$IsoReporting)
csv$COUNTERPART_AREA <- ifelse(csv$COUNTERPART_AREA== "R1", "BLX", csv$COUNTERPART_AREA)


unique(csv$Type.of.FDI)
#get iso-country names

df1 <- csv %>% select(IsoReporting, Reporting.country) %>% distinct()
names(df1) <- c("iso", "country")
df2 <- csv %>% select(COUNTERPART_AREA, Partner.country.territory) %>% distinct()
names(df2) <- c("iso", "country")
df <- rbind(df1, df2) %>% distinct()
isocode<- read_excel(paste0(path,"RawData/Iso3CountryCode.xlsx"), sheet = 1)
test <- merge(df, isocode, by.x = "iso", by.y = "Isocode", all = T)

#get inflows, outflows, instock, outstock
df <- csv %>% filter(MEASURE=="USD", Type.of.FDI=="FDI financial flows - Total", Economic.activity=="All FDI activities")
df <- df %>% select(IsoReporting,  MEASURE_PRINCIPLE, Year, 
                    Value, COUNTERPART_AREA)
unique(df$MEASURE_PRINCIPLE)
dinflow <- df %>% filter(MEASURE_PRINCIPLE=="DI")
names(dinflow)[1] <- "host"
colnames(dinflow)[which(names(dinflow) == "COUNTERPART_AREA")] <- "source"
dinflow$MEASURE_PRINCIPLE <- "inflow_oecd4"

doutflow <- df %>% filter(MEASURE_PRINCIPLE=="DO")
names(doutflow)[1] <- "source"
colnames(doutflow)[which(names(doutflow) == "COUNTERPART_AREA")] <- "host"
doutflow$MEASURE_PRINCIPLE <- "outflow_oecd4"


df <- csv %>% filter(MEASURE=="USD", Type.of.FDI=="FDI positions -Total", Economic.activity=="All FDI activities")
df <- df %>% select(IsoReporting,  MEASURE_PRINCIPLE, Year, 
                    Value, COUNTERPART_AREA)
unique(df$MEASURE_PRINCIPLE)
dinstock <- df %>% filter(MEASURE_PRINCIPLE=="DI")
names(dinstock)[1] <- "host"
colnames(dinstock)[which(names(dinstock) == "COUNTERPART_AREA")] <- "source"
dinstock$MEASURE_PRINCIPLE <- "instock_oecd4"


doutstock <- df %>% filter(MEASURE_PRINCIPLE=="DO")
names(doutstock)[1] <- "source"
colnames(doutstock)[which(names(doutstock) == "COUNTERPART_AREA")] <- "host"
doutstock$MEASURE_PRINCIPLE <- "outstock_oecd4"

df <- rbind(dinflow, dinstock, doutflow, doutstock)
df <- reshape(df, idvar = c("host", "source",  "Year"), timevar = c("MEASURE_PRINCIPLE"), direction = "wide")
names(df) <- gsub("Value.", "", names(df))
oecd4 <- df

# ------------------------------------------- OECD Benchmark Definiition 3rd Edition ----------------------------
csv1 <- read.csv("FDI_FLOW_PARTNER_22112020023830897.csv")
csv2 <- read.csv("FDI_POSITION_PARTNER_22112020024648195.csv")
#--- Checking country name and iso
df1 <- csv1 %>% select(PC, Partner.country) %>% distinct()
names(df1) <- c("iso", "country")
df2 <- csv1 %>% select(COU, Reporting.country) %>% distinct()
names(df2) <- c("iso", "country")
df3 <- csv2 %>% select(PC, Partner.country) %>% distinct()
names(df3) <- c("iso", "country")
df4 <- csv2 %>% select(COU, Reporting.country) %>% distinct()
names(df4) <- c("iso", "country")
df <- rbind(df1, df2, df3, df4) %>% distinct()
isocode<- read_excel(paste0(path,"RawData/Iso3CountryCode.xlsx"), sheet = 1)
test <- merge(df, isocode, by.x = "iso", by.y = "Isocode", all = T)
#--- Flows
str(csv1)
df <- csv1 %>% filter(CUR == "USD") %>% select(PC, COU, Year, Type.of.FDI, Value) %>% distinct()
unique(df$Type.of.FDI)
df1 <- df %>% filter(Type.of.FDI == "Inward")
names(df1)[c(1,2)] <- c("source", "host")
df1$Type.of.FDI <- "inflow_oecd3"
df2 <- df %>% filter(Type.of.FDI == "Outward")
names(df2)[c(1,2)] <- c( "host", "source")
df2$Type.of.FDI <- "outflow_oecd3"
#--- Stocks
str(csv2)
df <- csv2 %>% filter(CUR == "USD") %>% select(PC, COU, Year, Type.of.FDI, Value) %>% distinct()
unique(df$Type.of.FDI)
df3 <- df %>% filter(Type.of.FDI == "Inward")
names(df3)[c(1,2)] <- c("source", "host")
df3$Type.of.FDI <- "instock_oecd3"
df4 <- df %>% filter(Type.of.FDI == "Outward")
names(df4)[c(1,2)] <- c( "host", "source")
df4$Type.of.FDI <- "outstock_oecd3"
#--- Merging
df <- rbind(df1,df2,df3, df4)
df <- reshape(df, idvar = c("host", "source",  "Year"), timevar = c("Type.of.FDI"), direction = "wide")
names(df) <- gsub("Value.", "", names(df))
oecd3 <- df
#---filtering
oecd <- merge(oecd3, oecd4, by.x = c("host", "source",  "Year"), by.y = c("host", "source",  "Year"), all = T)
isocode<- read_excel(paste0(path,"RawData/Iso3CountryCode.xlsx"), sheet = 1)
oecd <- merge(oecd, isocode, by.x = "host", by.y = "Isocode") #get only obs with host's iso match the isocode data frame
oecd <- oecd %>% select(-Country)
oecd <- merge(oecd, isocode, by.x = "source", by.y = "Isocode") #get only obs with source's iso match the isocode data frame
oecd <- oecd %>% select(-Country)


setwd(paste0(path,"DerivedData"))
filetosave <- list(oecd3, oecd4, oecd)
save(oecd3, oecd4, oecd, file='3_OECD.RData')

Sys.time() -s ## 23 secs