# 18 Nov 2020 
setwd("C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/RawData")

library(RCurl)
library(openxlsx)

iso3 <- read.xlsx("Iso3CountryCode.xlsx", sheet = "iso3list")
isolist <- iso3$Isocode
# isolist <- append(isolist, "XYZ")
# isolist <- append(isolist, "XYZ1")
# isolist<- append(isolist, "CHN")
i =0
errorlist <- list("1")
setwd("C:/Users/ngoca/Dropbox/Pubs_since_2018/2021_Divestment_Sample_Selection/Data/RawData/UNCTAD_FDI_data")

for (iso in isolist) {
    i = i+1
    url <- paste0( "https://unctad.org/system/files/non-official-document/webdiaeia2014d3_",iso, ".xls")
    tryCatch({
        filename <- paste0(iso, ".xls")
        download.file(url, destfile = filename, mode = "wb")
    }, error=function(x){}
        )
    print(c(i,iso))
}


# url <- "https://unctad.org/system/files/non-official-document/webdiaeia2014d3_BbGR.xls"
# download.file(url, destfile = "test1.xls", mode = "wb")
