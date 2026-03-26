### Pre-processing of Mapping German Fiction in Translation Dataset ###
##Author: Lisa Teichmann
##Date: 19 January 2024
##Comment: This script compiles the CSV's with data for each year into one table

### How to get the raw data from the DNB ###
## 1. Go to: https://portal.dnb.de/opac.htm (account required)
## 2. Filter the catalogue with the following query (Expertsearch): spo=ger and (sgt=59 or sgt=B) and (jhr=2020)
## spo" stands for original language, "sgt=59" for German literature, and "sgt=B" for Belletristik (fiction).
## Make sure to filter by books (Bücher)!
## 3. On the right hand side click on "In meine Auswahl übernehmen"
## 4. repeat the search query for each year changing "and (jhr=2020)", each time clicking on click on "In meine Auswahl übernehmen"
## Attention: for 2004 use the following query (otherwise it cannot be exportetd): (spo=ger and (sgt=59 or sgt=B) and jhr=2004) not num=9782913886933
## You can only download a maximum of 10.000 data entries per query, which is why we need to chunk it per year
## 5. In "Datenshop" (right hand side) select the queries and export as csv_title as a data format
## 6. Once you have all files, rename them and you are good to go!

#Load packages
library(ggplot2)
library(dplyr)
library(gridExtra)

#For the dataset analyzed in my thesis extracted on April 15th 2021
#Load the CSV's for each year
dnb2020<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2020.csv", header=T,row.names=NULL,sep=",")
dnb2019<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2019.csv", header=T,row.names=NULL,sep=";")
dnb2018<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2018.csv", header=T,row.names=NULL,sep=";")
dnb2017<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2017.csv", header=T,row.names=NULL,sep=";")
dnb2016<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2016.csv", header=T,row.names=NULL,sep=";")
dnb2015<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2015.csv", header=T,row.names=NULL,sep=";")
dnb2014<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2014.csv", header=T,row.names=NULL,sep=";")
dnb2013<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2013.csv", header=T,row.names=NULL,sep=";")
dnb2012<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2012.csv", header=T,row.names=NULL,sep=";")
dnb2011<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2011.csv", header=T,row.names=NULL,sep=";")
dnb2010<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2010.csv", header=T,row.names=NULL,sep=";")
dnb2009<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2009.csv", header=T,row.names=NULL,sep=";")
dnb2008<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2008.csv", header=T,row.names=NULL,sep=";")
dnb2007<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2007.csv", header=T,row.names=NULL,sep=";")
dnb2006<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2006.csv", header=T,row.names=NULL,sep=";")
dnb2005<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2005.csv", header=T,row.names=NULL,sep=";")
dnb2004<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2004.csv", header=T,row.names=NULL,sep=";")
dnb2003<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2003.csv", header=T,row.names=NULL,sep=";")
dnb2002<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2002.csv", header=T,row.names=NULL,sep=";")
dnb2001<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2001.csv", header=T,row.names=NULL,sep=";")
dnb2000<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_2000.csv", header=T,row.names=NULL,sep=";")
dnb90_99<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_1990-1999.csv", header=T,row.names=NULL,sep=";")
dnb80_89<-read.csv("Data/dnb_transdata_thesisfinal/dnb-datashop_ger_1980-89.csv", header=T,row.names=NULL,sep=";")

#Merge tables into one master table
alldnb_2021 <- rbind.data.frame(dnb2020, dnb2019, dnb2018, dnb2017, dnb2016, dnb2015, dnb2014, dnb2013, dnb2012, dnb2011, dnb2010, dnb2009, dnb2008, dnb2007, dnb2006, dnb2005, dnb2004, dnb2003, dnb2002, dnb2001, dnb2000, dnb90_99, dnb80_89)
## save
write.csv(alldnb_2021, file="Data/dnb_transdata_210415/alldnb_2021_2.csv")

# # we can filter by year for creating additional subsets
# 
# dnb1989 <- alldnb_2021[alldnb_2021$year == '1989',]
# dnb1988 <- alldnb_2021[alldnb_2021$year == '1988',]
# dnb1987 <- alldnb_2021[alldnb_2021$year == '1987',]
# dnb1986 <- alldnb_2021[alldnb_2021$year == '1986',]
# dnb1985 <- alldnb_2021[alldnb_2021$year == '1985',]
# dnb1984 <- alldnb_2021[alldnb_2021$year == '1984',]
# dnb1983 <- alldnb_2021[alldnb_2021$year == '1983',]
# dnb1982 <- alldnb_2021[alldnb_2021$year == '1982',]
# dnb1981 <- alldnb_2021[alldnb_2021$year == '1981',]
# dnb1980 <- alldnb_2021[alldnb_2021$year == '1980',]
# 
# dnb1999 <- alldnb_2021[alldnb_2021$year == '1999',]
# dnb1998 <- alldnb_2021[alldnb_2021$year == '1998',]
# dnb1997 <- alldnb_2021[alldnb_2021$year == '1997',]
# dnb1996 <- alldnb_2021[alldnb_2021$year == '1996',]
# dnb1995 <- alldnb_2021[alldnb_2021$year == '1995',]
# dnb1994 <- alldnb_2021[alldnb_2021$year == '1994',]
# dnb1993 <- alldnb_2021[alldnb_2021$year == '1993',]
# dnb1992 <- alldnb_2021[alldnb_2021$year == '1992',]
# dnb1991 <- alldnb_2021[alldnb_2021$year == '1991',]
# dnb1990 <- alldnb_2021[alldnb_2021$year == '1990',]

###For 2023 data extracted in chunks on May 22 2023
## search query for more than one year : spo=ger and (sgt=59 or sgt=B) and (jhr=1980 or jhr=1981 or jhr=1982 or jhr=1983 or jhr=1984 or jhr=1985 or jhr=1986 or jhr=1987 or jhr=1988 or jhr=1989 or jhr=1990)	

alldnb_2023_1980_1989<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_1980-1989.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_1990_1999<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_1990-1999.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_2000_2008_no2004<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_2000-2008-no2004.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_2009_2014<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_2009-2014.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_2015_2019<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_2015-2019.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_2020_2023<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T18_41_28_2020-2023.csv", header=T,row.names=NULL,sep=";")
alldnb_2023_2004<-read.csv("Data/dnb_transdata_220523/dnb-datashop_2023-5-22T19_35_14_2004.csv", header=T,row.names=NULL,sep=";")

alldnb_2023 <- rbind(alldnb_2023_1980_1989,alldnb_2023_1990_1999,alldnb_2023_2000_2008_no2004,alldnb_2023_2009_2014,alldnb_2023_2015_2019,alldnb_2023_2020_2023,alldnb_2023_2004)

write.csv(alldnb_2023, file="Data/dnb_transdata_220523/alldnb_2023_220523.csv")


