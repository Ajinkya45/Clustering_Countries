# The clustering is based upon average values of indicators across all years 

# load libraries
library(readr) # CSV file I/O, e.g. the read_csv function
library(reshape) # to transform rows to columns (cast function)
library(cluster) # to plot clusters

# load data
Indicators <- read.csv("../input/Indicators.csv")

# Below indicators are used to cluster countries
code <- c('SE.XPD.CTOT.ZS','GB.XPD.RSDV.GD.ZS','SE.ADT.LITR.ZS','EG.ELC.ACCS.ZS',
          'SL.UEM.LTRM.ZS','SL.UEM.TOTL.ZS','FB.CBK.BRCH.P5','GC.DOD.TOTL.GD.ZS','EN.ATM.CO2E.PC',
          'BX.KLT.DINV.WD.GD.ZS','NY.GDP.PCAP.CD','NY.GNP.MKTP.CD','FP.CPI.TOTL.ZG','NY.ADJ.NNTY.PC.CD',
          'PA.NUS.FCRF','DT.DOD.PVLX.GN.ZS','IC.REG.DURS','FI.RES.TOTL.DT.ZS','SH.MED.BEDS.ZS',
          'SP.POP.1564.TO.ZS','TX.VAL.TECH.CD','SP.DYN.CDRT.IN','SP.DYN.CBRT.IN','SI.POV.GINI',
          'IC.BUS.EASE.XQ','IT.NET.USER.P2','IT.CEL.SETS.P2','IQ.CPA.TRAD.XQ','TM.VAL.FUEL.ZS.UN',
          'TX.VAL.MANF.ZS.UN','NE.TRD.GNFS.ZS')

# Transforming data to perform clustering          
indic <- cast(Indicators, CountryName ~ IndicatorCode, mean)
indic <- indic[,c('CountryName',code)]
row.names(indic) <- indic[,1]
indic <- indic[,c(-1,-2,-4,-6,-9,-14,-17,-19,-25,-29)]
indic <- na.omit(indic)
Cnames <- rownames(indic)

# Standerdising data
indic.norm <- sapply(indic, scale)
row.names(indic.norm) <- Cnames

# K-means clustering
set.seed(42)
indic_cluster <- kmeans(indic.norm, 3)
indic_cluster$size

# plotting clusters
par(mfrow=c(1,1))
clusplot(indic.norm,indic_cluster$cluster,color = TRUE, shade = TRUE, labels = 2, lines = 0)

indic.norm <- data.frame(cbind(indic_cluster$cluster, rownames(indic), indic.norm))
colnames(indic.norm) <- c("Cluster_No","CountryName",colnames(indic.norm)[c(-1,-2)])
rownames(indic.norm) <- NULL

developed_countries <- subset(indic.norm, Cluster_No == 3, select = CountryName)
developing_countries <- subset(indic.norm, Cluster_No == 1, select = CountryName)
underdeveloped_countries <- subset(indic.norm, Cluster_No == 2, select = CountryName)

print(developed_countries)
print(developing_countries)
print(underdeveloped_countries)
