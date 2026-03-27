### to clear data
rm()

library(psych)

### to open a file
datum=read.csv(file.choose())
### use Barr & Benef_update csv file

### view column titles
head (datum)

BarrierFactor1=data.frame(datum[,1:5])
alpha (BarrierFactor1)

BarrierFactor2=data.frame(datum[,7:12])
alpha (BarrierFactor2)

BarrierFactor3=data.frame(datum[,14:16])
alpha (BarrierFactor3)

BarrierFactor4=data.frame(datum[,18:21])
alpha (BarrierFactor4)

--------------

BenefitFactor1=data.frame(datum[,23:32])
alpha (BenefitFactor1)

BenefitFactor2=data.frame(datum[,34:35])
alpha (BenefitFactor2)

BenefitFactor3=data.frame(datum[,37:38])
alpha (BenefitFactor3)

BenefitFactor4=data.frame(datum[,40:41])
alpha (BenefitFactor4)

BenefitFactor5=data.frame(datum[,43:45])
alpha (BenefitFactor5)

BenefitFactor6=data.frame(datum[,47:48])
alpha (BenefitFactor6)

BenefitFactor7=data.frame(datum[,50:51])
alpha (BenefitFactor7)

BenefitFactor8=data.frame(datum[,53:55])
alpha (BenefitFactor8)
-----
#Covid Cronbach

CovidAll=data.frame(datum[,1:])
alpha (CovidAll)











