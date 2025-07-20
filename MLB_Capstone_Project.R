library(readxl)

batdata <- read_excel("bat201019.xlsx")

batdata$PA<-batdata$AB+batdata$BB+batdata$SF+batdata$HBP
batdata$RBIRate <- batdata$RBI / batdata$PA
batdata$HitRate <- batdata$H / batdata$PA
batdata$DoubleRate <- batdata$Double / batdata$PA
batdata$TripleRate <- batdata$Triple / batdata$PA
batdata$HRRate <- batdata$HR / batdata$PA
batdata$BBRate <- batdata$BB / batdata$PA
batdata$SORate <- batdata$SO / batdata$PA
batdata$SBRate <- batdata$SB / batdata$PA
batdata$RunRate <- batdata$R / batdata$PA
batdata$IBBRate <- batdata$IBB / batdata$PA
batdata$HBPRate <- batdata$HBP / batdata$PA
batdata$SFRate <- batdata$SF / batdata$PA
batdata<-subset(batdata,PA>=300)


bat2019<-subset(batdata,yearID==2019)
woba2019 <- data.frame("playerID"=bat2019$playerID,"woba"=((.69*(bat2019$BB-bat2019$IBB)+.719*bat2019$HBP+
.87*(bat2019$H-bat2019$Double-bat2019$Triple-bat2019$HR)
+1.217*bat2019$Double+1.529*bat2019$Triple+1.94*bat2019$HR)/
(bat2019$PA-bat2019$IBB)))

bat2018 <- subset(batdata,yearID==2018)
woba2018 <- data.frame("playerID"=bat2018$playerID,"woba"=((.69*(bat2018$BB-bat2018$IBB)+.72*bat2018$HBP+
.88*(bat2018$H-bat2018$Double-bat2018$Triple-bat2018$HR)
+1.247*bat2018$Double+1.578*bat2018$Triple+2.031*bat2018$HR)/
(bat2018$PA-bat2018$IBB)))
bat201819<-merge(bat2018,woba2019,by="playerID")

bat2017 <- subset(batdata,yearID==2017)
woba2017 <- data.frame("playerID"=bat2017$playerID,"woba"=((.693*(bat2017$BB-bat2017$IBB)+.723*bat2017$HBP+
.877*(bat2017$H-bat2017$Double-bat2017$Triple-bat2017$HR)
+1.232*bat2017$Double+1.552*bat2017$Triple+1.98*bat2017$HR)/
(bat2017$PA-bat2017$IBB)))
bat201718<-merge(bat2017,woba2018,by="playerID")

bat2016 <- subset(batdata,yearID==2016)
woba2016 <- data.frame("playerID"=bat2016$playerID,"woba"=((.691*(bat2016$BB-bat2016$IBB)+.721*bat2016$HBP+
.878*(bat2016$H-bat2016$Double-bat2016$Triple-bat2016$HR)
+1.242*bat2016$Double+1.569*bat2016$Triple+2.015*bat2016$HR)/
(bat2016$PA-bat2016$IBB)))
bat201617<-merge(bat2016,woba2017,by="playerID")

bat2015 <- subset(batdata,yearID==2015)
woba2015 <- data.frame("playerID"=bat2015$playerID,"woba"=((.687*(bat2015$BB-bat2015$IBB)+.718*bat2015$HBP+
.881*(bat2015$H-bat2015$Double-bat2015$Triple-bat2015$HR)
+1.256*bat2015$Double+1.594*bat2015$Triple+2.065*bat2015$HR)/
(bat2015$PA-bat2015$IBB)))
bat201516<-merge(bat2015,woba2016,by="playerID")

bat2014 <- subset(batdata,yearID==2014)
woba2014 <- data.frame("playerID"=bat2014$playerID,"woba"=((.689*(bat2014$BB-bat2014$IBB)+.722*bat2014$HBP+
.892*(bat2014$H-bat2014$Double-bat2014$Triple-bat2014$HR)
+1.283*bat2014$Double+1.635*bat2014$Triple+2.135*bat2014$HR)/
(bat2014$PA-bat2014$IBB)))
bat201415<-merge(bat2014,woba2015,by="playerID")

bat2013 <- subset(batdata,yearID==2013)
woba2013 <- data.frame("playerID"=bat2013$playerID,"woba"=((.69*(bat2013$BB-bat2013$IBB)+.722*bat2013$HBP+
.888*(bat2013$H-bat2013$Double-bat2013$Triple-bat2013$HR)
+1.271*bat2013$Double+1.616*bat2013$Triple+2.101*bat2013$HR)/
(bat2013$PA-bat2013$IBB)))
bat201314<-merge(bat2013,woba2014,by="playerID")

bat2012 <- subset(batdata,yearID==2012)
woba2012 <- data.frame("playerID"=bat2012$playerID,"woba"=((.691*(bat2012$BB-bat2012$IBB)+.722*bat2012$HBP+
.884*(bat2012$H-bat2012$Double-bat2012$Triple-bat2012$HR)
+1.257*bat2012$Double+1.593*bat2012$Triple+2.058*bat2012$HR)/
(bat2012$PA-bat2012$IBB)))
bat201213<-merge(bat2012,woba2013,by="playerID")

bat2011 <- subset(batdata,yearID==2011)
woba2011 <- data.frame("playerID"=bat2011$playerID,"woba"=((.694*(bat2011$BB-bat2011$IBB)+.726*bat2011$HBP+
.89*(bat2011$H-bat2011$Double-bat2011$Triple-bat2011$HR)
+1.27*bat2011$Double+1.611*bat2011$Triple+2.086*bat2011$HR)/
(bat2011$PA-bat2011$IBB)))
bat201112<-merge(bat2011,woba2012,by="playerID")

bat2010 <- subset(batdata,yearID==2010)
bat201011<-merge(bat2010,woba2011,by="playerID")

battotal<-rbind(bat201011,bat201112,bat201213,bat201314,
bat201415,bat201516,bat201617,bat201718,bat201819)





batmodeltotal<-lm(woba~PA+RunRate+HitRate+DoubleRate+TripleRate+HRRate+SBRate
               +BBRate+SORate+IBBRate+HBPRate+SFRate,weights=PA,data=battotal)
summary(batmodeltotal)

plot(batmodeltotal, which = 1, col = "blue")

ncvTest(batmodeltotal)
shapiro.test(batmodeltotal$residuals)



batcordata<-data.frame(battotal$woba,battotal$PA,
battotal$RunRate,battotal$HitRate,battotal$DoubleRate,
battotal$TripleRate,battotal$HRRate,battotal$SBRate,
battotal$BBRate,battotal$SORate,battotal$IBBRate,
battotal$HBPRate,battotal$SFRate)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(batcordata)

pairs(woba~PA+RunRate+HitRate+DoubleRate+TripleRate+HRRate+SBRate
      +BBRate+SORate+IBBRate+HBPRate+SFRate,data=battotal)


battotalTransform <- battotal

#which(battotalTransform$==0)

battotalTransform$TripleRate<-battotalTransform$TripleRate+.001
battotalTransform$HRRate<-battotalTransform$HRRate+.001
battotalTransform$SBRate<-battotalTransform$SBRate+.001
battotalTransform$IBBRate<-battotalTransform$IBBRate+.001
battotalTransform$HBPRate<-battotalTransform$HBPRate+.001
battotalTransform$SFRate<-battotalTransform$SFRate+.001


require(alr4)
summary(powerTransform(cbind(PA, HitRate, DoubleRate, TripleRate, HRRate,
BBRate, SORate, SBRate, RunRate, IBBRate, SFRate, HBPRate) ~1,
weights=PA,data = battotalTransform))

batmodeltransform<-lm(woba~PA+HitRate+DoubleRate+I(log(TripleRate))+I(sqrt(HRRate))
+I(sqrt(BBRate))+I(sqrt(SORate))+I(log(SBRate))+RunRate+I(log(IBBRate))+I(sqrt(SFRate))+I(sqrt(HBPRate)),
weights=PA,data=battotalTransform)
summary(batmodeltransform)
plot(batmodeltransform, which=1, col = "blue")

ncvTest(batmodeltransform)
shapiro.test(batmodeltransform$residuals)


require(alr4)
invResPlot(batmodeltransform)

step(batmodeltransform, lower = ~1, upper = ~.)


library(MASS)

step(batmodeltransform,
     direction="back", criterion = "AIC")

battotalfinal<-battotalTransform[c(-47,-862,-1352,-829,-1380,-906),]

batmodelfinal<-lm(woba~PA+HitRate+DoubleRate+I(sqrt(HRRate))
               +I(sqrt(BBRate))+I(sqrt(SORate))+I(log(SBRate))+I(log(IBBRate))+I(sqrt(HBPRate)),
               weights=PA,data=battotalTransform)
summary(batmodelfinal)
plot(batmodelfinal, which=1, col = "blue")

ncvTest(batmodelfinal)
shapiro.test(batmodelfinal$residuals)


require(olsrr)
ols_plot_cooksd_chart(batmodeltransform)

battotalfinal<-battotalTransform[c(-47,-862,-1352,-829,-1380,-906),]




batmodelfinal2<-lm(I(log(woba))~PA+HitRate+DoubleRate+I(sqrt(HRRate))
                  +I(sqrt(BBRate))+I(sqrt(SORate))+I(log(SBRate))+I(log(IBBRate))+I(sqrt(HBPRate)),
                  weights=PA,data=battotalfinal)
summary(batmodelfinal2)
plot(batmodelfinal2, which=1, col = "blue")



ncvTest(batmodelfinal2)
shapiro.test(batmodelfinal2$residuals)








pitchdata <- read_excel("pitch201019.xlsx")

pitchdata$IPOBF<-pitchdata$IPouts/pitchdata$BFP
pitchdata$HRRate<-pitchdata$HR/pitchdata$BFP
pitchdata$BBRate<-pitchdata$BB/pitchdata$BFP
pitchdata$SORate<-pitchdata$SO/pitchdata$BFP
pitchdata$IBBRate<-pitchdata$IBB/pitchdata$BFP
pitchdata$WPRate<-pitchdata$WP/pitchdata$BFP
pitchdata$HBPRate<-pitchdata$HBP/pitchdata$BFP
pitchdata$GIDPRate<-pitchdata$GIDP/pitchdata$BFP


startdata<-subset(pitchdata,GS>=18)
startdata<-subset(startdata,G-GS<=3)
startdata$IPOGS<-startdata$IPouts/startdata$GS
startdata$WPG<-startdata$W/startdata$G
startdata$LPG<-startdata$L/startdata$G


reliefdata<-subset(pitchdata,G>=40)
reliefdata<-subset(reliefdata,GS<=2)
reliefdata$SVPG<-reliefdata$SV/(reliefdata$G-reliefdata$GS)
reliefdata$WPG<-reliefdata$W/reliefdata$G
reliefdata$LPG<-reliefdata$L/reliefdata$G


start2019<-subset(startdata,yearID==2019)
relief2019<-subset(reliefdata,yearID==2019)
startera2019<-data.frame("playerID"=start2019$playerID,"NextERA"=start2019$ERA)
reliefera2019<-data.frame("playerID"=relief2019$playerID,"NextERA"=relief2019$ERA)


start2018<-subset(startdata,yearID==2018)
relief2018<-subset(reliefdata,yearID==2018)
startera2018<-data.frame("playerID"=start2018$playerID,"NextERA"=start2018$ERA)
reliefera2018<-data.frame("playerID"=relief2018$playerID,"NextERA"=relief2018$ERA)
start201819<-merge(start2018,startera2019,by="playerID")
relief201819<-merge(relief2018,reliefera2019,by="playerID")

start2017<-subset(startdata,yearID==2017)
relief2017<-subset(reliefdata,yearID==2017)
startera2017<-data.frame("playerID"=start2017$playerID,"NextERA"=start2017$ERA)
reliefera2017<-data.frame("playerID"=relief2017$playerID,"NextERA"=relief2017$ERA)
start201718<-merge(start2017,startera2018,by="playerID")
relief201718<-merge(relief2017,reliefera2018,by="playerID")

start2016<-subset(startdata,yearID==2016)
relief2016<-subset(reliefdata,yearID==2016)
startera2016<-data.frame("playerID"=start2016$playerID,"NextERA"=start2016$ERA)
reliefera2016<-data.frame("playerID"=relief2016$playerID,"NextERA"=relief2016$ERA)
start201617<-merge(start2016,startera2017,by="playerID")
relief201617<-merge(relief2017,reliefera2017,by="playerID")

start2015<-subset(startdata,yearID==2015)
relief2015<-subset(reliefdata,yearID==2015)
startera2015<-data.frame("playerID"=start2015$playerID,"NextERA"=start2015$ERA)
reliefera2015<-data.frame("playerID"=relief2015$playerID,"NextERA"=relief2015$ERA)
start201516<-merge(start2015,startera2016,by="playerID")
relief201516<-merge(relief2015,reliefera2016,by="playerID")

start2014<-subset(startdata,yearID==2014)
relief2014<-subset(reliefdata,yearID==2014)
startera2014<-data.frame("playerID"=start2014$playerID,"NextERA"=start2014$ERA)
reliefera2014<-data.frame("playerID"=relief2014$playerID,"NextERA"=relief2014$ERA)
start201415<-merge(start2014,startera2015,by="playerID")
relief201415<-merge(relief2014,reliefera2015,by="playerID")

start2013<-subset(startdata,yearID==2013)
relief2013<-subset(reliefdata,yearID==2013)
startera2013<-data.frame("playerID"=start2013$playerID,"NextERA"=start2013$ERA)
reliefera2013<-data.frame("playerID"=relief2013$playerID,"NextERA"=relief2013$ERA)
start201314<-merge(start2013,startera2014,by="playerID")
relief201314<-merge(relief2013,reliefera2014,by="playerID")

start2012<-subset(startdata,yearID==2012)
relief2012<-subset(reliefdata,yearID==2012)
startera2012<-data.frame("playerID"=start2012$playerID,"NextERA"=start2012$ERA)
reliefera2012<-data.frame("playerID"=relief2012$playerID,"NextERA"=relief2012$ERA)
start201213<-merge(start2012,startera2013,by="playerID")
relief201213<-merge(relief2012,reliefera2013,by="playerID")

start2011<-subset(startdata,yearID==2011)
relief2011<-subset(reliefdata,yearID==2011)
startera2011<-data.frame("playerID"=start2011$playerID,"NextERA"=start2011$ERA)
reliefera2011<-data.frame("playerID"=relief2011$playerID,"NextERA"=relief2011$ERA)
start201112<-merge(start2011,startera2012,by="playerID")
relief201112<-merge(relief2011,reliefera2012,by="playerID")

start2010<-subset(startdata,yearID==2010)
relief2010<-subset(reliefdata,yearID==2010)
start201011<-merge(start2010,startera2011,by="playerID")
relief201011<-merge(relief2010,reliefera2011,by="playerID")

starttotal<-rbind(start201011,start201112,start201213,
start201314,start201415,start201516,start201617,start201718,start201819)

relieftotal<-rbind(relief201011,relief201112,relief201213,relief201314,
relief201415,relief201516,relief201617,relief201718,relief201819)


startmodel<-lm(NextERA~ERA+BAOpp+BFP+HRRate+BBRate
+SORate+IPOBF+IBBRate+WPRate+HBPRate+GIDPRate+
IPOGS+WPG+LPG,weights=BFP,data=starttotal)
summary(startmodel)

plot(startmodel, which = 1, col = "blue")

startcordata<-data.frame(starttotal$NextERA,starttotal$ERA,starttotal$BAOpp,starttotal$BFP,
                         starttotal$HRRate,starttotal$BBRate,starttotal$SORate,
                         starttotal$IPOBF,starttotal$IBBRate,starttotal$WPRate,
                         starttotal$HBPRate,starttotal$GIDPRate,starttotal$IPOGS,
                         starttotal$WPG,starttotal$LPG)

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(startcordata)

starttotalTransform <- starttotal

#which(starttotalTransform$==0)


starttotalTransform$IBBRate<-starttotalTransform$IBBRate + .001
starttotalTransform$HBPRate<-starttotalTransform$HBPRate + .001
starttotalTransform$WPRate<-starttotalTransform$WPRate + .001



require(alr4)
summary(powerTransform(cbind(ERA,BAOpp,BFP,HRRate,BBRate,
SORate,IPOBF,IBBRate,WPRate,HBPRate,GIDPRate,
IPOGS,WPG,LPG) ~1,weights=BFP, data = starttotalTransform))




startmodeltransform<-lm(NextERA~I(sqrt(ERA))+BAOpp+I(BFP^2)+I(sqrt(HRRate))
                        +BBRate+I(sqrt(SORate))+IPOBF+I(IBBRate^(1/3))
                        +I(WPRate^(1/3))+I(sqrt(HBPRate))+I(sqrt(GIDPRate))
                        +I(log(IPOGS))+I(sqrt(WPG))+LPG,
                        weights=BFP,data=starttotalTransform)
summary(startmodeltransform)
plot(startmodeltransform, which=1, col = "blue")


require(alr4)
invResPlot(startmodeltransform)


startmodeltransform<-lm(I(NextERA^(-7/10))~I(sqrt(ERA))+BAOpp+I(BFP^2)+I(sqrt(HRRate))
                        +BBRate+I(sqrt(SORate))+IPOBF+I(IBBRate^(1/3))
                        +I(WPRate^(1/3))+I(sqrt(HBPRate))+I(sqrt(GIDPRate))
                        +I(log(IPOGS))+I(sqrt(WPG))+LPG,
                        weights=BFP,data=starttotalTransform)
summary(startmodeltransform)
plot(startmodeltransform, which=1, col = "blue")


library(MASS)

step(startmodeltransform,
     direction="back", criterion = "AIC")

startmodelfinal<-lm(I(sqrt(NextERA))~I(sqrt(ERA))+I(sqrt(HRRate))
+BBRate+I(sqrt(SORate))+I(sqrt(HBPRate))
+I(log(IPOGS)),data=starttotalTransform)
summary(startmodelfinal)
plot(startmodelfinal, which=1, col = "blue")


ncvTest(startmodelfinal)
shapiro.test(startmodelfinal$residuals)



require(olsrr)
ols_plot_cooksd_bar(startmodelfinal)

starttotalfinal<-starttotalTransform[c(-269,-566,-666),]

startmodelfinal2<-lm(I(sqrt(NextERA))~I(sqrt(HRRate))
                    +BBRate+I(sqrt(SORate))+I(sqrt(HBPRate))
                    +I(log(IPOGS)),data=starttotalfinal)
summary(startmodelfinal2)
plot(startmodelfinal2, which=1, col = "blue")

ncvTest(startmodelfinal2)
shapiro.test(startmodelfinal2$residuals)















reliefmodel<-lm(NextERA~ERA+BAOpp+BFP+HRRate+BBRate
+SORate+IPOBF+IBBRate+WPRate+HBPRate+GIDPRate+
SVPG+WPG+LPG,weights=BFP,data=relieftotal)
summary(reliefmodel)


plot(reliefmodel, which = 1, col = "blue")

reliefcordata<-data.frame(relieftotal$NextERA,relieftotal$ERA,relieftotal$BAOpp,relieftotal$BFP,
                         relieftotal$HRRate,relieftotal$BBRate,relieftotal$SORate,
                         relieftotal$IPOBF,relieftotal$IBBRate,relieftotal$WPRate,
                         relieftotal$HBPRate,relieftotal$GIDPRate,relieftotal$SVPG,
                         relieftotal$WPG,relieftotal$LPG)

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(reliefcordata)

relieftotalTransform <- relieftotal

#which(relieftotalTransform$WPRate==0)


relieftotalTransform$SVPG<-relieftotalTransform$SVPG+.001
relieftotalTransform$LPG<-relieftotalTransform$LPG+.001
relieftotalTransform$HRRate<-relieftotalTransform$HRRate+.001
relieftotalTransform$HBPRate<-relieftotalTransform$HBPRate+.001
relieftotalTransform$IBBRate<-relieftotalTransform$IBBRate+.001
relieftotalTransform$GIDPRate<-relieftotalTransform$GIDPRate+.001
relieftotalTransform$WPRate<-relieftotalTransform$WPRate+.001
relieftotalTransform$WPG<-relieftotalTransform$WPG+.001



require(alr4)
summary(powerTransform(cbind(ERA,BAOpp,BFP,HRRate,BBRate,
                             SORate,IPOBF,IBBRate,WPRate,HBPRate,GIDPRate,
                             SVPG,WPG,LPG) ~1,weights=BFP, data = relieftotalTransform))




reliefmodeltransform<-lm(NextERA~I(sqrt(ERA))+BAOpp+I(sqrt(BFP))+I(sqrt(HRRate))
                        +BBRate+I(sqrt(SORate))+I(log(IPOBF))+I(IBBRate^(1/2))
                        +I(WPRate^(1/2))+I(sqrt(HBPRate))+I(sqrt(GIDPRate))
                        +I(log(SVPG))+I(sqrt(WPG))+I(sqrt(LPG)),
                        weights=BFP,data=relieftotalTransform)
summary(reliefmodeltransform)
plot(reliefmodeltransform, which=1, col = "blue")


require(alr4)
invResPlot(reliefmodeltransform)


reliefmodeltransform<-lm(I(log(NextERA))~I(sqrt(ERA))+BAOpp+I(sqrt(BFP))+I(sqrt(HRRate))
                         +BBRate+I(sqrt(SORate))+I(log(IPOBF))+I(IBBRate^(1/2))
                         +I(WPRate^(1/2))+I(sqrt(HBPRate))+I(sqrt(GIDPRate))
                         +I(log(SVPG))+I(sqrt(WPG))+I(sqrt(LPG)),
                         weights=BFP,data=relieftotalTransform)
summary(reliefmodeltransform)
plot(reliefmodeltransform, which=1, col = "blue")


library(MASS)

step(reliefmodeltransform,
     direction="back", criterion = "AIC")


reliefmodelfinal<-lm(I(sqrt(NextERA))~ERA+I(sqrt(HRRate))
                         +I(sqrt(SORate))+I(WPRate^(1/2)),
                         weights=BFP,data=relieftotalTransform)
summary(reliefmodelfinal)
plot(reliefmodelfinal, which=1, col = "blue")


ncvTest(reliefmodelfinal)
shapiro.test(reliefmodelfinal$residuals)



require(olsrr)
ols_plot_cooksd_bar(reliefmodelfinal)

relieftotalfinal2<-relieftotalTransform[c(-335,-445,-530,-577,-897,-408,-407,-41),]



reliefmodelfinal2<-lm(I((NextERA)^(1/2))~I(sqrt(ERA))+I(log(HRRate))
                        +I(SORate^(1/3))+I(WPRate^(1/2)),
                        weights=BFP,data=relieftotalfinal2)
summary(reliefmodelfinal2)
plot(reliefmodelfinal2,which=1, col = "blue")


ncvTest(reliefmodelfinal2)
shapiro.test(reliefmodelfinal2$residuals)

#which(reliefmodelfinal2$fitted.values<1.25)






mean(relieftotal$BFP)
mean(starttotal$BFP)













step<-c(seq(1,601,by=10))

rsqr <- matrix(ncol=2,nrow=61)

num=1

library(readxl)

for (i in step) {

batdata <- read_excel("bat201019.xlsx")

batdata$PA<-batdata$AB+batdata$BB+batdata$SF+batdata$HBP
batdata$RBIRate <- batdata$RBI / batdata$PA
batdata$HitRate <- batdata$H / batdata$PA
batdata$DoubleRate <- batdata$Double / batdata$PA
batdata$TripleRate <- batdata$Triple / batdata$PA
batdata$HRRate <- batdata$HR / batdata$PA
batdata$BBRate <- batdata$BB / batdata$PA
batdata$SORate <- batdata$SO / batdata$PA
batdata$SBRate <- batdata$SB / batdata$PA
batdata$RunRate <- batdata$R / batdata$PA
batdata$IBBRate <- batdata$IBB / batdata$PA
batdata$HBPRate <- batdata$HBP / batdata$PA
batdata$SFRate <- batdata$SF / batdata$PA
batdata<-subset(batdata,PA>=i)

bat2019<-subset(batdata,yearID==2019)
woba2019 <- data.frame("playerID"=bat2019$playerID,"woba"=((.69*(bat2019$BB-bat2019$IBB)+.719*bat2019$HBP+
                                                              .87*(bat2019$H-bat2019$Double-bat2019$Triple-bat2019$HR)
                                                            +1.217*bat2019$Double+1.529*bat2019$Triple+1.94*bat2019$HR)/
                                                             (bat2019$PA-bat2019$IBB)))

bat2018 <- subset(batdata,yearID==2018)
woba2018 <- data.frame("playerID"=bat2018$playerID,"woba"=((.69*(bat2018$BB-bat2018$IBB)+.72*bat2018$HBP+
                                                              .88*(bat2018$H-bat2018$Double-bat2018$Triple-bat2018$HR)
                                                            +1.247*bat2018$Double+1.578*bat2018$Triple+2.031*bat2018$HR)/
                                                             (bat2018$PA-bat2018$IBB)))
bat201819<-merge(bat2018,woba2019,by="playerID")

bat2017 <- subset(batdata,yearID==2017)
woba2017 <- data.frame("playerID"=bat2017$playerID,"woba"=((.693*(bat2017$BB-bat2017$IBB)+.723*bat2017$HBP+
                                                              .877*(bat2017$H-bat2017$Double-bat2017$Triple-bat2017$HR)
                                                            +1.232*bat2017$Double+1.552*bat2017$Triple+1.98*bat2017$HR)/
                                                             (bat2017$PA-bat2017$IBB)))
bat201718<-merge(bat2017,woba2018,by="playerID")

bat2016 <- subset(batdata,yearID==2016)
woba2016 <- data.frame("playerID"=bat2016$playerID,"woba"=((.691*(bat2016$BB-bat2016$IBB)+.721*bat2016$HBP+
                                                              .878*(bat2016$H-bat2016$Double-bat2016$Triple-bat2016$HR)
                                                            +1.242*bat2016$Double+1.569*bat2016$Triple+2.015*bat2016$HR)/
                                                             (bat2016$PA-bat2016$IBB)))
bat201617<-merge(bat2016,woba2017,by="playerID")

bat2015 <- subset(batdata,yearID==2015)
woba2015 <- data.frame("playerID"=bat2015$playerID,"woba"=((.687*(bat2015$BB-bat2015$IBB)+.718*bat2015$HBP+
                                                              .881*(bat2015$H-bat2015$Double-bat2015$Triple-bat2015$HR)
                                                            +1.256*bat2015$Double+1.594*bat2015$Triple+2.065*bat2015$HR)/
                                                             (bat2015$PA-bat2015$IBB)))
bat201516<-merge(bat2015,woba2016,by="playerID")

bat2014 <- subset(batdata,yearID==2014)
woba2014 <- data.frame("playerID"=bat2014$playerID,"woba"=((.689*(bat2014$BB-bat2014$IBB)+.722*bat2014$HBP+
                                                              .892*(bat2014$H-bat2014$Double-bat2014$Triple-bat2014$HR)
                                                            +1.283*bat2014$Double+1.635*bat2014$Triple+2.135*bat2014$HR)/
                                                             (bat2014$PA-bat2014$IBB)))
bat201415<-merge(bat2014,woba2015,by="playerID")

bat2013 <- subset(batdata,yearID==2013)
woba2013 <- data.frame("playerID"=bat2013$playerID,"woba"=((.69*(bat2013$BB-bat2013$IBB)+.722*bat2013$HBP+
                                                              .888*(bat2013$H-bat2013$Double-bat2013$Triple-bat2013$HR)
                                                            +1.271*bat2013$Double+1.616*bat2013$Triple+2.101*bat2013$HR)/
                                                             (bat2013$PA-bat2013$IBB)))
bat201314<-merge(bat2013,woba2014,by="playerID")

bat2012 <- subset(batdata,yearID==2012)
woba2012 <- data.frame("playerID"=bat2012$playerID,"woba"=((.691*(bat2012$BB-bat2012$IBB)+.722*bat2012$HBP+
                                                              .884*(bat2012$H-bat2012$Double-bat2012$Triple-bat2012$HR)
                                                            +1.257*bat2012$Double+1.593*bat2012$Triple+2.058*bat2012$HR)/
                                                             (bat2012$PA-bat2012$IBB)))
bat201213<-merge(bat2012,woba2013,by="playerID")

bat2011 <- subset(batdata,yearID==2011)
woba2011 <- data.frame("playerID"=bat2011$playerID,"woba"=((.694*(bat2011$BB-bat2011$IBB)+.726*bat2011$HBP+
                                                              .89*(bat2011$H-bat2011$Double-bat2011$Triple-bat2011$HR)
                                                            +1.27*bat2011$Double+1.611*bat2011$Triple+2.086*bat2011$HR)/
                                                             (bat2011$PA-bat2011$IBB)))
bat201112<-merge(bat2011,woba2012,by="playerID")

bat2010 <- subset(batdata,yearID==2010)
bat201011<-merge(bat2010,woba2011,by="playerID")

battotal<-rbind(bat201011,bat201112,bat201213,bat201314,
                bat201415,bat201516,bat201617,bat201718,bat201819)

modeltotal<-lm(woba~PA+RunRate+HitRate+DoubleRate+TripleRate+HRRate+SBRate+BBRate+SORate+IBBRate+HBPRate+SFRate,weights=PA,data=battotal)

rsqr[num,1]<-i
rsqr[num,2]<-summary(modeltotal)$r.squared

num=num+1

}

colnames(rsqr)<-c("Minimum Plate Appearances", "R^2")

plot(rsqr,main="Minimum Plate Appearances vs. R^2",ylim=c(0,0.5))








step<-c(seq(1,30,by=1))

rsqrstart <- matrix(ncol=2,nrow=30)
rsqrrelief <- matrix(ncol=2,nrow=30)

num=1

library(readxl)

for (i in step) {
  
  
  pitchdata <- read_excel("pitch201019.xlsx")
  
  pitchdata$IPOBF<-pitchdata$IPouts/pitchdata$BFP
  pitchdata$HRRate<-pitchdata$HR/pitchdata$BFP
  pitchdata$BBRate<-pitchdata$BB/pitchdata$BFP
  pitchdata$SORate<-pitchdata$SO/pitchdata$BFP
  pitchdata$IBBRate<-pitchdata$IBB/pitchdata$BFP
  pitchdata$WPRate<-pitchdata$WP/pitchdata$BFP
  pitchdata$HBPRate<-pitchdata$HBP/pitchdata$BFP
  pitchdata$GIDPRate<-pitchdata$GIDP/pitchdata$BFP
  
  
  startdata<-subset(pitchdata,GS>=i)
  startdata<-subset(startdata,G-GS<=3)
  startdata$IPOGS<-startdata$IPouts/startdata$GS
  startdata$WPG<-startdata$W/startdata$G
  startdata$LPG<-startdata$L/startdata$G
  
  
  reliefdata<-subset(pitchdata,G>=40)
  reliefdata<-subset(reliefdata,GS<=2)
  reliefdata$SVPG<-reliefdata$SV/(reliefdata$G-reliefdata$GS)
  reliefdata$WPG<-reliefdata$W/reliefdata$G
  reliefdata$LPG<-reliefdata$L/reliefdata$G
  
  
  start2019<-subset(startdata,yearID==2019)
  relief2019<-subset(reliefdata,yearID==2019)
  startera2019<-data.frame("playerID"=start2019$playerID,"NextERA"=start2019$ERA)
  reliefera2019<-data.frame("playerID"=relief2019$playerID,"NextERA"=relief2019$ERA)
  
  
  start2018<-subset(startdata,yearID==2018)
  relief2018<-subset(reliefdata,yearID==2018)
  startera2018<-data.frame("playerID"=start2018$playerID,"NextERA"=start2018$ERA)
  reliefera2018<-data.frame("playerID"=relief2018$playerID,"NextERA"=relief2018$ERA)
  start201819<-merge(start2018,startera2019,by="playerID")
  relief201819<-merge(relief2018,reliefera2019,by="playerID")
  
  start2017<-subset(startdata,yearID==2017)
  relief2017<-subset(reliefdata,yearID==2017)
  startera2017<-data.frame("playerID"=start2017$playerID,"NextERA"=start2017$ERA)
  reliefera2017<-data.frame("playerID"=relief2017$playerID,"NextERA"=relief2017$ERA)
  start201718<-merge(start2017,startera2018,by="playerID")
  relief201718<-merge(relief2017,reliefera2018,by="playerID")
  
  start2016<-subset(startdata,yearID==2016)
  relief2016<-subset(reliefdata,yearID==2016)
  startera2016<-data.frame("playerID"=start2016$playerID,"NextERA"=start2016$ERA)
  reliefera2016<-data.frame("playerID"=relief2016$playerID,"NextERA"=relief2016$ERA)
  start201617<-merge(start2016,startera2017,by="playerID")
  relief201617<-merge(relief2017,reliefera2017,by="playerID")
  
  start2015<-subset(startdata,yearID==2015)
  relief2015<-subset(reliefdata,yearID==2015)
  startera2015<-data.frame("playerID"=start2015$playerID,"NextERA"=start2015$ERA)
  reliefera2015<-data.frame("playerID"=relief2015$playerID,"NextERA"=relief2015$ERA)
  start201516<-merge(start2015,startera2016,by="playerID")
  relief201516<-merge(relief2015,reliefera2016,by="playerID")
  
  start2014<-subset(startdata,yearID==2014)
  relief2014<-subset(reliefdata,yearID==2014)
  startera2014<-data.frame("playerID"=start2014$playerID,"NextERA"=start2014$ERA)
  reliefera2014<-data.frame("playerID"=relief2014$playerID,"NextERA"=relief2014$ERA)
  start201415<-merge(start2014,startera2015,by="playerID")
  relief201415<-merge(relief2014,reliefera2015,by="playerID")
  
  start2013<-subset(startdata,yearID==2013)
  relief2013<-subset(reliefdata,yearID==2013)
  startera2013<-data.frame("playerID"=start2013$playerID,"NextERA"=start2013$ERA)
  reliefera2013<-data.frame("playerID"=relief2013$playerID,"NextERA"=relief2013$ERA)
  start201314<-merge(start2013,startera2014,by="playerID")
  relief201314<-merge(relief2013,reliefera2014,by="playerID")
  
  start2012<-subset(startdata,yearID==2012)
  relief2012<-subset(reliefdata,yearID==2012)
  startera2012<-data.frame("playerID"=start2012$playerID,"NextERA"=start2012$ERA)
  reliefera2012<-data.frame("playerID"=relief2012$playerID,"NextERA"=relief2012$ERA)
  start201213<-merge(start2012,startera2013,by="playerID")
  relief201213<-merge(relief2012,reliefera2013,by="playerID")
  
  start2011<-subset(startdata,yearID==2011)
  relief2011<-subset(reliefdata,yearID==2011)
  startera2011<-data.frame("playerID"=start2011$playerID,"NextERA"=start2011$ERA)
  reliefera2011<-data.frame("playerID"=relief2011$playerID,"NextERA"=relief2011$ERA)
  start201112<-merge(start2011,startera2012,by="playerID")
  relief201112<-merge(relief2011,reliefera2012,by="playerID")
  
  start2010<-subset(startdata,yearID==2010)
  relief2010<-subset(reliefdata,yearID==2010)
  start201011<-merge(start2010,startera2011,by="playerID")
  relief201011<-merge(relief2010,reliefera2011,by="playerID")
  
  starttotal<-rbind(start201011,start201112,start201213,
                    start201314,start201415,start201516,start201617,start201718,start201819)
  
  relieftotal<-rbind(relief201011,relief201112,relief201213,relief201314,
                     relief201415,relief201516,relief201617,relief201718,relief201819)
  
  
  startmodel<-lm(NextERA~ERA+BAOpp+BFP+HRRate+BBRate
                 +SORate+IPOBF+IBBRate+WPRate+HBPRate+GIDPRate+
                   IPOGS+WPG+LPG,weights=BFP,data=starttotal)

  
  #reliefmodel<-lm(NextERA~ERA+BAOpp+BFP+HRRate+BBRate
   #               +SORate+IPOBF+IBBRate+WPRate+HBPRate+GIDPRate+
     #               SVPG+WPG+LPG,weights=BFP,data=relieftotal)
 
  
  
  rsqrstart[num,1]<-i
  rsqrstart[num,2]<-summary(startmodel)$r.squared
  
  #rsqrrelief[num,1]<-i
  #rsqrrelief[num,2]<-summary(reliefmodel)$r.squared
  
  num=num+1
  
}
  
  

colnames(rsqrstart)<-c("Minimum Games Started", "R^2")

plot(rsqrstart,main="Minimum Games Started vs. R^2",ylim=c(0,0.5))





step<-c(seq(2,60,by=2))

#rsqrstart <- matrix(ncol=2,nrow=30)
rsqrrelief <- matrix(ncol=2,nrow=30)

num=1

library(readxl)

for (i in step) {
  
  
  pitchdata <- read_excel("pitch201019.xlsx")
  
  pitchdata$IPOBF<-pitchdata$IPouts/pitchdata$BFP
  pitchdata$HRRate<-pitchdata$HR/pitchdata$BFP
  pitchdata$BBRate<-pitchdata$BB/pitchdata$BFP
  pitchdata$SORate<-pitchdata$SO/pitchdata$BFP
  pitchdata$IBBRate<-pitchdata$IBB/pitchdata$BFP
  pitchdata$WPRate<-pitchdata$WP/pitchdata$BFP
  pitchdata$HBPRate<-pitchdata$HBP/pitchdata$BFP
  pitchdata$GIDPRate<-pitchdata$GIDP/pitchdata$BFP
  
  
  startdata<-subset(pitchdata,GS>=18)
  startdata<-subset(startdata,G-GS<=3)
  startdata$IPOGS<-startdata$IPouts/startdata$GS
  startdata$WPG<-startdata$W/startdata$G
  startdata$LPG<-startdata$L/startdata$G
  
  
  reliefdata<-subset(pitchdata,G>=i)
  reliefdata<-subset(reliefdata,GS<=2)
  reliefdata$SVPG<-reliefdata$SV/(reliefdata$G-reliefdata$GS)
  reliefdata$WPG<-reliefdata$W/reliefdata$G
  reliefdata$LPG<-reliefdata$L/reliefdata$G
  
  
  start2019<-subset(startdata,yearID==2019)
  relief2019<-subset(reliefdata,yearID==2019)
  startera2019<-data.frame("playerID"=start2019$playerID,"NextERA"=start2019$ERA)
  reliefera2019<-data.frame("playerID"=relief2019$playerID,"NextERA"=relief2019$ERA)
  
  
  start2018<-subset(startdata,yearID==2018)
  relief2018<-subset(reliefdata,yearID==2018)
  startera2018<-data.frame("playerID"=start2018$playerID,"NextERA"=start2018$ERA)
  reliefera2018<-data.frame("playerID"=relief2018$playerID,"NextERA"=relief2018$ERA)
  start201819<-merge(start2018,startera2019,by="playerID")
  relief201819<-merge(relief2018,reliefera2019,by="playerID")
  
  start2017<-subset(startdata,yearID==2017)
  relief2017<-subset(reliefdata,yearID==2017)
  startera2017<-data.frame("playerID"=start2017$playerID,"NextERA"=start2017$ERA)
  reliefera2017<-data.frame("playerID"=relief2017$playerID,"NextERA"=relief2017$ERA)
  start201718<-merge(start2017,startera2018,by="playerID")
  relief201718<-merge(relief2017,reliefera2018,by="playerID")
  
  start2016<-subset(startdata,yearID==2016)
  relief2016<-subset(reliefdata,yearID==2016)
  startera2016<-data.frame("playerID"=start2016$playerID,"NextERA"=start2016$ERA)
  reliefera2016<-data.frame("playerID"=relief2016$playerID,"NextERA"=relief2016$ERA)
  start201617<-merge(start2016,startera2017,by="playerID")
  relief201617<-merge(relief2017,reliefera2017,by="playerID")
  
  start2015<-subset(startdata,yearID==2015)
  relief2015<-subset(reliefdata,yearID==2015)
  startera2015<-data.frame("playerID"=start2015$playerID,"NextERA"=start2015$ERA)
  reliefera2015<-data.frame("playerID"=relief2015$playerID,"NextERA"=relief2015$ERA)
  start201516<-merge(start2015,startera2016,by="playerID")
  relief201516<-merge(relief2015,reliefera2016,by="playerID")
  
  start2014<-subset(startdata,yearID==2014)
  relief2014<-subset(reliefdata,yearID==2014)
  startera2014<-data.frame("playerID"=start2014$playerID,"NextERA"=start2014$ERA)
  reliefera2014<-data.frame("playerID"=relief2014$playerID,"NextERA"=relief2014$ERA)
  start201415<-merge(start2014,startera2015,by="playerID")
  relief201415<-merge(relief2014,reliefera2015,by="playerID")
  
  start2013<-subset(startdata,yearID==2013)
  relief2013<-subset(reliefdata,yearID==2013)
  startera2013<-data.frame("playerID"=start2013$playerID,"NextERA"=start2013$ERA)
  reliefera2013<-data.frame("playerID"=relief2013$playerID,"NextERA"=relief2013$ERA)
  start201314<-merge(start2013,startera2014,by="playerID")
  relief201314<-merge(relief2013,reliefera2014,by="playerID")
  
  start2012<-subset(startdata,yearID==2012)
  relief2012<-subset(reliefdata,yearID==2012)
  startera2012<-data.frame("playerID"=start2012$playerID,"NextERA"=start2012$ERA)
  reliefera2012<-data.frame("playerID"=relief2012$playerID,"NextERA"=relief2012$ERA)
  start201213<-merge(start2012,startera2013,by="playerID")
  relief201213<-merge(relief2012,reliefera2013,by="playerID")
  
  start2011<-subset(startdata,yearID==2011)
  relief2011<-subset(reliefdata,yearID==2011)
  startera2011<-data.frame("playerID"=start2011$playerID,"NextERA"=start2011$ERA)
  reliefera2011<-data.frame("playerID"=relief2011$playerID,"NextERA"=relief2011$ERA)
  start201112<-merge(start2011,startera2012,by="playerID")
  relief201112<-merge(relief2011,reliefera2012,by="playerID")
  
  start2010<-subset(startdata,yearID==2010)
  relief2010<-subset(reliefdata,yearID==2010)
  start201011<-merge(start2010,startera2011,by="playerID")
  relief201011<-merge(relief2010,reliefera2011,by="playerID")
  
  starttotal<-rbind(start201011,start201112,start201213,
                    start201314,start201415,start201516,start201617,start201718,start201819)
  
  relieftotal<-rbind(relief201011,relief201112,relief201213,relief201314,
                     relief201415,relief201516,relief201617,relief201718,relief201819)
  
  

  
  reliefmodel<-lm(NextERA~ERA+BAOpp+BFP+HRRate+BBRate
                 +SORate+IPOBF+IBBRate+WPRate+HBPRate+GIDPRate+
                 SVPG+WPG+LPG,weights=BFP,data=relieftotal)
  
  
  

  
  rsqrrelief[num,1]<-i
  rsqrrelief[num,2]<-summary(reliefmodel)$r.squared
  
  num=num+1
  
}



colnames(rsqrrelief)<-c("Minimum Games", "R^2")

plot(rsqrrelief,main="Minimum Games vs. R^2",ylim=c(0,0.5))
  