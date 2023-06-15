## climate-child health analysis
## June 2023
## Corey Bradshaw

## libraries
library(boot)
library(mice)
library(dismo)
library(gbm)

## functions
# beta distribution shape parameter estimator function
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

## import data
dat <- read.table("alldat2.csv", sep=";", header=T)
head(dat)

## population density
plot(dat$AREASQKM21, log10(dat$POP_sum), pch=19)
plot(dat$AREASQKM21, log10(dat$POP_mean), pch=19)
dat$popD <- dat$POP_sum/dat$AREASQKM21
hist(log10(dat$popD))

## income
hist(log10(dat$HSEdat_Inc))

## number in household
hist(dat$HSEdat_HSw)
plot(log10(dat$popD), dat$HSEdat_HSw, pch=19)

### climate data
## mean max temperature warmest month
hist(dat$MAXTEMP_me)

## mean min temperature coldest month
hist(dat$MINT_mean)

## mean isothermality (temperature evenness)
hist(dat$ISOTHERM_2)

## mean precipitation wettest month
hist(log10(dat$PRCP_mean))

### health data
## % live births with low birth weight (2014-2016)
hist(logit(dat$HSEdat_pcL/100))

## infant and child (< 6 yrs old) mortality
hist(dat$HSEdat_d_2)
hist(ifelse(dat$HSEdat_d_2 == 0, log10(1), log10(dat$HSEdat_d_2)))

## % Apgar < 7
hist(dat$HSEdat_Apg)
hist(logit(dat$HSEdat_Apg/100))

### multiple imputation of health data (include all columns)
## all health data
hlthDat <- dat[,c(18:(16+35))]
colnames(hlthDat) <- c("dthInf1416", "dthInfYC1416", "dthInf1315", "dthInfYC1315", "dthInf1214", "dthInfYC1214", "dthInf1113",
                       "dthInfYC1113", "dthInf1012", "dthInfYC1012", "pcLObw1416", "pcLObw1315", "pcLObw1214", "pcWomSmk1416",
                       "pcWomSmk1315", "pcWomSmk1214", "womGE1av1416", "womGE1av1315", "womGE1av1214","pcWom20wSmk1719",
                       "pcWom20wRepSmk1719", "pcAC1tri1719", "pcAC10wk1719", "Episio1", "Episio2","ApgarLT7","LabInd",
                       "caesar", "nonInstrVagBirth", "InstrVagBirth", "genAnaesth", "pcSmallBab40wk", "tears1stB", "tearsAllB")
head(hlthDat)
str(hlthDat)
dim(hlthDat)

## only most recent data
hlthDatRec <- hlthDat[,c(1,2,11,14,17,20:34)]
head(hlthDatRec)

## multiple imputation
md.pattern(hlthDatRec)
hlthDatRec.imp <- mice(hlthDatRec, m=dim(hlthDatRec)[2], maxit=500, method="pmm", seed=101)
summary(hlthDatRec)

hlthDatRec.imp$imp$dthInf1416
hlthDatRec.imp$imp$dthInfYC1416
hlthDatRec.imp$imp$pcLObw1416
hlthDatRec.imp$imp$pcWomSmk1416
hlthDatRec.imp$imp$womGE1av1416
hlthDatRec.imp$imp$pcWom20wSmk1719
hlthDatRec.imp$imp$pcWom20wRepSmk1719
hlthDatRec.imp$imp$pcAC1tri1719
hlthDatRec.imp$imp$pcAC10wk1719
hlthDatRec.imp$imp$Episio1
hlthDatRec.imp$imp$Episio2
hlthDatRec.imp$imp$ApgarLT7
hlthDatRec.imp$imp$LabInd
hlthDatRec.imp$imp$caesar
hlthDatRec.imp$imp$nonInstrVagBirth
hlthDatRec.imp$imp$InstrVagBirth
hlthDatRec.imp$imp$genAnaesth
hlthDatRec.imp$imp$pcSmallBab40wk
hlthDatRec.imp$imp$tears1stB
hlthDatRec.imp$imp$tearsAllB

hlthDatRec.compl1 <- complete(hlthDatRec.imp, 1)
hlthDatRec.compl2 <- complete(hlthDatRec.imp, 2)
hlthDatRec.compl3 <- complete(hlthDatRec.imp, 3)
hlthDatRec.compl4 <- complete(hlthDatRec.imp, 4)
hlthDatRec.compl5 <- complete(hlthDatRec.imp, 5)
hlthDatRec.compl6 <- complete(hlthDatRec.imp, 6)
hlthDatRec.compl7 <- complete(hlthDatRec.imp, 7)
hlthDatRec.compl8 <- complete(hlthDatRec.imp, 8)
hlthDatRec.compl9 <- complete(hlthDatRec.imp, 9)
hlthDatRec.compl10 <- complete(hlthDatRec.imp, 10)
hlthDatRec.compl11 <- complete(hlthDatRec.imp, 11)
hlthDatRec.compl12 <- complete(hlthDatRec.imp, 12)
hlthDatRec.compl13 <- complete(hlthDatRec.imp, 13)
hlthDatRec.compl14 <- complete(hlthDatRec.imp, 14)
hlthDatRec.compl15 <- complete(hlthDatRec.imp, 15)
hlthDatRec.compl16 <- complete(hlthDatRec.imp, 16)
hlthDatRec.compl17 <- complete(hlthDatRec.imp, 17)
hlthDatRec.compl18 <- complete(hlthDatRec.imp, 18)
hlthDatRec.compl19 <- complete(hlthDatRec.imp, 19)
hlthDatRec.compl20 <- complete(hlthDatRec.imp, 20)

hlthDatRec.compl.mn <- hlthDatRec
sub1 <- which(is.na(hlthDatRec[,1]) == T)
dat1 <- data.frame(hlthDatRec.compl1[sub1, 1], hlthDatRec.compl2[sub1, 1], hlthDatRec.compl3[sub1, 1], hlthDatRec.compl4[sub1, 1],
                   hlthDatRec.compl5[sub1, 1], hlthDatRec.compl6[sub1, 1], hlthDatRec.compl7[sub1, 1], hlthDatRec.compl8[sub1, 1],
                   hlthDatRec.compl9[sub1, 1], hlthDatRec.compl10[sub1, 1], hlthDatRec.compl11[sub1, 1], hlthDatRec.compl12[sub1, 1],
                   hlthDatRec.compl13[sub1, 1], hlthDatRec.compl14[sub1, 1], hlthDatRec.compl15[sub1, 1], hlthDatRec.compl16[sub1, 1],
                   hlthDatRec.compl17[sub1, 1], hlthDatRec.compl18[sub1, 1], hlthDatRec.compl19[sub1, 1], hlthDatRec.compl20[sub1, 1])
hlthDatRec.compl.mn[sub1, 1] <- apply(dat1, MARGIN=1, mean)

sub2 <- which(is.na(hlthDatRec[,2]) == T)
dat2 <- data.frame(hlthDatRec.compl1[sub2, 2], hlthDatRec.compl2[sub2, 2], hlthDatRec.compl3[sub2, 2], hlthDatRec.compl4[sub2, 2],
                   hlthDatRec.compl5[sub2, 2], hlthDatRec.compl6[sub2, 2], hlthDatRec.compl7[sub2, 2], hlthDatRec.compl8[sub2, 2],
                   hlthDatRec.compl9[sub2, 2], hlthDatRec.compl10[sub2, 2], hlthDatRec.compl11[sub2, 2], hlthDatRec.compl12[sub2, 2],
                   hlthDatRec.compl13[sub2, 2], hlthDatRec.compl14[sub2, 2], hlthDatRec.compl15[sub2, 2], hlthDatRec.compl16[sub2, 2],
                   hlthDatRec.compl17[sub2, 2], hlthDatRec.compl18[sub2, 2], hlthDatRec.compl19[sub2, 2], hlthDatRec.compl20[sub2, 2])
hlthDatRec.compl.mn[sub2, 2] <- apply(dat2, MARGIN=1, mean)

sub3 <- which(is.na(hlthDatRec[,3]) == T)
dat3 <- data.frame(hlthDatRec.compl1[sub3, 3], hlthDatRec.compl2[sub3, 3], hlthDatRec.compl3[sub3, 3], hlthDatRec.compl4[sub3, 3],
                   hlthDatRec.compl5[sub3, 3], hlthDatRec.compl6[sub3, 3], hlthDatRec.compl7[sub3, 3], hlthDatRec.compl8[sub3, 3],
                   hlthDatRec.compl9[sub3, 3], hlthDatRec.compl10[sub3, 3], hlthDatRec.compl11[sub3, 3], hlthDatRec.compl12[sub3, 3],
                   hlthDatRec.compl13[sub3, 3], hlthDatRec.compl14[sub3, 3], hlthDatRec.compl15[sub3, 3], hlthDatRec.compl16[sub3, 3],
                   hlthDatRec.compl17[sub3, 3], hlthDatRec.compl18[sub3, 3], hlthDatRec.compl19[sub3, 3], hlthDatRec.compl20[sub3, 3])
hlthDatRec.compl.mn[sub3, 3] <- apply(dat3, MARGIN=1, mean)

sub4 <- which(is.na(hlthDatRec[,4]) == T)
dat4 <- data.frame(hlthDatRec.compl1[sub4, 4], hlthDatRec.compl2[sub4, 4], hlthDatRec.compl3[sub4, 4], hlthDatRec.compl4[sub4, 4],
                   hlthDatRec.compl5[sub4, 4], hlthDatRec.compl6[sub4, 4], hlthDatRec.compl7[sub4, 4], hlthDatRec.compl8[sub4, 4],
                   hlthDatRec.compl9[sub4, 4], hlthDatRec.compl10[sub4, 4], hlthDatRec.compl11[sub4, 4], hlthDatRec.compl12[sub4, 4],
                   hlthDatRec.compl13[sub4, 4], hlthDatRec.compl14[sub4, 4], hlthDatRec.compl15[sub4, 4], hlthDatRec.compl16[sub4, 4],
                   hlthDatRec.compl17[sub4, 4], hlthDatRec.compl18[sub4, 4], hlthDatRec.compl19[sub4, 4], hlthDatRec.compl20[sub4, 4])
hlthDatRec.compl.mn[sub4, 4] <- apply(dat4, MARGIN=1, mean)

sub5 <- which(is.na(hlthDatRec[,5]) == T)
dat5 <- data.frame(hlthDatRec.compl1[sub5, 5], hlthDatRec.compl2[sub5, 5], hlthDatRec.compl3[sub5, 5], hlthDatRec.compl4[sub5, 5],
                   hlthDatRec.compl5[sub5, 5], hlthDatRec.compl6[sub5, 5], hlthDatRec.compl7[sub5, 5], hlthDatRec.compl8[sub5, 5],
                   hlthDatRec.compl9[sub5, 5], hlthDatRec.compl10[sub5, 5], hlthDatRec.compl11[sub5, 5], hlthDatRec.compl12[sub5, 5],
                   hlthDatRec.compl13[sub5, 5], hlthDatRec.compl14[sub5, 5], hlthDatRec.compl15[sub5, 5], hlthDatRec.compl16[sub5, 5],
                   hlthDatRec.compl17[sub5, 5], hlthDatRec.compl18[sub5, 5], hlthDatRec.compl19[sub5, 5], hlthDatRec.compl20[sub5, 5])
hlthDatRec.compl.mn[sub5, 5] <- apply(dat5, MARGIN=1, mean)

sub6 <- which(is.na(hlthDatRec[,6]) == T)
dat6 <- data.frame(hlthDatRec.compl1[sub6, 6], hlthDatRec.compl2[sub6, 6], hlthDatRec.compl3[sub6, 6], hlthDatRec.compl4[sub6, 6],
                   hlthDatRec.compl5[sub6, 6], hlthDatRec.compl6[sub6, 6], hlthDatRec.compl7[sub6, 6], hlthDatRec.compl8[sub6, 6],
                   hlthDatRec.compl9[sub6, 6], hlthDatRec.compl10[sub6, 6], hlthDatRec.compl11[sub6, 6], hlthDatRec.compl12[sub6, 6],
                   hlthDatRec.compl13[sub6, 6], hlthDatRec.compl14[sub6, 6], hlthDatRec.compl15[sub6, 6], hlthDatRec.compl16[sub6, 6],
                   hlthDatRec.compl17[sub6, 6], hlthDatRec.compl18[sub6, 6], hlthDatRec.compl19[sub6, 6], hlthDatRec.compl20[sub6, 6])
hlthDatRec.compl.mn[sub6, 6] <- apply(dat6, MARGIN=1, mean)

sub7 <- which(is.na(hlthDatRec[,7]) == T)
dat7 <- data.frame(hlthDatRec.compl1[sub7, 7], hlthDatRec.compl2[sub7, 7], hlthDatRec.compl3[sub7, 7], hlthDatRec.compl4[sub7, 7],
                   hlthDatRec.compl5[sub7, 7], hlthDatRec.compl6[sub7, 7], hlthDatRec.compl7[sub7, 7], hlthDatRec.compl8[sub7, 7],
                   hlthDatRec.compl9[sub7, 7], hlthDatRec.compl10[sub7, 7], hlthDatRec.compl11[sub7, 7], hlthDatRec.compl12[sub7, 7],
                   hlthDatRec.compl13[sub7, 7], hlthDatRec.compl14[sub7, 7], hlthDatRec.compl15[sub7, 7], hlthDatRec.compl16[sub7, 7],
                   hlthDatRec.compl17[sub7, 7], hlthDatRec.compl18[sub7, 7], hlthDatRec.compl19[sub7, 7], hlthDatRec.compl20[sub7, 7])
hlthDatRec.compl.mn[sub7, 7] <- apply(dat7, MARGIN=1, mean)

sub8 <- which(is.na(hlthDatRec[,8]) == T)
dat8 <- data.frame(hlthDatRec.compl1[sub8, 8], hlthDatRec.compl2[sub8, 8], hlthDatRec.compl3[sub8, 8], hlthDatRec.compl4[sub8, 8],
                   hlthDatRec.compl5[sub8, 8], hlthDatRec.compl6[sub8, 8], hlthDatRec.compl7[sub8, 8], hlthDatRec.compl8[sub8, 8],
                   hlthDatRec.compl9[sub8, 8], hlthDatRec.compl10[sub8, 8], hlthDatRec.compl11[sub8, 8], hlthDatRec.compl12[sub8, 8],
                   hlthDatRec.compl13[sub8, 8], hlthDatRec.compl14[sub8, 8], hlthDatRec.compl15[sub8, 8], hlthDatRec.compl16[sub8, 8],
                   hlthDatRec.compl17[sub8, 8], hlthDatRec.compl18[sub8, 8], hlthDatRec.compl19[sub8, 8], hlthDatRec.compl20[sub8, 8])
hlthDatRec.compl.mn[sub8, 8] <- apply(dat8, MARGIN=1, mean)

sub9 <- which(is.na(hlthDatRec[,9]) == T)
dat9 <- data.frame(hlthDatRec.compl1[sub9, 9], hlthDatRec.compl2[sub9, 9], hlthDatRec.compl3[sub9, 9], hlthDatRec.compl4[sub9, 9],
                   hlthDatRec.compl5[sub9, 9], hlthDatRec.compl6[sub9, 9], hlthDatRec.compl7[sub9, 9], hlthDatRec.compl8[sub9, 9],
                   hlthDatRec.compl9[sub9, 9], hlthDatRec.compl10[sub9, 9], hlthDatRec.compl11[sub9, 9], hlthDatRec.compl12[sub9, 9],
                   hlthDatRec.compl13[sub9, 9], hlthDatRec.compl14[sub9, 9], hlthDatRec.compl15[sub9, 9], hlthDatRec.compl16[sub9, 9],
                   hlthDatRec.compl17[sub9, 9], hlthDatRec.compl18[sub9, 9], hlthDatRec.compl19[sub9, 9], hlthDatRec.compl20[sub9, 9])
hlthDatRec.compl.mn[sub9, 9] <- apply(dat9, MARGIN=1, mean)

sub10 <- which(is.na(hlthDatRec[,10]) == T)
dat10 <- data.frame(hlthDatRec.compl1[sub10, 10], hlthDatRec.compl2[sub10, 10], hlthDatRec.compl3[sub10, 10], hlthDatRec.compl4[sub10, 10],
                   hlthDatRec.compl5[sub10, 10], hlthDatRec.compl6[sub10, 10], hlthDatRec.compl7[sub10, 10], hlthDatRec.compl8[sub10, 10],
                   hlthDatRec.compl9[sub10, 10], hlthDatRec.compl10[sub10, 10], hlthDatRec.compl11[sub10, 10], hlthDatRec.compl12[sub10, 10],
                   hlthDatRec.compl13[sub10, 10], hlthDatRec.compl14[sub10, 10], hlthDatRec.compl15[sub10, 10], hlthDatRec.compl16[sub10, 10],
                   hlthDatRec.compl17[sub10, 10], hlthDatRec.compl18[sub10, 10], hlthDatRec.compl19[sub10, 10], hlthDatRec.compl20[sub10, 10])
hlthDatRec.compl.mn[sub10, 10] <- apply(dat10, MARGIN=1, mean)

sub11 <- which(is.na(hlthDatRec[,11]) == T)
dat11 <- data.frame(hlthDatRec.compl1[sub11, 11], hlthDatRec.compl2[sub11, 11], hlthDatRec.compl3[sub11, 11], hlthDatRec.compl4[sub11, 11],
                    hlthDatRec.compl5[sub11, 11], hlthDatRec.compl6[sub11, 11], hlthDatRec.compl7[sub11, 11], hlthDatRec.compl8[sub11, 11],
                    hlthDatRec.compl9[sub11, 11], hlthDatRec.compl10[sub11, 11], hlthDatRec.compl11[sub11, 11], hlthDatRec.compl12[sub11, 11],
                    hlthDatRec.compl13[sub11, 11], hlthDatRec.compl14[sub11, 11], hlthDatRec.compl15[sub11, 11], hlthDatRec.compl16[sub11, 11],
                    hlthDatRec.compl17[sub11, 11], hlthDatRec.compl18[sub11, 11], hlthDatRec.compl19[sub11, 11], hlthDatRec.compl20[sub11, 11])
hlthDatRec.compl.mn[sub11, 11] <- apply(dat11, MARGIN=1, mean)

sub12 <- which(is.na(hlthDatRec[,12]) == T)
dat12 <- data.frame(hlthDatRec.compl1[sub12, 12], hlthDatRec.compl2[sub12, 12], hlthDatRec.compl3[sub12, 12], hlthDatRec.compl4[sub12, 12],
                    hlthDatRec.compl5[sub12, 12], hlthDatRec.compl6[sub12, 12], hlthDatRec.compl7[sub12, 12], hlthDatRec.compl8[sub12, 12],
                    hlthDatRec.compl9[sub12, 12], hlthDatRec.compl10[sub12, 12], hlthDatRec.compl11[sub12, 12], hlthDatRec.compl12[sub12, 12],
                    hlthDatRec.compl13[sub12, 12], hlthDatRec.compl14[sub12, 12], hlthDatRec.compl15[sub12, 12], hlthDatRec.compl16[sub12, 12],
                    hlthDatRec.compl17[sub12, 12], hlthDatRec.compl18[sub12, 12], hlthDatRec.compl19[sub12, 12], hlthDatRec.compl20[sub12, 12])
hlthDatRec.compl.mn[sub12, 12] <- apply(dat12, MARGIN=1, mean)

sub13 <- which(is.na(hlthDatRec[,13]) == T)
dat13 <- data.frame(hlthDatRec.compl1[sub13, 13], hlthDatRec.compl2[sub13, 13], hlthDatRec.compl3[sub13, 13], hlthDatRec.compl4[sub13, 13],
                    hlthDatRec.compl5[sub13, 13], hlthDatRec.compl6[sub13, 13], hlthDatRec.compl7[sub13, 13], hlthDatRec.compl8[sub13, 13],
                    hlthDatRec.compl9[sub13, 13], hlthDatRec.compl10[sub13, 13], hlthDatRec.compl11[sub13, 13], hlthDatRec.compl12[sub13, 13],
                    hlthDatRec.compl13[sub13, 13], hlthDatRec.compl14[sub13, 13], hlthDatRec.compl15[sub13, 13], hlthDatRec.compl16[sub13, 13],
                    hlthDatRec.compl17[sub13, 13], hlthDatRec.compl18[sub13, 13], hlthDatRec.compl19[sub13, 13], hlthDatRec.compl20[sub13, 13])
hlthDatRec.compl.mn[sub13, 13] <- apply(dat13, MARGIN=1, mean)

sub14 <- which(is.na(hlthDatRec[,14]) == T)
dat14 <- data.frame(hlthDatRec.compl1[sub14, 14], hlthDatRec.compl2[sub14, 14], hlthDatRec.compl3[sub14, 14], hlthDatRec.compl4[sub14, 14],
                    hlthDatRec.compl5[sub14, 14], hlthDatRec.compl6[sub14, 14], hlthDatRec.compl7[sub14, 14], hlthDatRec.compl8[sub14, 14],
                    hlthDatRec.compl9[sub14, 14], hlthDatRec.compl10[sub14, 14], hlthDatRec.compl11[sub14, 14], hlthDatRec.compl12[sub14, 14],
                    hlthDatRec.compl13[sub14, 14], hlthDatRec.compl14[sub14, 14], hlthDatRec.compl15[sub14, 14], hlthDatRec.compl16[sub14, 14],
                    hlthDatRec.compl17[sub14, 14], hlthDatRec.compl18[sub14, 14], hlthDatRec.compl19[sub14, 14], hlthDatRec.compl20[sub14, 14])
hlthDatRec.compl.mn[sub14, 14] <- apply(dat14, MARGIN=1, mean)

sub15 <- which(is.na(hlthDatRec[,15]) == T)
dat15 <- data.frame(hlthDatRec.compl1[sub15, 15], hlthDatRec.compl2[sub15, 15], hlthDatRec.compl3[sub15, 15], hlthDatRec.compl4[sub15, 15],
                    hlthDatRec.compl5[sub15, 15], hlthDatRec.compl6[sub15, 15], hlthDatRec.compl7[sub15, 15], hlthDatRec.compl8[sub15, 15],
                    hlthDatRec.compl9[sub15, 15], hlthDatRec.compl10[sub15, 15], hlthDatRec.compl11[sub15, 15], hlthDatRec.compl12[sub15, 15],
                    hlthDatRec.compl13[sub15, 15], hlthDatRec.compl14[sub15, 15], hlthDatRec.compl15[sub15, 15], hlthDatRec.compl16[sub15, 15],
                    hlthDatRec.compl17[sub15, 15], hlthDatRec.compl18[sub15, 15], hlthDatRec.compl19[sub15, 15], hlthDatRec.compl20[sub15, 15])
hlthDatRec.compl.mn[sub15, 15] <- apply(dat15, MARGIN=1, mean)

sub16 <- which(is.na(hlthDatRec[,16]) == T)
dat16 <- data.frame(hlthDatRec.compl1[sub16, 16], hlthDatRec.compl2[sub16, 16], hlthDatRec.compl3[sub16, 16], hlthDatRec.compl4[sub16, 16],
                    hlthDatRec.compl5[sub16, 16], hlthDatRec.compl6[sub16, 16], hlthDatRec.compl7[sub16, 16], hlthDatRec.compl8[sub16, 16],
                    hlthDatRec.compl9[sub16, 16], hlthDatRec.compl10[sub16, 16], hlthDatRec.compl11[sub16, 16], hlthDatRec.compl12[sub16, 16],
                    hlthDatRec.compl13[sub16, 16], hlthDatRec.compl14[sub16, 16], hlthDatRec.compl15[sub16, 16], hlthDatRec.compl16[sub16, 16],
                    hlthDatRec.compl17[sub16, 16], hlthDatRec.compl18[sub16, 16], hlthDatRec.compl19[sub16, 16], hlthDatRec.compl20[sub16, 16])
hlthDatRec.compl.mn[sub16, 16] <- apply(dat16, MARGIN=1, mean)

sub17 <- which(is.na(hlthDatRec[,17]) == T)
dat17 <- data.frame(hlthDatRec.compl1[sub17, 17], hlthDatRec.compl2[sub17, 17], hlthDatRec.compl3[sub17, 17], hlthDatRec.compl4[sub17, 17],
                    hlthDatRec.compl5[sub17, 17], hlthDatRec.compl6[sub17, 17], hlthDatRec.compl7[sub17, 17], hlthDatRec.compl8[sub17, 17],
                    hlthDatRec.compl9[sub17, 17], hlthDatRec.compl10[sub17, 17], hlthDatRec.compl11[sub17, 17], hlthDatRec.compl12[sub17, 17],
                    hlthDatRec.compl13[sub17, 17], hlthDatRec.compl14[sub17, 17], hlthDatRec.compl15[sub17, 17], hlthDatRec.compl16[sub17, 17],
                    hlthDatRec.compl17[sub17, 17], hlthDatRec.compl18[sub17, 17], hlthDatRec.compl19[sub17, 17], hlthDatRec.compl20[sub17, 17])
hlthDatRec.compl.mn[sub17, 17] <- apply(dat17, MARGIN=1, mean)

sub18 <- which(is.na(hlthDatRec[,18]) == T)
dat18 <- data.frame(hlthDatRec.compl1[sub18, 18], hlthDatRec.compl2[sub18, 18], hlthDatRec.compl3[sub18, 18], hlthDatRec.compl4[sub18, 18],
                    hlthDatRec.compl5[sub18, 18], hlthDatRec.compl6[sub18, 18], hlthDatRec.compl7[sub18, 18], hlthDatRec.compl8[sub18, 18],
                    hlthDatRec.compl9[sub18, 18], hlthDatRec.compl10[sub18, 18], hlthDatRec.compl11[sub18, 18], hlthDatRec.compl12[sub18, 18],
                    hlthDatRec.compl13[sub18, 18], hlthDatRec.compl14[sub18, 18], hlthDatRec.compl15[sub18, 18], hlthDatRec.compl16[sub18, 18],
                    hlthDatRec.compl17[sub18, 18], hlthDatRec.compl18[sub18, 18], hlthDatRec.compl19[sub18, 18], hlthDatRec.compl20[sub18, 18])
hlthDatRec.compl.mn[sub18, 18] <- apply(dat18, MARGIN=1, mean)

sub19 <- which(is.na(hlthDatRec[,19]) == T)
dat19 <- data.frame(hlthDatRec.compl1[sub19, 19], hlthDatRec.compl2[sub19, 19], hlthDatRec.compl3[sub19, 19], hlthDatRec.compl4[sub19, 19],
                    hlthDatRec.compl5[sub19, 19], hlthDatRec.compl6[sub19, 19], hlthDatRec.compl7[sub19, 19], hlthDatRec.compl8[sub19, 19],
                    hlthDatRec.compl9[sub19, 19], hlthDatRec.compl10[sub19, 19], hlthDatRec.compl11[sub19, 19], hlthDatRec.compl12[sub19, 19],
                    hlthDatRec.compl13[sub19, 19], hlthDatRec.compl14[sub19, 19], hlthDatRec.compl15[sub19, 19], hlthDatRec.compl16[sub19, 19],
                    hlthDatRec.compl17[sub19, 19], hlthDatRec.compl18[sub19, 19], hlthDatRec.compl19[sub19, 19], hlthDatRec.compl20[sub19, 19])
hlthDatRec.compl.mn[sub19, 19] <- apply(dat19, MARGIN=1, mean)

sub20 <- which(is.na(hlthDatRec[,20]) == T)
dat20 <- data.frame(hlthDatRec.compl1[sub20, 20], hlthDatRec.compl2[sub20, 20], hlthDatRec.compl3[sub20, 20], hlthDatRec.compl4[sub20, 20],
                    hlthDatRec.compl5[sub20, 20], hlthDatRec.compl6[sub20, 20], hlthDatRec.compl7[sub20, 20], hlthDatRec.compl8[sub20, 20],
                    hlthDatRec.compl9[sub20, 20], hlthDatRec.compl10[sub20, 20], hlthDatRec.compl11[sub20, 20], hlthDatRec.compl12[sub20, 20],
                    hlthDatRec.compl13[sub20, 20], hlthDatRec.compl14[sub20, 20], hlthDatRec.compl15[sub20, 20], hlthDatRec.compl16[sub20, 20],
                    hlthDatRec.compl17[sub20, 20], hlthDatRec.compl18[sub20, 20], hlthDatRec.compl19[sub20, 20], hlthDatRec.compl20[sub20, 20])
hlthDatRec.compl.mn[sub20, 20] <- apply(dat20, MARGIN=1, mean)

dim(hlthDatRec.compl.mn)
dim(dat)

## create full dataset (means only) with scaled values
ldthInfYC1416 <- ifelse(hlthDatRec.compl.mn$dthInfYC1416 == 0, log10(1), log10(hlthDatRec.compl.mn$dthInfYC1416))
lpcLObw1416 <- logit(hlthDatRec.compl.mn$pcLObw1416/100)
lApgarLT7 <- logit(hlthDatRec.compl.mn$ApgarLT7/100)
fullDatMean <- data.frame(ldthInfYC1416, lpcLObw1416, lApgarLT7, log10(dat$popD), 
                          dat$HSEdat_HSw, log10(dat$HSEdat_Inc), dat$MAXTEMP_me, dat$MINT_mean, dat$ISOTHERM_2, log10(dat$PRCP_mean))
colnames(fullDatMean) <- c("ldthInfYC1416", "lpcLObw1416", "lApgarLT7", "lPopD", "HSw", "lInc", "maxTwM", "minTcM", "isothrm", "lPrcpWm")
fullDatMeanSc <- as.data.frame(scale(fullDatMean, center=T, scale=T))
head(fullDatMeanSc)

save.image("hlthClimAnalysis.RData")

## test bivariate relationships
# infant/child mortality
plot(fullDatMeanSc$lPopD, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$HSw, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$lInc, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$maxTwM, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$minTcM, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$isothrm, fullDatMeanSc$ldthInfYC1416, pch=19)
plot(fullDatMeanSc$lPrcpWm, fullDatMeanSc$ldthInfYC1416, pch=19)

# low birth weight
plot(fullDatMeanSc$lPopD, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$HSw, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$lInc, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$maxTwM, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$minTcM, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$isothrm, fullDatMeanSc$lpcLObw1416, pch=19)
plot(fullDatMeanSc$lPrcpWm, fullDatMeanSc$lpcLObw1416, pch=19)

# low Apgar
plot(fullDatMeanSc$lPopD, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$HSw, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$lInc, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$maxTwM, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$minTcM, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$isothrm, fullDatMeanSc$lApgarLT7, pch=19)
plot(fullDatMeanSc$lPrcpWm, fullDatMeanSc$lApgarLT7, pch=19)

head(fullDatMeanSc)
### boosted regression tree of mean climate conditions
## infant and child mortality
brt.fit.mort <- gbm.step(fullDatMeanSc, gbm.x = attr(fullDatMeanSc, "names")[c(4:9)], gbm.y = attr(fullDatMeanSc, "names")[1], family="gaussian", max.trees=100000, tolerance = 0.0001, learning.rate = 0.0001, bag.fraction=0.75, tree.complexity = 2)
summary(brt.fit.mort)
D2 <- 100 * (brt.fit.mort$cv.statistics$deviance.mean - brt.fit.mort$self.statistics$mean.resid) / brt.fit.mort$cv.statistics$deviance.mean
D2 # % deviance explained
gbm.plot(brt.fit.mort)
gbm.plot.fits(brt.fit.mort)

brt.CV.cor <- 100 * brt.fit.mort$cv.statistics$correlation.mean
brt.CV.cor
brt.CV.cor.se <- 100 * brt.fit.mort$cv.statistics$correlation.se
brt.CV.cor.se
print(c(brt.CV.cor, brt.CV.cor.se))

## low birth weight
brt.fit.bw <- gbm.step(fullDatMeanSc, gbm.x = attr(fullDatMeanSc, "names")[c(4:9)], gbm.y = attr(fullDatMeanSc, "names")[2], family="gaussian", max.trees=100000, tolerance = 0.0001, learning.rate = 0.001, bag.fraction=0.75, tree.complexity = 2)
summary(brt.fit.bw)
D2 <- 100 * (brt.fit.bw$cv.statistics$deviance.mean - brt.fit.bw$self.statistics$mean.resid) / brt.fit.bw$cv.statistics$deviance.mean
D2 # % deviance explained
gbm.plot(brt.fit.bw)
gbm.plot.fits(brt.fit.bw)

brt.CV.cor <- 100 * brt.fit.bw$cv.statistics$correlation.mean
brt.CV.cor
brt.CV.cor.se <- 100 * brt.fit.bw$cv.statistics$correlation.se
brt.CV.cor.se
print(c(brt.CV.cor, brt.CV.cor.se))

## low Apgar
brt.fit.apg <- gbm.step(fullDatMeanSc, gbm.x = attr(fullDatMeanSc, "names")[c(4:9)], gbm.y = attr(fullDatMeanSc, "names")[3], family="gaussian", max.trees=100000, tolerance = 0.00001, learning.rate = 0.0003, bag.fraction=0.75, tree.complexity = 2)
summary(brt.fit.apg)
D2 <- 100 * (brt.fit.apg$cv.statistics$deviance.mean - brt.fit.apg$self.statistics$mean.resid) / brt.fit.apg$cv.statistics$deviance.mean
D2 # % deviance explained
gbm.plot(brt.fit.apg)
gbm.plot.fits(brt.fit.apg)

brt.CV.cor <- 100 * brt.fit.apg$cv.statistics$correlation.mean
brt.CV.cor
brt.CV.cor.se <- 100 * brt.fit.apg$cv.statistics$correlation.se
brt.CV.cor.se
print(c(brt.CV.cor, brt.CV.cor.se))


## create dataset with SDs (linear scale; transform later after resampling)
fullDatSDlinear <- data.frame(hlthDatRec.compl.mn$dthInfYC1416, hlthDatRec.compl.mn$pcLObw1416, hlthDatRec.compl.mn$ApgarLT7, dat$popD, 
                          dat$HSEdat_HSw, dat$HSEdat_Inc, dat$MAXTEMP_me, dat$MAXTEMP_st, dat$MINT_mean, dat$MINT_stdde,
                          dat$ISOTHERM_2, dat$ISOTHERM_4, dat$PRCP_mean, dat$PRCP_stdde)
colnames(fullDatSDlinear) <- c("dthInfYC1416", "pcLObw1416", "ApgarLT7", "PopD", "HSw", "Inc", "maxTwMMn", "maxTwMSd",
                               "minTcMMn", "minTcMSd", "isothrmMn", "isothrmSd", "PrcpWmMn", "PrcpWmSd")

head(fullDatSDlinear[,c(5:7,9,11,13)])

## correlation matrix of raw values
cor.dat <- fullDatSDlinear[,c(5:7,9,11,13)]
cormat <- cor(na.omit(cor.dat), method="kendall")
cormat[lower.tri(cormat)] <- NA
cormat

## correlation matrix of scaled values
cor.dat2 <- as.data.frame(scale(fullDatSDlinear[,c(5:7,9,11,13)], center=T, scale=T))
cormat2 <- cor(na.omit(cor.dat2), method="kendall")
cormat2[lower.tri(cormat2)] <- NA
cormat2


#############
## iterate ##
#############
biter <- 1000
eq.sp.points <- 100
sample.size <- 0.5 # 0 to 1

# create storage arrays
val.mort.arr <- pred.mort.arr <- val.bw.arr <- pred.bw.arr <- val.apg.arr <- pred.apg.arr <- 
  array(data = NA, dim = c(eq.sp.points, length(c(4:9)), biter), dimnames=list(paste("x",1:eq.sp.points,sep=""), 
  colnames(fullDatSDlinear[,c(5:7,9,11,13)]), paste("b",1:biter,sep="")))

# create storage vectors
D2.mort.vec <- CV.cor.mort.vec <- CV.cor.se.mort.vec <- D2.bw.vec <- CV.cor.bw.vec <- CV.cor.se.bw.vec <-
  D2.apg.vec <- CV.cor.apg.vec <- CV.cor.se.apg.vec <- HSw.mort.ri <- Inc.mort.ri <- Tmax.mort.ri <- Tmin.mort.ri <- 
  isothrm.mort.ri <- prcp.mort.ri <- HSw.bw.ri <- Inc.bw.ri <- Tmax.bw.ri <- Tmin.bw.ri <- isothrm.bw.ri <- prcp.bw.ri <-
  HSw.apg.ri <- Inc.apg.ri <- Tmax.apg.ri <- Tmin.apg.ri <- isothrm.apg.ri <- prcp.apg.ri <- rep(NA,biter)

# b loop
for (b in 1:biter) {
  
  ## resample rows without replacement
  rand.sub <- sort(sample(1:dim(fullDatSDlinear)[1], round(sample.size*dim(fullDatSDlinear)[1]), replace=F))
  rand.dat <- fullDatSDlinear[rand.sub,]
  
  # max temperature (Normal)
  rand.dat$maxTwM.it <- rnorm(dim(rand.dat)[1], rand.dat$maxTwMMn, rand.dat$maxTwMSd)
  
  # min temperature (Normal)
  rand.dat$minTcM.it <- rnorm(dim(rand.dat)[1], rand.dat$minTcMMn, rand.dat$minTcMSd)
  
  # isothermality (Beta)
  isothrm.alpha <- estBetaParams(rand.dat$isothrmMn/100, (rand.dat$isothrmSd/100)^2)$alpha
  isothrm.beta <- estBetaParams(rand.dat$isothrmMn/100, (rand.dat$isothrmSd/100)^2)$beta
  rand.dat$isothrm.it <- rbeta(dim(rand.dat)[1], isothrm.alpha, isothrm.beta)
  #rand.dat$isothrm.it
  
  # precipitation (Normal)
  rand.dat$PrcpWm.it <- rnorm(dim(rand.dat)[1], rand.dat$PrcpWmMn, rand.dat$PrcpWmSd)
  
  # transform
  rand.dat$ldthInfYC1416 <- ifelse(rand.dat$dthInfYC1416 == 0, log10(1), log10(rand.dat$dthInfYC1416))
  rand.dat$lpcLObw1416 <- logit(rand.dat$pcLObw1416/100)
  rand.dat$lApgarLT7 <- logit(rand.dat$ApgarLT7/100)
  rand.dat$lPopD <- ifelse(is.nan(log10(rand.dat$PopD)) == F, log10(rand.dat$PopD), NA)
  rand.dat$lInc <- ifelse(is.nan(log10(rand.dat$Inc)) == F, log10(rand.dat$Inc), NA)
  rand.dat$lPrcpWm.it <- ifelse(is.nan(log10(rand.dat$PrcpWm.it)) == F, log10(rand.dat$PrcpWm.it), NA)
  
  # scale
  rand.datSC <- as.data.frame(scale(na.omit(rand.dat[,c(19:21,5,23,15,16,17,24)]), center=T, scale=T))

  ## boosted regression trees
  # infant/child mortality
  brt.fit.mort <- gbm.step(rand.datSC, gbm.x = attr(rand.datSC, "names")[c(4:9)], gbm.y = attr(rand.datSC, "names")[1], family="gaussian", max.trees=100000, tolerance = 0.001, learning.rate = 0.0003, bag.fraction=0.75, tree.complexity = 2, silent=T, tolerance.method = "auto")
  summ.fit.mort <- summary(brt.fit.mort)
  
  # low birth weight
  brt.fit.bw <- gbm.step(rand.datSC, gbm.x = attr(rand.datSC, "names")[c(4:9)], gbm.y = attr(rand.datSC, "names")[2], family="gaussian", max.trees=100000, tolerance = 0.0001, learning.rate = 0.002, bag.fraction=0.75, tree.complexity = 2, silent=T, tolerance.method = "auto")
  summ.fit.bw <- summary(brt.fit.bw)
  
  # low Apgar
  brt.fit.apg <- gbm.step(rand.datSC, gbm.x = attr(rand.datSC, "names")[c(4:9)], gbm.y = attr(rand.datSC, "names")[3], family="gaussian", max.trees=100000, tolerance = 0.0001, learning.rate = 0.0005, bag.fraction=0.75, tree.complexity = 2, silent=T, tolerance.method = "auto")
  summ.fit.apg <- summary(brt.fit.apg)
  
  ## variable relative importance
  # infant/child mortality
  HSw.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][1])]
  Inc.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][2])]
  Tmax.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][3])]
  Tmin.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][4])]
  isothrm.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][5])]
  prcp.mort.ri[b] <- summ.fit.mort$rel.inf[which(summ.fit.mort$var == attr(rand.datSC, "names")[4:9][6])]

  # low birth weight
  HSw.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][1])]
  Inc.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][2])]
  Tmax.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][3])]
  Tmin.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][4])]
  isothrm.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][5])]
  prcp.bw.ri[b] <- summ.fit.bw$rel.inf[which(summ.fit.bw$var == attr(rand.datSC, "names")[4:9][6])]

  # low Apgar
  HSw.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][1])]
  Inc.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][2])]
  Tmax.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][3])]
  Tmin.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][4])]
  isothrm.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][5])]
  prcp.apg.ri[b] <- summ.fit.apg$rel.inf[which(summ.fit.apg$var == attr(rand.datSC, "names")[4:9][6])]
  
  ## goodness of fit
  D2.mort.vec[b] <- 100 * (brt.fit.mort$cv.statistics$deviance.mean - brt.fit.mort$self.statistics$mean.resid) / brt.fit.mort$cv.statistics$deviance.mean
  D2.bw.vec[b] <- 100 * (brt.fit.bw$cv.statistics$deviance.mean - brt.fit.bw$self.statistics$mean.resid) / brt.fit.bw$cv.statistics$deviance.mean
  D2.apg.vec[b] <- 100 * (brt.fit.apg$cv.statistics$deviance.mean - brt.fit.apg$self.statistics$mean.resid) / brt.fit.apg$cv.statistics$deviance.mean
  
  CV.cor.mort.vec[b] <- 100 * brt.fit.mort$cv.statistics$correlation.mean
  CV.cor.se.mort.vec[b] <- 100 *brt.fit.mort$cv.statistics$correlation.se
  CV.cor.bw.vec[b] <- 100 * brt.fit.bw$cv.statistics$correlation.mean
  CV.cor.se.bw.vec[b] <- 100 *brt.fit.bw$cv.statistics$correlation.se
  CV.cor.apg.vec[b] <- 100 * brt.fit.apg$cv.statistics$correlation.mean
  CV.cor.se.apg.vec[b] <- 100 *brt.fit.apg$cv.statistics$correlation.se
  
  ## reponse curves
  RESP.val.mort <- RESP.pred.mort <- RESP.val.bw <- RESP.pred.bw <- RESP.val.apg <- RESP.pred.apg <-
    matrix(data=NA, nrow=eq.sp.points, ncol=length(c(4:9)))
  
  ## output average predictions
  for (p in 1:length(c(4:9))) {
    RESP.val.mort[,p] <- plot.gbm(brt.fit.mort, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,1]
    RESP.pred.mort[,p] <- plot.gbm(brt.fit.mort, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,2]
    RESP.val.bw[,p] <- plot.gbm(brt.fit.bw, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,1]
    RESP.pred.bw[,p] <- plot.gbm(brt.fit.bw, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,2]
    RESP.val.apg[,p] <- plot.gbm(brt.fit.apg, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,1]
    RESP.pred.apg[,p] <- plot.gbm(brt.fit.apg, i.var=p, continuous.resolution=eq.sp.points, return.grid=T)[,2]
  } # end p
  
  RESP.val.mort.dat <- as.data.frame(RESP.val.mort)
  colnames(RESP.val.mort.dat) <- brt.fit.mort$var.names
  RESP.pred.mort.dat <- as.data.frame(RESP.pred.mort)
  colnames(RESP.pred.mort.dat) <- brt.fit.mort$var.names

  RESP.val.bw.dat <- as.data.frame(RESP.val.bw)
  colnames(RESP.val.bw.dat) <- brt.fit.bw$var.names
  RESP.pred.bw.dat <- as.data.frame(RESP.pred.bw)
  colnames(RESP.pred.bw.dat) <- brt.fit.bw$var.names

  RESP.val.apg.dat <- as.data.frame(RESP.val.apg)
  colnames(RESP.val.apg.dat) <- brt.fit.apg$var.names
  RESP.pred.apg.dat <- as.data.frame(RESP.pred.apg)
  colnames(RESP.pred.apg.dat) <- brt.fit.apg$var.names
  
  val.mort.arr[, , b] <- as.matrix(RESP.val.mort.dat)
  pred.mort.arr[, , b] <- as.matrix(RESP.pred.mort.dat)

  val.bw.arr[, , b] <- as.matrix(RESP.val.bw.dat)
  pred.bw.arr[, , b] <- as.matrix(RESP.pred.bw.dat)

  val.apg.arr[, , b] <- as.matrix(RESP.val.apg.dat)
  pred.apg.arr[, , b] <- as.matrix(RESP.pred.apg.dat)
  
  print(b)
  
} # end b

# kappa method to reduce effects of outliers on bootstrap estimates
kappa <- 2
kappa.n <- length(c(4:9))
pred.mort.update <- pred.mort.arr[,,1:biter]
pred.bw.update <- pred.bw.arr[,,1:biter]
pred.apg.update <- pred.apg.arr[,,1:biter]

for (k in 1:kappa.n) {
  boot.mort.mean <- apply(pred.mort.update, MARGIN=c(1,2), mean, na.rm=T)
  boot.mort.sd <- apply(pred.mort.update, MARGIN=c(1,2), sd, na.rm=T)
  boot.bw.mean <- apply(pred.bw.update, MARGIN=c(1,2), mean, na.rm=T)
  boot.bw.sd <- apply(pred.bw.update, MARGIN=c(1,2), sd, na.rm=T)
  boot.apg.mean <- apply(pred.apg.update, MARGIN=c(1,2), mean, na.rm=T)
  boot.apg.sd <- apply(pred.apg.update, MARGIN=c(1,2), sd, na.rm=T)
  
  for (z in 1:biter) {
    pred.mort.update[,,z] <- ifelse((pred.mort.update[,,z] < (boot.mort.mean-kappa*boot.mort.sd) | pred.mort.update[,,z] >
                                       (boot.mort.mean+kappa*boot.mort.sd)), NA, pred.mort.update[,,z])
    pred.bw.update[,,z] <- ifelse((pred.bw.update[,,z] < (boot.bw.mean-kappa*boot.bw.sd) | pred.bw.update[,,z] >
                                       (boot.bw.mean+kappa*boot.bw.sd)), NA, pred.bw.update[,,z])
    pred.apg.update[,,z] <- ifelse((pred.apg.update[,,z] < (boot.apg.mean-kappa*boot.apg.sd) | pred.apg.update[,,z] >
                                       (boot.apg.mean+kappa*boot.apg.sd)), NA, pred.apg.update[,,z])
  }
  print(k)
} # end k

pred.mort.med <- apply(pred.mort.update, MARGIN=c(1,2), median, na.rm=T)
pred.mort.lo <- apply(pred.mort.update, MARGIN=c(1,2), quantile, probs=0.025, na.rm=T)
pred.mort.up <- apply(pred.mort.update, MARGIN=c(1,2), quantile, probs=0.975, na.rm=T)
val.mort.med <- apply(val.mort.arr[,,1:biter], MARGIN=c(1,2), median, na.rm=T)

pred.bw.med <- apply(pred.bw.update, MARGIN=c(1,2), median, na.rm=T)
pred.bw.lo <- apply(pred.bw.update, MARGIN=c(1,2), quantile, probs=0.025, na.rm=T)
pred.bw.up <- apply(pred.bw.update, MARGIN=c(1,2), quantile, probs=0.975, na.rm=T)
val.bw.med <- apply(val.bw.arr[,,1:biter], MARGIN=c(1,2), median, na.rm=T)

pred.apg.med <- apply(pred.apg.update, MARGIN=c(1,2), median, na.rm=T)
pred.apg.lo <- apply(pred.apg.update, MARGIN=c(1,2), quantile, probs=0.025, na.rm=T)
pred.apg.up <- apply(pred.apg.update, MARGIN=c(1,2), quantile, probs=0.975, na.rm=T)
val.apg.med <- apply(val.apg.arr[,,1:biter], MARGIN=c(1,2), median, na.rm=T)

# kappa method for output vectors
D2.mort.update <- D2.mort.vec[1:biter]
CV.cor.mort.update <- CV.cor.mort.vec[1:biter]
CV.cor.se.mort.update <- CV.cor.se.mort.vec[1:biter]

D2.bw.update <- D2.bw.vec[1:biter]
CV.cor.bw.update <- CV.cor.bw.vec[1:biter]
CV.cor.se.bw.update <- CV.cor.se.bw.vec[1:biter]

D2.apg.update <- D2.apg.vec[1:biter]
CV.cor.apg.update <- CV.cor.apg.vec[1:biter]
CV.cor.se.apg.update <- CV.cor.se.apg.vec[1:biter]

HSw.mort.ri.update <- HSw.mort.ri[1:biter]
Inc.mort.ri.update <- Inc.mort.ri[1:biter]
Tmax.mort.ri.update <- Tmax.mort.ri[1:biter]
Tmin.mort.ri.update <- Tmin.mort.ri[1:biter]
isothrm.mort.ri.update <- isothrm.mort.ri[1:biter]
prcp.mort.ri.update <- prcp.mort.ri[1:biter]

HSw.bw.ri.update <- HSw.bw.ri[1:biter]
Inc.bw.ri.update <- Inc.bw.ri[1:biter]
Tmax.bw.ri.update <- Tmax.bw.ri[1:biter]
Tmin.bw.ri.update <- Tmin.bw.ri[1:biter]
isothrm.bw.ri.update <- isothrm.bw.ri[1:biter]
prcp.bw.ri.update <- prcp.bw.ri[1:biter]

HSw.apg.ri.update <- HSw.apg.ri[1:biter]
Inc.apg.ri.update <- Inc.apg.ri[1:biter]
Tmax.apg.ri.update <- Tmax.apg.ri[1:biter]
Tmin.apg.ri.update <- Tmin.apg.ri[1:biter]
isothrm.apg.ri.update <- isothrm.apg.ri[1:biter]
prcp.apg.ri.update <- prcp.apg.ri[1:biter]

for (k in 1:kappa.n) {
  D2.mort.mean <- mean(D2.mort.update, na.rm=T); D2.sd.mort <- sd(D2.mort.update, na.rm=T)
  CV.cor.mort.mean <- mean(CV.cor.mort.update, na.rm=T); CV.cor.sd.mort <- sd(CV.cor.mort.update, na.rm=T)
  CV.cor.se.mort.mean <- mean(CV.cor.se.mort.update, na.rm=T); CV.cor.se.sd.mort <- sd(CV.cor.se.mort.update, na.rm=T)

  D2.bw.mean <- mean(D2.bw.update, na.rm=T); D2.sd.bw <- sd(D2.bw.update, na.rm=T)
  CV.cor.bw.mean <- mean(CV.cor.bw.update, na.rm=T); CV.cor.sd.bw <- sd(CV.cor.bw.update, na.rm=T)
  CV.cor.se.bw.mean <- mean(CV.cor.se.bw.update, na.rm=T); CV.cor.se.sd.bw <- sd(CV.cor.se.bw.update, na.rm=T)

  D2.apg.mean <- mean(D2.apg.update, na.rm=T); D2.sd.apg <- sd(D2.apg.update, na.rm=T)
  CV.cor.apg.mean <- mean(CV.cor.apg.update, na.rm=T); CV.cor.sd.apg <- sd(CV.cor.apg.update, na.rm=T)
  CV.cor.se.apg.mean <- mean(CV.cor.se.apg.update, na.rm=T); CV.cor.se.sd.apg <- sd(CV.cor.se.apg.update, na.rm=T)
  
  HSw.mort.mean <- mean(HSw.mort.ri.update, na.rm=T); HSw.sd.mort <- sd(HSw.mort.ri.update, na.rm=T)
  Inc.mort.mean <- mean(Inc.mort.ri.update, na.rm=T); Inc.sd.mort <- sd(Inc.mort.ri.update, na.rm=T)
  Tmax.mort.mean <- mean(Tmax.mort.ri.update, na.rm=T); Tmax.sd.mort <- sd(Tmax.mort.ri.update, na.rm=T)
  Tmin.mort.mean <- mean(Tmin.mort.ri.update, na.rm=T); Tmin.sd.mort <- sd(Tmin.mort.ri.update, na.rm=T)
  isothrm.mort.mean <- mean(isothrm.mort.ri.update, na.rm=T); isothrm.sd.mort <- sd(isothrm.mort.ri.update, na.rm=T)
  prcp.mort.mean <- mean(prcp.mort.ri.update, na.rm=T); prcp.sd.mort <- sd(prcp.mort.ri.update, na.rm=T)

  HSw.bw.mean <- mean(HSw.bw.ri.update, na.rm=T); HSw.sd.bw <- sd(HSw.bw.ri.update, na.rm=T)
  Inc.bw.mean <- mean(Inc.bw.ri.update, na.rm=T); Inc.sd.bw <- sd(Inc.bw.ri.update, na.rm=T)
  Tmax.bw.mean <- mean(Tmax.bw.ri.update, na.rm=T); Tmax.sd.bw <- sd(Tmax.bw.ri.update, na.rm=T)
  Tmin.bw.mean <- mean(Tmin.bw.ri.update, na.rm=T); Tmin.sd.bw <- sd(Tmin.bw.ri.update, na.rm=T)
  isothrm.bw.mean <- mean(isothrm.bw.ri.update, na.rm=T); isothrm.sd.bw <- sd(isothrm.bw.ri.update, na.rm=T)
  prcp.bw.mean <- mean(prcp.bw.ri.update, na.rm=T); prcp.sd.bw <- sd(prcp.bw.ri.update, na.rm=T)

  HSw.apg.mean <- mean(HSw.apg.ri.update, na.rm=T); HSw.sd.apg <- sd(HSw.apg.ri.update, na.rm=T)
  Inc.apg.mean <- mean(Inc.apg.ri.update, na.rm=T); Inc.sd.apg <- sd(Inc.apg.ri.update, na.rm=T)
  Tmax.apg.mean <- mean(Tmax.apg.ri.update, na.rm=T); Tmax.sd.apg <- sd(Tmax.apg.ri.update, na.rm=T)
  Tmin.apg.mean <- mean(Tmin.apg.ri.update, na.rm=T); Tmin.sd.apg <- sd(Tmin.apg.ri.update, na.rm=T)
  isothrm.apg.mean <- mean(isothrm.apg.ri.update, na.rm=T); isothrm.sd.apg <- sd(isothrm.apg.ri.update, na.rm=T)
  prcp.apg.mean <- mean(prcp.apg.ri.update, na.rm=T); prcp.sd.apg <- sd(prcp.apg.ri.update, na.rm=T)
  
  for (u in 1:biter) {
    D2.mort.update[u] <- ifelse((D2.mort.update[u] < (D2.mort.mean-kappa*D2.sd.mort) | D2.mort.update[u] > (D2.mort.mean+kappa*D2.sd.mort)), NA, D2.mort.update[u])
    CV.cor.mort.update[u] <- ifelse((CV.cor.mort.update[u] < (CV.cor.mort.mean-kappa*CV.cor.sd.mort) | CV.cor.mort.update[u] > (CV.cor.mort.mean+kappa*CV.cor.sd.mort)), NA, CV.cor.mort.update[u])
    CV.cor.se.mort.update[u] <- ifelse((CV.cor.se.mort.update[u] < (CV.cor.se.mort.mean-kappa*CV.cor.se.sd.mort) | CV.cor.se.mort.update[u] > (CV.cor.se.mort.mean+kappa*CV.cor.se.sd.mort)), NA, CV.cor.se.mort.update[u])
    
    D2.bw.update[u] <- ifelse((D2.bw.update[u] < (D2.bw.mean-kappa*D2.sd.bw) | D2.bw.update[u] > (D2.bw.mean+kappa*D2.sd.bw)), NA, D2.bw.update[u])
    CV.cor.bw.update[u] <- ifelse((CV.cor.bw.update[u] < (CV.cor.bw.mean-kappa*CV.cor.sd.bw) | CV.cor.bw.update[u] > (CV.cor.bw.mean+kappa*CV.cor.sd.bw)), NA, CV.cor.bw.update[u])
    CV.cor.se.bw.update[u] <- ifelse((CV.cor.se.bw.update[u] < (CV.cor.se.bw.mean-kappa*CV.cor.se.sd.bw) | CV.cor.se.bw.update[u] > (CV.cor.se.bw.mean+kappa*CV.cor.se.sd.bw)), NA, CV.cor.se.bw.update[u])

    D2.apg.update[u] <- ifelse((D2.apg.update[u] < (D2.apg.mean-kappa*D2.sd.apg) | D2.apg.update[u] > (D2.apg.mean+kappa*D2.sd.apg)), NA, D2.apg.update[u])
    CV.cor.apg.update[u] <- ifelse((CV.cor.apg.update[u] < (CV.cor.apg.mean-kappa*CV.cor.sd.apg) | CV.cor.apg.update[u] > (CV.cor.apg.mean+kappa*CV.cor.sd.apg)), NA, CV.cor.apg.update[u])
    CV.cor.se.apg.update[u] <- ifelse((CV.cor.se.apg.update[u] < (CV.cor.se.apg.mean-kappa*CV.cor.se.sd.apg) | CV.cor.se.apg.update[u] > (CV.cor.se.apg.mean+kappa*CV.cor.se.sd.apg)), NA, CV.cor.se.apg.update[u])

    HSw.mort.ri.update[u] <- ifelse((HSw.mort.ri.update[u] < (HSw.mort.mean-kappa*HSw.sd.mort) | HSw.mort.ri.update[u] > (HSw.mort.mean+kappa*HSw.sd.mort)), NA, HSw.mort.ri.update[u])
    Inc.mort.ri.update[u] <- ifelse((Inc.mort.ri.update[u] < (Inc.mort.mean-kappa*Inc.sd.mort) | Inc.mort.ri.update[u] > (Inc.mort.mean+kappa*Inc.sd.mort)), NA, Inc.mort.ri.update[u])
    Tmax.mort.ri.update[u] <- ifelse((Tmax.mort.ri.update[u] < (Tmax.mort.mean-kappa*Tmax.sd.mort) | Tmax.mort.ri.update[u] > (Tmax.mort.mean+kappa*Tmax.sd.mort)), NA, Tmax.mort.ri.update[u])
    Tmin.mort.ri.update[u] <- ifelse((Tmin.mort.ri.update[u] < (Tmin.mort.mean-kappa*Tmin.sd.mort) | Tmin.mort.ri.update[u] > (Tmin.mort.mean+kappa*Tmin.sd.mort)), NA, Tmin.mort.ri.update[u])
    isothrm.mort.ri.update[u] <- ifelse((isothrm.mort.ri.update[u] < (isothrm.mort.mean-kappa*isothrm.sd.mort) | isothrm.mort.ri.update[u] > (isothrm.mort.mean+kappa*isothrm.sd.mort)), NA, isothrm.mort.ri.update[u])
    prcp.mort.ri.update[u] <- ifelse((prcp.mort.ri.update[u] < (prcp.mort.mean-kappa*prcp.sd.mort) | prcp.mort.ri.update[u] > (prcp.mort.mean+kappa*prcp.sd.mort)), NA, prcp.mort.ri.update[u])

    HSw.bw.ri.update[u] <- ifelse((HSw.bw.ri.update[u] < (HSw.bw.mean-kappa*HSw.sd.bw) | HSw.bw.ri.update[u] > (HSw.bw.mean+kappa*HSw.sd.bw)), NA, HSw.bw.ri.update[u])
    Inc.bw.ri.update[u] <- ifelse((Inc.bw.ri.update[u] < (Inc.bw.mean-kappa*Inc.sd.bw) | Inc.bw.ri.update[u] > (Inc.bw.mean+kappa*Inc.sd.bw)), NA, Inc.bw.ri.update[u])
    Tmax.bw.ri.update[u] <- ifelse((Tmax.bw.ri.update[u] < (Tmax.bw.mean-kappa*Tmax.sd.bw) | Tmax.bw.ri.update[u] > (Tmax.bw.mean+kappa*Tmax.sd.bw)), NA, Tmax.bw.ri.update[u])
    Tmin.bw.ri.update[u] <- ifelse((Tmin.bw.ri.update[u] < (Tmin.bw.mean-kappa*Tmin.sd.bw) | Tmin.bw.ri.update[u] > (Tmin.bw.mean+kappa*Tmin.sd.bw)), NA, Tmin.bw.ri.update[u])
    isothrm.bw.ri.update[u] <- ifelse((isothrm.bw.ri.update[u] < (isothrm.bw.mean-kappa*isothrm.sd.bw) | isothrm.bw.ri.update[u] > (isothrm.bw.mean+kappa*isothrm.sd.bw)), NA, isothrm.bw.ri.update[u])
    prcp.bw.ri.update[u] <- ifelse((prcp.bw.ri.update[u] < (prcp.bw.mean-kappa*prcp.sd.bw) | prcp.bw.ri.update[u] > (prcp.bw.mean+kappa*prcp.sd.bw)), NA, prcp.bw.ri.update[u])
  
    HSw.apg.ri.update[u] <- ifelse((HSw.apg.ri.update[u] < (HSw.apg.mean-kappa*HSw.sd.apg) | HSw.apg.ri.update[u] > (HSw.apg.mean+kappa*HSw.sd.apg)), NA, HSw.apg.ri.update[u])
    Inc.apg.ri.update[u] <- ifelse((Inc.apg.ri.update[u] < (Inc.apg.mean-kappa*Inc.sd.apg) | Inc.apg.ri.update[u] > (Inc.apg.mean+kappa*Inc.sd.apg)), NA, Inc.apg.ri.update[u])
    Tmax.apg.ri.update[u] <- ifelse((Tmax.apg.ri.update[u] < (Tmax.apg.mean-kappa*Tmax.sd.apg) | Tmax.apg.ri.update[u] > (Tmax.apg.mean+kappa*Tmax.sd.apg)), NA, Tmax.apg.ri.update[u])
    Tmin.apg.ri.update[u] <- ifelse((Tmin.apg.ri.update[u] < (Tmin.apg.mean-kappa*Tmin.sd.apg) | Tmin.apg.ri.update[u] > (Tmin.apg.mean+kappa*Tmin.sd.apg)), NA, Tmin.apg.ri.update[u])
    isothrm.apg.ri.update[u] <- ifelse((isothrm.apg.ri.update[u] < (isothrm.apg.mean-kappa*isothrm.sd.apg) | isothrm.apg.ri.update[u] > (isothrm.apg.mean+kappa*isothrm.sd.apg)), NA, isothrm.apg.ri.update[u])
    prcp.apg.ri.update[u] <- ifelse((prcp.apg.ri.update[u] < (prcp.apg.mean-kappa*prcp.sd.apg) | prcp.apg.ri.update[u] > (prcp.apg.mean+kappa*prcp.sd.apg)), NA, prcp.apg.ri.update[u])
  
  } # end for
  
  print(k)
} # end k

D2.mort.med <- median(D2.mort.update, na.rm=TRUE)
D2.mort.lo <- quantile(D2.mort.update, probs=0.025, na.rm=TRUE)
D2.mort.up <- quantile(D2.mort.update, probs=0.975, na.rm=TRUE)
print(c(D2.mort.lo,D2.mort.med,D2.mort.up))

D2.bw.med <- median(D2.bw.update, na.rm=TRUE)
D2.bw.lo <- quantile(D2.bw.update, probs=0.025, na.rm=TRUE)
D2.bw.up <- quantile(D2.bw.update, probs=0.975, na.rm=TRUE)
print(c(D2.bw.lo,D2.bw.med,D2.bw.up))

D2.apg.med <- median(D2.apg.update, na.rm=TRUE)
D2.apg.lo <- quantile(D2.apg.update, probs=0.025, na.rm=TRUE)
D2.apg.up <- quantile(D2.apg.update, probs=0.975, na.rm=TRUE)
print(c(D2.apg.lo,D2.apg.med,D2.apg.up))

CV.cor.mort.med <- median(CV.cor.mort.update, na.rm=TRUE)
CV.cor.mort.lo <- quantile(CV.cor.mort.update, probs=0.025, na.rm=TRUE)
CV.cor.mort.up <- quantile(CV.cor.mort.update, probs=0.975, na.rm=TRUE)
print(c(CV.cor.mort.lo,CV.cor.mort.med,CV.cor.mort.up))

CV.cor.bw.med <- median(CV.cor.bw.update, na.rm=TRUE)
CV.cor.bw.lo <- quantile(CV.cor.bw.update, probs=0.025, na.rm=TRUE)
CV.cor.bw.up <- quantile(CV.cor.bw.update, probs=0.975, na.rm=TRUE)
print(c(CV.cor.bw.lo,CV.cor.bw.med,CV.cor.bw.up))

CV.cor.apg.med <- median(CV.cor.apg.update, na.rm=TRUE)
CV.cor.apg.lo <- quantile(CV.cor.apg.update, probs=0.025, na.rm=TRUE)
CV.cor.apg.up <- quantile(CV.cor.apg.update, probs=0.975, na.rm=TRUE)
print(c(CV.cor.apg.lo,CV.cor.apg.med,CV.cor.apg.up))

HSw.mort.ri.lo <- quantile(HSw.mort.ri.update, probs=0.025, na.rm=TRUE)
HSw.mort.ri.med <- median(HSw.mort.ri.update, na.rm=TRUE)
HSw.mort.ri.up <- quantile(HSw.mort.ri.update, probs=0.975, na.rm=TRUE)
Inc.mort.ri.lo <- quantile(Inc.mort.ri.update, probs=0.025, na.rm=TRUE)
Inc.mort.ri.med <- median(Inc.mort.ri.update, na.rm=TRUE)
Inc.mort.ri.up <- quantile(Inc.mort.ri.update, probs=0.975, na.rm=TRUE)
Tmax.mort.ri.lo <- quantile(Tmax.mort.ri.update, probs=0.025, na.rm=TRUE)
Tmax.mort.ri.med <- median(Tmax.mort.ri.update, na.rm=TRUE)
Tmax.mort.ri.up <- quantile(Tmax.mort.ri.update, probs=0.975, na.rm=TRUE)
Tmin.mort.ri.lo <- quantile(Tmin.mort.ri.update, probs=0.025, na.rm=TRUE)
Tmin.mort.ri.med <- median(Tmin.mort.ri.update, na.rm=TRUE)
Tmin.mort.ri.up <- quantile(Tmin.mort.ri.update, probs=0.975, na.rm=TRUE)
isothrm.mort.ri.lo <- quantile(isothrm.mort.ri.update, probs=0.025, na.rm=TRUE)
isothrm.mort.ri.med <- median(isothrm.mort.ri.update, na.rm=TRUE)
isothrm.mort.ri.up <- quantile(isothrm.mort.ri.update, probs=0.975, na.rm=TRUE)
prcp.mort.ri.lo <- quantile(prcp.mort.ri.update, probs=0.025, na.rm=TRUE)
prcp.mort.ri.med <- median(prcp.mort.ri.update, na.rm=TRUE)
prcp.mort.ri.up <- quantile(prcp.mort.ri.update, probs=0.975, na.rm=TRUE)

HSw.bw.ri.lo <- quantile(HSw.bw.ri.update, probs=0.025, na.rm=TRUE)
HSw.bw.ri.med <- median(HSw.bw.ri.update, na.rm=TRUE)
HSw.bw.ri.up <- quantile(HSw.bw.ri.update, probs=0.975, na.rm=TRUE)
Inc.bw.ri.lo <- quantile(Inc.bw.ri.update, probs=0.025, na.rm=TRUE)
Inc.bw.ri.med <- median(Inc.bw.ri.update, na.rm=TRUE)
Inc.bw.ri.up <- quantile(Inc.bw.ri.update, probs=0.975, na.rm=TRUE)
Tmax.bw.ri.lo <- quantile(Tmax.bw.ri.update, probs=0.025, na.rm=TRUE)
Tmax.bw.ri.med <- median(Tmax.bw.ri.update, na.rm=TRUE)
Tmax.bw.ri.up <- quantile(Tmax.bw.ri.update, probs=0.975, na.rm=TRUE)
Tmin.bw.ri.lo <- quantile(Tmin.bw.ri.update, probs=0.025, na.rm=TRUE)
Tmin.bw.ri.med <- median(Tmin.bw.ri.update, na.rm=TRUE)
Tmin.bw.ri.up <- quantile(Tmin.bw.ri.update, probs=0.975, na.rm=TRUE)
isothrm.bw.ri.lo <- quantile(isothrm.bw.ri.update, probs=0.025, na.rm=TRUE)
isothrm.bw.ri.med <- median(isothrm.bw.ri.update, na.rm=TRUE)
isothrm.bw.ri.up <- quantile(isothrm.bw.ri.update, probs=0.975, na.rm=TRUE)
prcp.bw.ri.lo <- quantile(prcp.bw.ri.update, probs=0.025, na.rm=TRUE)
prcp.bw.ri.med <- median(prcp.bw.ri.update, na.rm=TRUE)
prcp.bw.ri.up <- quantile(prcp.bw.ri.update, probs=0.975, na.rm=TRUE)

HSw.apg.ri.lo <- quantile(HSw.apg.ri.update, probs=0.025, na.rm=TRUE)
HSw.apg.ri.med <- median(HSw.apg.ri.update, na.rm=TRUE)
HSw.apg.ri.up <- quantile(HSw.apg.ri.update, probs=0.975, na.rm=TRUE)
Inc.apg.ri.lo <- quantile(Inc.apg.ri.update, probs=0.025, na.rm=TRUE)
Inc.apg.ri.med <- median(Inc.apg.ri.update, na.rm=TRUE)
Inc.apg.ri.up <- quantile(Inc.apg.ri.update, probs=0.975, na.rm=TRUE)
Tmax.apg.ri.lo <- quantile(Tmax.apg.ri.update, probs=0.025, na.rm=TRUE)
Tmax.apg.ri.med <- median(Tmax.apg.ri.update, na.rm=TRUE)
Tmax.apg.ri.up <- quantile(Tmax.apg.ri.update, probs=0.975, na.rm=TRUE)
Tmin.apg.ri.lo <- quantile(Tmin.apg.ri.update, probs=0.025, na.rm=TRUE)
Tmin.apg.ri.med <- median(Tmin.apg.ri.update, na.rm=TRUE)
Tmin.apg.ri.up <- quantile(Tmin.apg.ri.update, probs=0.975, na.rm=TRUE)
isothrm.apg.ri.lo <- quantile(isothrm.apg.ri.update, probs=0.025, na.rm=TRUE)
isothrm.apg.ri.med <- median(isothrm.apg.ri.update, na.rm=TRUE)
isothrm.apg.ri.up <- quantile(isothrm.apg.ri.update, probs=0.975, na.rm=TRUE)
prcp.apg.ri.lo <- quantile(prcp.apg.ri.update, probs=0.025, na.rm=TRUE)
prcp.apg.ri.med <- median(prcp.apg.ri.update, na.rm=TRUE)
prcp.apg.ri.up <- quantile(prcp.apg.ri.update, probs=0.975, na.rm=TRUE)

ri.mort.lo <- c(HSw.mort.ri.lo,Inc.mort.ri.lo,Tmax.mort.ri.lo,Tmin.mort.ri.lo,isothrm.mort.ri.lo,prcp.mort.ri.lo)
ri.mort.med <- c(HSw.mort.ri.med,Inc.mort.ri.med,Tmax.mort.ri.med,Tmin.mort.ri.med,isothrm.mort.ri.med,prcp.mort.ri.med)
ri.mort.up <- c(HSw.mort.ri.up,Inc.mort.ri.up,Tmax.mort.ri.up,Tmin.mort.ri.up,isothrm.mort.ri.up,prcp.mort.ri.up)

ri.bw.lo <- c(HSw.bw.ri.lo,Inc.bw.ri.lo,Tmax.bw.ri.lo,Tmin.bw.ri.lo,isothrm.bw.ri.lo,prcp.bw.ri.lo)
ri.bw.med <- c(HSw.bw.ri.med,Inc.bw.ri.med,Tmax.bw.ri.med,Tmin.bw.ri.med,isothrm.bw.ri.med,prcp.bw.ri.med)
ri.bw.up <- c(HSw.bw.ri.up,Inc.bw.ri.up,Tmax.bw.ri.up,Tmin.bw.ri.up,isothrm.bw.ri.up,prcp.bw.ri.up)

ri.apg.lo <- c(HSw.apg.ri.lo,Inc.apg.ri.lo,Tmax.apg.ri.lo,Tmin.apg.ri.lo,isothrm.apg.ri.lo,prcp.apg.ri.lo)
ri.apg.med <- c(HSw.apg.ri.med,Inc.apg.ri.med,Tmax.apg.ri.med,Tmin.apg.ri.med,isothrm.apg.ri.med,prcp.apg.ri.med)
ri.apg.up <- c(HSw.apg.ri.up,Inc.apg.ri.up,Tmax.apg.ri.up,Tmin.apg.ri.up,isothrm.apg.ri.up,prcp.apg.ri.up)

ri.mort.out <- as.data.frame(cbind(ri.mort.med,ri.mort.up,ri.mort.lo))
colnames(ri.mort.out) <- c("ri.med","ri.up","ri.lo")
rownames(ri.mort.out) <- colnames(rand.datSC[,c(4:9)])

ri.mort.sort <- ri.mort.out[order(ri.mort.out[,1],decreasing=T),1:3]
ri.mort.sort

ri.bw.out <- as.data.frame(cbind(ri.bw.med,ri.bw.up,ri.bw.lo))
colnames(ri.bw.out) <- c("ri.med","ri.up","ri.lo")
rownames(ri.bw.out) <- colnames(rand.datSC[,c(4:9)])

ri.bw.sort <- ri.bw.out[order(ri.bw.out[,1],decreasing=T),1:3]
ri.bw.sort

ri.apg.out <- as.data.frame(cbind(ri.apg.med,ri.apg.up,ri.apg.lo))
colnames(ri.apg.out) <- c("ri.med","ri.up","ri.lo")
rownames(ri.apg.out) <- colnames(rand.datSC[,c(4:9)])

ri.apg.sort <- ri.apg.out[order(ri.apg.out[,1],decreasing=T),1:3]
ri.apg.sort

save.image("hlthClimAnalysis.RData")
