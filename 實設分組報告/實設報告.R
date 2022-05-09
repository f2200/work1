P=read.table("C:/Users/a0981/OneDrive/桌面/實設分組報告/實設報告.csv",header=TRUE,sep=",")
View(P)
P$Material=as.factor(P$Material)
P$Coils=as.factor(P$Coils)
Y=P$Y
Material=P$Material
Coils=P$Coils

FD.aov=aov(Y~.^2,data=P)
FD.aov=aov(Y~Material+Coils+Material*Coils,data=P)
#前面兩條程式碼一樣
summary(FD.aov)

#residual vs run
plot(FD.aov$residuals)
abline(h=0)
library(lmtest)
dwtest(Y~.^2,data=P)#獨立檢定

#normal Q-Q plot
qqnorm(FD.aov$residuals)
qqline(FD.aov$residuals)
shapiro.test(FD.aov$residuals)#常態性檢定

#residual vs fitted
plot(as.numeric(P$Material),FD.aov$residuals,xlab="Material")
plot(as.numeric(P$Coils),FD.aov$residuals,xlab="Coils")
abline(h=0)
library(car)
ncvTest(lm(Y~.^2,data=P))#殘差變異數同質性檢定

trans1=powerTransform(lm(Y~Material+Coils+Material*Coils))#建立Yij
newpeak1=bcPower(Y,trans1$lambda)#建立Yij的lambda
View(newpeak1)
#新的Y值
#建立模組
aov.test4=aov(newpeak1~Material+Coils+Material*Coils)
#重新跑一次三檢定
plot(aov.test4$residuals)
abline(h=0)
library(lmtest)
dwtest(newpeak1~Material+Coils+Material*Coils)

qqnorm(aov.test4$residuals)
qqline(aov.test4$residuals)
shapiro.test(aov.test4$residuals)#常態性檢定

plot(as.numeric(P$Material),aov.test4$residuals,xlab="Material")
plot(as.numeric(P$Coils),aov.test4$residuals,xlab="Coils")
abline(h=0)
library(car)
ncvTest(lm(newpeak1~Material+Coils+Material*Coils))

summary(aov.test4)


