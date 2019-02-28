# cross validation
install.packages("caret")
library(caret)

apt_all_info_all$hoCnt<-as.integer(apt_all_info_all$hoCnt)
apt_all_info_all$hoCnt<-as.integer(apt_all_info_all$year)
apt_2018_info1<-apt_all_info_all[(apt_all_info_all$year>=2015)&
                                   apt_all_info_all$area_factor==1,]
apt_2018_info2<-apt_all_info_all[(apt_all_info_all$year>=2015)&
                                apt_all_info_all$area_factor==2,]
apt_2018_info3<-apt_all_info_all[apt_all_info_all$area_factor==3,]
apt_2018_info4<-apt_all_info_all[(apt_all_info_all$year>=2015)&
                                   apt_all_info_all$area_factor==4,]
apt_2018_info5<-apt_all_info_all[apt_all_info_all$area_factor==5,]

# 1 테스트 데이터 형성과 회귀분석
dataf<-NA
dataf <- createFolds(apt_2018_info1$price_area, k=5)
dataf$Fold1
trainData <-apt_2018_info1[-dataf$Fold1,]
testData <- apt_2018_info1[dataf$Fold1,]

fit1 <- lm(price_area~subway+mid+hoCnt+
             floor+factor(month)+factor(year)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=trainData)

summary(fit1)
# 테스트
pre1 <- predict(fit2, newdata = testData, interval="prediction",
                level=0.5)
pre1 <- as.data.frame(pre1)

# 얘는 뭐?
pre1 <- cbind(pre1, testData$price_area)
head(pre1)
table(trainData[trainData$price_area<=0])
summary(pre1$fit)
tf <- NA
pre1 <- cbind(pre1, tf)
pre1$tf[pre1$`testData$price_area`>=pre1$lwr&pre1$`testData$price_area`<=pre1$upr]<-T
pre1$tf[is.na(pre1$tf)]<-F
head(pre1)
View(pre1)
# 예측 성공 비율
sum(pre1$tf=="TRUE")/dim(pre1)[1]
summary(trainData$price_area)

#계수추출
csh = as.data.frame(coef(fit1))
write.csv(csh, "apt_code/apt_data/area_factor1.csv")
write.csv(apt_2018_info3, "apt_code/apt_data/apt_2018_info3.csv")
###############
# 크기1
fit1 <- lm(price_area~subway+hoCnt+floor+factor(month)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=apt_2018_info1)

summary(fit1)

#계수추출
csh1 = as.data.frame(coef(fit1))
write.csv(csh1, "apt_code/apt_data/area_factor1.csv")
# 크기2
fit2 <- lm(price_area~subway+mid+hoCnt+floor+factor(month)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=trainData)

summary(fit2)

#계수추출
csh2 = as.data.frame(coef(fit2))
write.csv(csh2, "apt_code/apt_data/area_factor2.csv")
# 크기3
fit3 <- lm(price_area~subway+mid+hoCnt+floor+factor(month)+
             factor(year)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=apt_2018_info3)

summary(fit3)

#계수추출
csh3 = as.data.frame(coef(fit3))
write.csv(csh3, "apt_code/apt_data/area_factor3.csv")
# 크기4
fit4 <- lm(price_area~subway+mid+hoCnt+floor+factor(month)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=apt_2018_info4)

summary(fit4)

#계수추출
csh4 = as.data.frame(coef(fit4))
write.csv(csh4, "apt_code/apt_data/area_factor4.csv")
# 크기5
fit5 <- lm(price_area~subway+mid+hoCnt+floor+factor(month)+
             factor(gudongCode)+factor(codeAptNm_factor)+
             factor(codeHeatNm_factor),
           data=apt_2018_info5)

summary(fit5)

#계수추출
csh5 = as.data.frame(coef(fit5))
write.csv(csh5, "apt_code/apt_data/area_factor5.csv")

