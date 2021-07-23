library(ggplot2)   ;library(readxl)
library(mgcv)      ;library(gamm4)     
library(dplyr)     ;library(lubridate) 
library(splines)   ;library(gridExtra) 
library(sqldf)     ;library(data.table)
library(metafor)   ;library(mixmeta)
library(tsModel)
#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#
setwd("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//github_heatwave//heatwave")

summary(df.1y)

# 사망 장소 >> 병원에서 사망 시 전날로 처리, 도로에서 사망 제외....! >> case 적어서 제외
table(df.5y$d.place)
df.5y <- subset(df.5y, !d.place %in% c(5, 99))
df.5y$hospi <- ifelse(df.5y$d.place==2,1,0)
# 사망 시간 >> 병원 외부여도 12시간내 사망은 전날로 처리
table(df.5y$d.hour)
df.5y <- subset(df.5y, d.hour!=99)
# 사망 주소 >> 시도 코드; 세종과 제주는 제외
unique(df.5y$d.address) %>% length()
df.5y <- subset(df.5y, !d.address %in% c(29, 39))

nrow(df.5y) #최종 6124명! 
######################################################################################
# 아래 둘 중 하나로 선택하여 진행!

# (1) 전체 사망 포함 >> 6124명
data <- df.5y

# (2) 사망 이유 분류: S랑 T 빼고 자연사만 포함 >> 5528명
data$d.r1 %>% table() %>% View()
data <- subset(df.5y, !substr(d.r1,1,1) %in%  c("S", "T"))
######################################################################################
data$d.date <- as.character(data$d.date)
data$year <- substr(data$d.date,1,4)
data$month <- substr(data$d.date,5,6)
data$day <- substr(data$d.date,7,8)

data$d.date2 <- paste0(data$year, "-", data$month, "-", data$day)
data$d.date.r <- as.character(ymd(data$d.date2) - days(1))
data$date <- ifelse(data$d.place==2, data$d.date.r, ifelse(data$d.hour<12, data$d.date.r, data$d.date2))

##################################################################################################
# 15개 도시 모두 (제주, 세종 제외)
sido=c(11,21,22,23,24,25,26,31,32,33,34,35,36,37,38)
label=c("서울","부산","대구","인천","광주","대전","울산","경기","강원","충북","충남","전북","전남","경북","경남")
dt <- subset(data, d.address %in% sido) # 6124명
unique(dt$d.address)
##################################################################################################
#폭염 기간(7-8월)만 OR 넓게 (6-9월)만
dt %>% dplyr::select(date) %>% group_by(year(date), month(date)) %>% count() %>% View()
dt <- subset(dt, month(date) %in% c(7:8));nrow(dt)
unique(month(dt$date))

#2015-2019년도만 >> 최종데이터: 1003명!!
dt$date <- ymd(dt$date)
summary(dt$date)

# case date(oc.date) 대비 control date 4개 만들어주기
# event 4주 전
dt$c1.date <- dt$date - days(28)
# event 3주 전
dt$c2.date <- dt$date - days(21)
# event 2주 전
dt$c3.date <- dt$date - days(14)
# event 1주 전
dt$c4.date <- dt$date - days(7)
# event 1주 후
dt$c5.date <- dt$date + days(7)
# event 2주 후
dt$c6.date <- dt$date + days(14)
# event 3주 후
dt$c7.date <- dt$date + days(21)
# event 4주 후
dt$c8.date <- dt$date + days(28)

### case-control 분리
dt.cont1 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c1.date)
dt.cont2 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c2.date)
dt.cont3 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c3.date)
dt.cont4 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c4.date)
dt.case <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                year, month, day, date)
dt.cont5 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c5.date)
dt.cont6 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c6.date)
dt.cont7 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c7.date)
dt.cont8 <- dt %>% dplyr::select(id, sex, age, d.date, d.hour, d.address, d.place, d.r1, hospi,
                                 year, month, day, c8.date)

names(dt.cont1)[13] <- c("date")
names(dt.cont2)[13] <- c("date")
names(dt.cont3)[13] <- c("date")
names(dt.cont4)[13] <- c("date")
names(dt.cont5)[13] <- c("date")
names(dt.cont6)[13] <- c("date")
names(dt.cont7)[13] <- c("date")
names(dt.cont8)[13] <- c("date")

dt.cont1$class <- 1
dt.cont2$class <- 2
dt.cont3$class <- 3
dt.cont4$class <- 4
dt.case$class <- 5
dt.cont5$class <- 6
dt.cont6$class <- 7
dt.cont7$class <- 8
dt.cont8$class <- 9

dt.cont1$event <- 0
dt.cont2$event <- 0
dt.cont3$event <- 0
dt.cont4$event <- 0
dt.case$event <- 1
dt.cont5$event <- 0
dt.cont6$event <- 0
dt.cont7$event <- 0
dt.cont8$event <- 0

# 전체 데이터 셋 합치기
dt.all <- rbind(dt.case,dt.cont1,dt.cont2,dt.cont3,dt.cont4,dt.cont5,dt.cont6,dt.cont7,dt.cont8) %>%  arrange(id,class)
dt.all$id <- rep(1:nrow(dt.case), each=9)
1003*9
1003*5
1986*5
#------------------------------------------------------------------------------------------------------#
# merge 위한 key 생성 
sido=c(11,21,22,23,24,25,26,29,31,32,33,34,35,36,37,38,39)
label=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")
geocode=c(11,26,27,28,29,30,31,50,41,42,43,44,45,46,47,48,49)
sido.label=data.frame(label,sido,geocode)
head(sido.label,17)

# 변수 이름 sido 로 변경
which(names(dt.all) == c("d.address"))
names(dt.all)[6] <- c("sido")

oc<-merge(dt.all, sido.label,by="sido",all.x=T)
head(oc,5)


#exposure-outcome data merge 
#exposure: temperature(max), air pollution(pm2.5), humidity, 이슬점dew
sido.me1016 <- read.csv("./sido_me(2015-2019).csv")
sido.ap1016 <- read.csv("./sido_monitoring(2015-2019).csv")

sido.me1016$y <- year(ymd(sido.me1016$date))
sido.me1016$m <- month(ymd(sido.me1016$date))
sido.me1016$label <- substr(sido.me1016$key,1,2)

sido.ap1016$date <- substr(sido.ap1016$key,4,13)
sido.ap1016$y <- year(ymd(sido.ap1016$date))
sido.ap1016$m <- month(ymd(sido.ap1016$date))
sido.ap1016$label <- substr(sido.ap1016$key,1,2)

oc$key <- paste0(oc$label,"-",oc$date)

#17개 도시*7년*12개월 = 588
#월별 폭염일수 카운트
me.cnt <- sido.me1016 %>% group_by(label, y, m) %>% summarize(cnt = sum(lag0_maxtemp>=33))
ap.avg <- sido.ap1016 %>% group_by(label, y, m) %>% summarize(avg = mean(lag0_PM25))
#사망 일주일 전 폭염일수 카운트
sido.me1016$me.cnt2 <- apply(Lag(sido.me1016$lag0_maxtemp>=33,0:6),1,sum,na.rm=T)
sido.ap1016$me.avg2 <- apply(Lag(sido.ap1016$lag0_PM25,0:6), 1, mean, na.rm=T)
me.cnt$key2 <- paste0(me.cnt$label,"-",me.cnt$y,"-",sprintf("%02d",me.cnt$m))
ap.avg$key2 <- paste0(ap.avg$label,"-",ap.avg$y,"-",sprintf("%02d",ap.avg$m))
oc$key2 <- substr(oc$key,1,10)

# merge
apme <- merge(sido.me1016 %>% dplyr::select(-y,-m,-label), sido.ap1016 %>% dplyr::select(-date,-y,-m), by="key", all=T)
apme.monthly <- merge(me.cnt, ap.avg, all=TRUE) %>% dplyr::select(key2, label, y, m, cnt, avg)

#apme <- merge(sido.me1016 %>% dplyr::select(-label.r), sido.ap1016 %>% dplyr::select(-date), by="key")

apme %>% dplyr::select(lag0_maxtemp,date) %>% group_by(year(date), month(date)) %>% summarise(sum(lag0_maxtemp>=33)) %>% View()
unique(month(oc$date))

ds <- merge(oc, apme %>% dplyr::select(-date,-label), by="key", all.x=TRUE)
ds <- merge(ds, apme.monthly %>% dplyr::select(key2, cnt, avg), by="key2", all.x=TRUE)

ds$sido <- as.factor(ds$sido)
ds$label <- as.factor(ds$label)
ds$dow <- as.factor(weekdays(ds$date))
ds$time <- as.numeric(as.factor(ds$date))
ds$sex <- as.factor(ds$sex)
ds$hospi <- as.factor(ds$hospi)
ds$class <- as.factor(ds$class)
ds$event <- as.logical(ds$event)

summary(ds)

write.csv(ds,file="5세미만영유아사망_casecrossover3(1:8).csv",row.names=F,na="")
#---------------------------------------------------#
#주소지 11:서울, 21:부산, 22:대구, 23:인천, 24:광주, 25:대전, 26:울산
#성별 1: 남, 2: 여
#영유아 나이 5세 미만
#병원 사망 유무 hospi
#---------------------------------------------------#
library(ggplot2)   ;library(readxl)
library(mgcv)      ;library(gamm4)     
library(dplyr)     ;library(lubridate) 
library(splines)   ;library(gridExtra) 
library(sqldf)     ;library(data.table)
library(metafor)   ;library(mixmeta)
library(tsModel)

setwd("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data")

# 2010-2016년 동안 생후 5세 미만 사망 영유아
sido.all <- read.csv("5세미만영유아사망_casecrossover3(1:8).csv")
#sido.all <- ds

naniar::miss_var_summary(sido.all)
str(sido.all) # 5015명

# 결측값 확인
naniar::miss_var_summary(sido.all)

sido.all<-sido.all[complete.cases(sido.all$lag0_PM25),] #5003명
sido.all<-sido.all[complete.cases(sido.all$lag1_PM25),] #4995명
sido.all<-sido.all[complete.cases(sido.all$lag2_PM25),] #4981명
sido.all<-sido.all[complete.cases(sido.all$lag3_PM25),] #4971명
sido.all<-sido.all[complete.cases(sido.all$lag4_PM25),] #4968명
sido.all<-sido.all[complete.cases(sido.all$lag5_PM25),] #4960명
sido.all<-sido.all[complete.cases(sido.all$lag6_PM25),] #4952명
sido.all<-sido.all[complete.cases(sido.all$lag7_PM25),] #4946명
sido.all<-sido.all[complete.cases(sido.all$lag0_meandew),] #4945명
sido.all<-sido.all[complete.cases(sido.all$lag2_meandew),] #4944명
sido.all<-sido.all[complete.cases(sido.all$lag3_meandew),] #4943명
sido.all<-sido.all[complete.cases(sido.all$lag5_meandew),] #4941명~~
sido.all<-sido.all[complete.cases(sido.all$lag7_meandew),] #8893명~~!

naniar::miss_var_summary(sido.all)

# apparent temperature
sido.all$lag0_apptemp <- -2.653 + (0.994*sido.all$lag0_maxtemp) + 0.0153*(sido.all$lag0_meandew^2)
sido.all$lag1_apptemp <- -2.653 + (0.994*sido.all$lag1_maxtemp) + 0.0153*(sido.all$lag1_meandew^2)
sido.all$lag2_apptemp <- -2.653 + (0.994*sido.all$lag2_maxtemp) + 0.0153*(sido.all$lag2_meandew^2)
sido.all$lag3_apptemp <- -2.653 + (0.994*sido.all$lag3_maxtemp) + 0.0153*(sido.all$lag3_meandew^2)
sido.all$lag4_apptemp <- -2.653 + (0.994*sido.all$lag4_maxtemp) + 0.0153*(sido.all$lag4_meandew^2)
sido.all$lag5_apptemp <- -2.653 + (0.994*sido.all$lag5_maxtemp) + 0.0153*(sido.all$lag5_meandew^2)
sido.all$lag6_apptemp <- -2.653 + (0.994*sido.all$lag6_maxtemp) + 0.0153*(sido.all$lag6_meandew^2)
sido.all$lag7_apptemp <- -2.653 + (0.994*sido.all$lag7_maxtemp) + 0.0153*(sido.all$lag7_meandew^2)
sido.all$lag01_apptemp <- -2.653 + (0.994*sido.all$lag01_maxtemp) + 0.0153*(sido.all$lag01_meandew^2)
sido.all$lag02_apptemp <- -2.653 + (0.994*sido.all$lag02_maxtemp) + 0.0153*(sido.all$lag02_meandew^2)
sido.all$lag03_apptemp <- -2.653 + (0.994*sido.all$lag03_maxtemp) + 0.0153*(sido.all$lag03_meandew^2)
sido.all$lag04_apptemp <- -2.653 + (0.994*sido.all$lag04_maxtemp) + 0.0153*(sido.all$lag04_meandew^2)
sido.all$lag05_apptemp <- -2.653 + (0.994*sido.all$lag05_maxtemp) + 0.0153*(sido.all$lag05_meandew^2)
sido.all$lag06_apptemp <- -2.653 + (0.994*sido.all$lag06_maxtemp) + 0.0153*(sido.all$lag06_meandew^2)

# month
sido.all$month <- as.numeric(as.factor(month(ymd(sido.all$date))))
naniar::miss_var_summary(sido.all)

# sex stratified >> 분석할 때를 위해 분류
# 병원
#sido.all <- subset(sido.all, hospi == 1); table(sido.all$event)
# 병원 외
#sido.all <- subset(sido.all, hospi == 0); table(sido.all$event)
###################################################################
###################################################################
#example GLM vs GAM
#if(!require(ggGam)) devtools::install_github("cardiomoon/ggGam") # 설치가 안된 경우 한번만 실행 
#install.packages("Epi")
#install.packages("survival")
#install.packages("mclogit")
#install.packages("gmodels")
library(ggGam);library(Epi);library(survival);library(mclogit);library(lme4)

gmodels::CrossTable(sido.all$label, sido.all$event)
#table(sido.m$event)
#table(sido.f$event)

# sido==11(서울), 21(부산), 22(대구), 23(인천), 24(광주), 25(대전), 26(울산)

heat.result<-function(SIDO){
  
  sub<-subset(sido.all,label==SIDO)
  
  fit0<-clogit(event~lag0_apptemp+lag0_PM25+month+strata(id),data=sub)
  fit1<-clogit(event~lag01_apptemp+lag01_PM25+month+strata(id),data=sub)
  fit2<-clogit(event~lag02_apptemp+lag02_PM25+month+strata(id),data=sub)
  fit3<-clogit(event~lag03_apptemp+lag04_PM25+month+strata(id),data=sub)
  fit4<-clogit(event~lag04_apptemp+lag04_PM25+month+strata(id),data=sub)
  fit5<-clogit(event~lag05_apptemp+lag05_PM25+month+strata(id),data=sub)
  fit6<-clogit(event~lag06_apptemp+lag06_PM25+month+strata(id),data=sub)
  #fit6<-clogit(event~lag06_maxtemp+lag06_PM25+lag06_meanhumi+lag06_meandew+time+dow+strata(id),data=sub)
  
  res<-as.data.frame(rbind(summary(fit0)$coef[1,],
                           summary(fit1)$coef[1,],
                           summary(fit2)$coef[1,],
                           summary(fit3)$coef[1,],
                           summary(fit4)$coef[1,],
                           summary(fit5)$coef[1,],
                           summary(fit6)$coef[1,]))
  
  res$exposure="temp"
  res$lag=paste0("lag0",1:7-1)
  
  names(res)[2:3] <- c("RR","Se")
  res$RR_lci=round(exp(res$coef-1.96*res$Se),2)
  res$RR_uci=round(exp(res$coef+1.96*res$Se),2)
  res$SIDO=SIDO
  return(res)
}

s1<-heat.result("서울")
s2<-heat.result("부산")
s3<-heat.result("대구")
s4<-heat.result("인천")
s5<-heat.result("광주")
s6<-heat.result("대전")
s7<-heat.result("울산")

s8<-heat.result("경기")
s9<-heat.result("강원")
s10<-heat.result("충북")
s11<-heat.result("충남")
s12<-heat.result("전북")
s13<-heat.result("전남")
s14<-heat.result("경북")
s15<-heat.result("경남")

#Meta-analysis 
#First step

ss<-rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15)
ss$SIDO=factor(ss$SIDO,levels=unique(ss$SIDO))

x11();ggplot(ss,aes(lag,RR))+geom_point(size=2)+
  geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),width=0.2)+theme_gray(base_size=20)+
  facet_wrap(~SIDO)+geom_hline(yintercept=1,col="red")+labs(x="",y="Relative risk (95% CI)")

m0<-with(subset(ss,lag=="lag00"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m1<-with(subset(ss,lag=="lag01"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m2<-with(subset(ss,lag=="lag02"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m3<-with(subset(ss,lag=="lag03"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m4<-with(subset(ss,lag=="lag04"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m5<-with(subset(ss,lag=="lag05"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
m6<-with(subset(ss,lag=="lag06"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))

#Second step
subfit<-subset(ss,lag=="lag01")
#각 모델 추정 결과 
meta1<-with(subfit,rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"));meta1
meta2<-with(subfit,rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="REML"));meta2

x11();forest(meta1,transf=exp,digits=3,
             refline=1,bg=4,col=2,cex.lab=1.0,cex.axis=1.0,cex=1.0,psize=1,mlab="Fixed effect Model")
addpoly(meta2,transf=exp,cex.lab=1.0,cex.axis=1.0,cex=1.0,digits=3,
        mlab=expression("Random effect Model"),col ="blue")

Itwo <- NULL
for (i in c(0:6)){
  lag.k <- as.character(paste0("lag0",i))
  subfit <- subset(ss,lag==lag.k)
  
  mt1 <- with(subfit, rma(coef, sei=Se,measure="RR",digits=3,slab=SIDO, method="FE"))
  mt2 <- with(subfit, rma(coef, sei=Se,measure="RR",digits=3,slab=SIDO, method="REML"))
  
  itwo <- as.data.frame(cbind(mt1$QE,mt1$QEp,mt1$I2,mt2$QE,mt2$QEp,mt2$I2))
  Itwo[[i+1]] <- itwo
  
  print(i)
}

hetero1 <- as.data.frame(rbindlist(Itwo))
names(hetero1) <- c("fixed_Q","fixed_Qpv","fixed_I2","random_Q","random_Qpv","random_I2")
write.csv(hetero1, "heterogenity(moving_average).csv")
#----------------------------------------------------------------------------------------------------#
# 특정 기온 초과 유무
# 폭염 정의 다르게 보기
#특정 온도 이틀 지속인 날만 >>> 강원도는 hw1인 날이 너무 적음(6), 대전은 hw8 아닌 날이 너무 적음(17)
hw1=ifelse(rowSums(Lag(sido.all$lag0_maxtemp>=33,0:1))>=2,1,0)
hw2=ifelse(rowSums(Lag(sido.all$lag0_maxtemp>=32,0:1))>=2,1,0)
hw3=ifelse(rowSums(Lag(sido.all$lag0_maxtemp>=31,0:1))>=2,1,0)
hw4=ifelse(rowSums(Lag(sido.all$lag0_maxtemp>=30,0:1))>=2,1,0)

hw5=ifelse(rowSums(Lag(sido.all$lag0_apptemp>=33,0:1))>=2,1,0)
hw6=ifelse(rowSums(Lag(sido.all$lag0_apptemp>=32,0:1))>=2,1,0)
hw7=ifelse(rowSums(Lag(sido.all$lag0_apptemp>=31,0:1))>=2,1,0)
hw8=ifelse(rowSums(Lag(sido.all$lag0_apptemp>=30,0:1))>=2,1,0)
#hw7<-ifelse(rowSums(Lag(tt$lag0_apptemp>=33,0:1))>=2,1,0)
#hw8<-ifelse(rowSums(Lag(tt$lag0_maxtemp>=33,0:1))>=2,1,0)

tt<-cbind(sido.all,hw1,hw2,hw3,hw4,hw5,hw6,hw7,hw8)

sido.tt <- tt
str(sido.tt)

########################################
sido.tt$hw1 <- as.factor(sido.tt$hw1)
sido.tt$hw2 <- as.factor(sido.tt$hw2)
sido.tt$hw3 <- as.factor(sido.tt$hw3)
sido.tt$hw4 <- as.factor(sido.tt$hw4)
sido.tt$hw5 <- as.factor(sido.tt$hw5)
sido.tt$hw6 <- as.factor(sido.tt$hw6)
sido.tt$hw7 <- as.factor(sido.tt$hw7)
sido.tt$hw8 <- as.factor(sido.tt$hw8)

summary(sido.tt)

heat.result2<-function(SIDO){
  
  sub2<-subset(sido.tt,label==SIDO)
  
  tfit1<-clogit(event~hw1+lag01_PM25+month+lag01_meanhumi+lag01_meandew+strata(id),data=sub2)
  tfit2<-clogit(event~hw2+lag01_PM25+month+lag01_meanhumi+lag01_meandew+strata(id),data=sub2)
  tfit3<-clogit(event~hw3+lag01_PM25+month+lag01_meanhumi+lag01_meandew+strata(id),data=sub2)
  tfit4<-clogit(event~hw4+lag01_PM25+month+lag01_meanhumi+lag01_meandew+strata(id),data=sub2)
  
  tfit5<-clogit(event~hw5+lag01_PM25+month+strata(id),data=sub2)
  tfit6<-clogit(event~hw6+lag01_PM25+month+strata(id),data=sub2)
  tfit7<-clogit(event~hw7+lag01_PM25+month+strata(id),data=sub2)
  tfit8<-clogit(event~hw8+lag01_PM25+month+strata(id),data=sub2)
  #tfit5 <- clogit(event~hw5+lag06_PM25+lag06_meanhumi+lag06_meandew+time+dow+strata(id),data=sub2)
  
  res2<-as.data.frame(rbind(summary(tfit1)$coef[1,],
                            summary(tfit2)$coef[1,],
                            summary(tfit3)$coef[1,],
                            summary(tfit4)$coef[1,],
                            summary(tfit5)$coef[1,],
                            summary(tfit6)$coef[1,],
                            summary(tfit7)$coef[1,],
                            summary(tfit8)$coef[1,]))
  
  res2$exposure=c("hw1","hw2","hw3","hw4","hw5",
                  "hw6","hw7","hw8")
  res2$lag=c("lag01")
  
  names(res2)[2:3] <- c("RR","Se")
  res2$RR_lci=round(exp(res2$coef-1.96*res2$Se),2)
  res2$RR_uci=round(exp(res2$coef+1.96*res2$Se),2)
  res2$SIDO=SIDO
  return(res2)
}

v1<-heat.result2("서울")
v2<-heat.result2("부산")
v3<-heat.result2("대구")
v4<-heat.result2("인천")
v5<-heat.result2("광주")
v6<-heat.result2("대전")
v7<-heat.result2("울산")

v8<-heat.result2("경기")
v9<-heat.result2("강원")
#sido.all %>% group_by(label) %>% summarise(sum(lag0_maxtemp>=33&lag1_maxtemp>=33)) %>% View()
v10<-heat.result2("충북")
v11<-heat.result2("충남")
v12<-heat.result2("전북")
v13<-heat.result2("전남")
v14<-heat.result2("경북")
v15<-heat.result2("경남")

#Meta-analysis 
#First step

w<-rbind(v1,v2,v3,v4,v5,v6,v7,v8,v10,v11,v12,v13,v14,v15) # 강원(v9) hw1, 대전(v6) hw8 제외
w<-rbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)

w$SIDO=factor(w$SIDO,levels=unique(w$SIDO))

x11();ggplot(w,aes(exposure,RR))+geom_point(size=2)+
  geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),width=0.2)+theme_gray(base_size=20)+
  facet_wrap(~SIDO)+geom_hline(yintercept=1,col="red")+labs(x="",y="Relative risk (95% CI)")

n1<-with(subset(w,exposure=="hw1"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n2<-with(subset(w,exposure=="hw2"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n3<-with(subset(w,exposure=="hw3"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n4<-with(subset(w,exposure=="hw4"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n5<-with(subset(w,exposure=="hw5"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))#99분위수대신 주간폭염일수
n6<-with(subset(w,exposure=="hw6"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n7<-with(subset(w,exposure=="hw7"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))
n8<-with(subset(w,exposure=="hw8"),rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"))

#Second step
subfit2<-subset(w,exposure=="hw5")
#각 모델 추정 결과 
tmeta1<-with(subfit2,rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="FE"));tmeta1
tmeta2<-with(subfit2,rma(coef,sei=Se,measure="RR",digits=3,slab=SIDO,method="REML"));tmeta2


x11();forest(tmeta1,transf=exp,digits=3,
             refline=1,bg=4,col=2,cex.lab=1.0,cex.axis=1.0,cex=1.0,psize=1,mlab="Fixed effect Model")
addpoly(tmeta2,transf=exp,cex.lab=1.0,cex.axis=1.0,cex=1.0,digits=3,
        mlab=expression("Random effect Model"),col ="blue")

#---------------------------------------------------------------------------------------------------------------------#
#GAMM result, 도시 random effect
library(gamm4)
gam.fit<-gamm4(event~lag0_maxtemp+ns(lag0_PM25)+ns(lag0_meanhumi)+lag0_meandew+ns(time),
               random=~(1|label),family="binomial",data=sido.tt)
summary(gam.fit$gam)

#GAMM max temperature exposure-response 
gamm.heat<-gamm4(event~
                   s(lag0_maxtemp)+
                   ns(lag0_PM25)+
                   ns(lag0_meanhumi)+
                   ns(lag01_meandew)+
                   ns(time),
                 random=~(1|label),
                 family="binomial",data=sido.tt)

x11();plot(gamm.heat$gam,select=1,
           shade=1,lwd=1,cex.lab=1.4,
           cex.axis=1.4,xlab="Temperature(°C)",
           ylab="Log RR");abline(v=33,lty=2)


