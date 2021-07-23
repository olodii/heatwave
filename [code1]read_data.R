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

a00 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2000.csv")
a01 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2001.csv")
a02 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2002.csv")
a03 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2003.csv")
a04 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2004.csv")
a05 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2005.csv")
a06 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2006.csv")
a07 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2007.csv")
a08 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2008.csv")
a09 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2009.csv")
a10 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2010.csv")
a11 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2011.csv")
a12 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2012.csv")
a13 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2013.csv")
a14 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2014.csv")
a15 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2015.csv")
a16 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2016.csv")
a17 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2017.csv")
a18 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2018.csv")
a19 <- read.csv("C://Users//USER//Desktop//kej//univ//2021-1학기 연구//폭염//TS_seminar//data//통계청사망원인통계//사망_연간자료_A형_2019.csv")

#2010-2019년도
names(a00)

df00 <- a00 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df01 <- a01 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df02 <- a02 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df03 <- a03 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df04 <- a04 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df05 <- a05 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df06 <- a06 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df07 <- a07 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df08 <- a08 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df09 <- a09 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df10 <- a10 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df11 <- a11 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df12 <- a12 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df13 <- a13 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df14 <- a14 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소)
df15 <- a15 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소,사망원인1)
df16 <- a16 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소,사망원인1)
df17 <- a17 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소,사망원인1)
df18 <- a18 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소,사망원인1)
df19 <- a19 %>% dplyr:: select(성별,사망연령.각세.,사망연월일,사망시간,사망자.주소.시도.,사망장소,사망원인1)

names(df00) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df01) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df02) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df03) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df04) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df05) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df06) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df07) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df08) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df09) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df10) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df11) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df12) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df13) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")
names(df14) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place")

names(df15) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place", "d.r1")
names(df16) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place", "d.r1")
names(df17) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place", "d.r1")
names(df18) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place", "d.r1")
names(df19) <- c("sex", "age", "d.date", "d.hour", "d.address", "d.place", "d.r1")

df00$d.r1 <- NaN
df01$d.r1 <- NaN
df02$d.r1 <- NaN
df03$d.r1 <- NaN
df04$d.r1 <- NaN
df05$d.r1 <- NaN
df06$d.r1 <- NaN
df07$d.r1 <- NaN
df08$d.r1 <- NaN
df09$d.r1 <- NaN
df10$d.r1 <- NaN
df11$d.r1 <- NaN
df12$d.r1 <- NaN
df13$d.r1 <- NaN
df14$d.r1 <- NaN

df <- rbind(df00, df01, df02, df03, df04, df05, df06, df07, df08, df09, df10,
            df11, df12, df13, df14, df15, df16, df17, df18, df19)

df$id <- seq(1,nrow(df))

# 기본 통계
summary(df$age)

df <- subset(df, age!=999)

df$date <- ymd(df$d.date)
df$d.year <- year(df$date)

df %>% group_by(d.year) %>% summarise(n()) %>% View()
df %>% group_by(d.year,age) %>% summarise(n()) %>% View()
df %>% group_by(d.year,age<5) %>% summarise(n()) %>% View()

# 5세미만 사망으로 한정
df.1y <- subset(df, age<1) # 1세 미만 영아 사망
df.3y <- subset(df, age<3) # 3세 미만 영유아 사망
df.5y <- subset(df, age<5) # 5세 미만 사망-출생 자료 
df.7y <- subset(df, age<7) # 7세 미만 preschool children 학령 전기 어린이
unique(df.1y$age)
unique(df.3y$age)
unique(df.5y$age)
unique(df.7y$age)

df.all <- NULL
df.all[[1]] <- df.1y
df.all[[2]] <- df.3y
df.all[[3]] <- df.5y
df.all[[4]] <- df.7y