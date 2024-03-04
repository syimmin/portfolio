setwd("C:\\r_temp")
getwd
library(readxl)
library(dplyr)
library(ggplot2)

# 데이터 불러오기기
db <- read_excel("대구은행_기말.xlsx")
View(db)


# group별 요약
db%>%
  group_by(`시/도`)%>%
  summarise(mean_3개월총수신평잔 = mean(`3개월 총수신평잔`),
            mean_수신변화율 = mean(`수신변화율`),
            mean_6개월기여손익 = mean(`6개월기여손익`),
            mean_마케팅점수 = mean(`마케팅점수`),
            mean_여신잔액 = mean(`여신잔액`),
            mean_연령 = mean(`연령`),
            mean_성별 = mean(`성별`),
            n = n())

db%>%
group_by(`연령`)%>%
  summarise(mean_3개월총수신평잔 = mean(`3개월 총수신평잔`),
            mean_수신변화율 = mean(`수신변화율`),
            mean_6개월기여손익 = mean(`6개월기여손익`),
            mean_여신잔액 = mean(`여신잔액`),
            mean_마케팅점수 = mean(`마케팅점수`),
            mean_성별 = mean(`성별`),
            n = n())


# t-test 진행
db_diff<- db%>%
  select(`시/도`, `3개월 총수신평잔`)%>%
  filter(`시/도`%in%c("경상남도", "경기도"))
head(db_diff)
table(db_diff$`시/도`)
t.test(data = db_diff, `3개월 총수신평잔`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `수신변화율`)%>%
  filter(`시/도`%in%c("경상남도", "경기도"))
head(db_diff)
table(db_diff$`시/도`)
t.test(data = db_diff, `수신변화율`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `여신잔액`)%>%
  filter(`시/도`%in%c("경상남도", "경기도"))
head(db_diff)
table(db_diff$`시/도`)
t.test(data = db_diff, `여신잔액`~`시/도`, var.equal = T)

# 회귀분석 진행
plot.new()
fit<- lm(`6개월기여손익`~`마케팅점수`+ `3개월 총수신평잔` + `여신잔액` + `수신변화율` + `연령` + `성별`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`3개월 총수신평잔` + `여신잔액` + `수신변화율` + `연령`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`3개월 총수신평잔`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`여신잔액`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`수신변화율`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`연령`, data = db)
summary(fit)

fit<- lm(`6개월기여손익`~`3개월 총수신평잔` + `여신잔액`, data = db)
summary(fit)

cor.test(db$`3개월 총수신평잔`, db$`여신잔액`)

#군집분석
db_cls <- data.frame(
  "6개월기여손익" = db$`6개월기여손익`,
  "3개월 총수신평잔" = db$`3개월 총수신평잔`,
  "연령" = db$연령,
  "수신변화율" = db$수신변화율,
  "여신잔액" = db$여신잔액,
  "마케팅점수" = db$마케팅점수
)

db_cls<- as.data.frame(scale(db_cls, center = TRUE, scale = TRUE))
db_cls

twss<- NULL
for (i in 1:9) {
  kc <- kmeans(db_cls, centers = i)
  twss <- c(twss, kc$tot.withinss)
}
plot(1:9, twss,
     xlim = c(0,10), type = "b",
     xlab = "군집수", ylab = "TWSS")

db_cls <- data.frame(
  "6개월기여손익" = db$`6개월기여손익`,
  "여신잔액" = db$여신잔액,
)
db_cls<- as.data.frame(scale(db_cls, center = TRUE, scale = TRUE))
db_cls

twss<- NULL
for (i in 1:15) {
  kc <- kmeans(db_cls, centers = i)
  twss <- c(twss, kc$tot.withinss)
}
plot(1:15, twss,
     xlim = c(0,16), type = "b",
     xlab = "군집수", ylab = "TWSS")
