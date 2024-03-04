setwd("C:\\r_temp")
getwd
library(readxl)
library(dplyr)

# 데이터 불러오기
db <- read_excel("대구은행_전처리.xlsx")
View(db)

# aggregate
aggregate(`6개월기여손익`~`연령`, db, mean)
aggregate(`3개월 총수신평잔`~`연령`, db, mean)
aggregate(`수신변화율`~`연령`, db, mean)
aggregate(`여신잔액`~`연령`, db, mean)

# group별 요약
db%>%
  group_by(`시/도`)%>%
  summarise(mean_3개월총수신평잔 = mean(`3개월 총수신평잔`),
            mean_수신변화율 = mean(`수신변화율`),
            mean_6개월기여손익 = mean(`6개월기여손익`),
            mean_여신잔액 = mean(`여신잔액`),
            mean_연령 = mean(`연령`),
            mean_성별 = mean(`성별`),
            n = n())

# t-test 진행행
db_diff<- db%>%
  select(`연령`, `수신변화율`)%>%
  filter(`연령`%in%c(20, 40))
head(db_diff)
table(db_diff$`연령`)

t.test(data = db_diff, `수신변화율`~`연령`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `수신변화율`)%>%
  filter(`시/도`%in%c("경상북도", "경기도"))
head(db_diff)
table(db_diff$`시/도`)

t.test(data = db_diff, `수신변화율`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `수신변화율`)%>%
  filter(`시/도`%in%c("경상북도", "대구광역시"))
head(db_diff)
table(db_diff$`시/도`)

t.test(data = db_diff, `수신변화율`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `수신변화율`)%>%
  filter(`시/도`%in%c("경상북도", "경상남도"))
head(db_diff)
table(db_diff$`시/도`)

t.test(data = db_diff, `수신변화율`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `3개월 총수신평잔`)%>%
  filter(`시/도`%in%c("경상북도", "경상남도"))
head(db_diff)
table(db_diff$`시/도`)

t.test(data = db_diff, `3개월 총수신평잔`~`시/도`, var.equal = T)

db_diff<- db%>%
  select(`시/도`, `3개월 총수신평잔`)%>%
  filter(`시/도`%in%c("경상북도", "대구광역시"))
head(db_diff)
table(db_diff$`시/도`)

t.test(data = db_diff, `3개월 총수신평잔`~`시/도`, var.equal = T)

# 회귀분석 진행
fit<- lm(`6개월기여손익`~`3개월 총수신평잔`, data = db)
abline(fit, col = 'blue')
summary(fit)
