library(multilinguer)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

useSejongDic()
pal2 <- brewer.pal(8,"Dark2")
text <- readLines("C:/r_temp/지방은행_블로그.txt", encoding="UTF-8", warn=FALSE)
text
noun <- sapply(text,extractNoun, USE.NAMES = F)
noun
class(noun)
noun2 <- unlist(noun) 
noun2 <- gsub("은행", "", noun2)
noun2 <- noun2[nchar(noun2)>1] #한글자 단어 제거
wordcount <- table(noun2)
temp <- sort(wordcount,decreasing = T)[1:15]
#temp <- temp[-1] 
barplot(temp, las = 2, names.arg = names(temp), col = "lightblue",          
        main = "Most frequent Words", ylab = "Word frequencies")

wordcloud(names(wordcount),          
          freq=wordcount,          
          scale = c(6,2),          
          min.freq = 5,   #단어의 최소빈도(최소 3번이상 출현하는 것들만 쓰자)          
          random.order = F,    #단어 출력 위치 (현재세팅은 빈도수 많은 애가 가운데로)          
          rot.per = 0.1,     #90도 회전 단어 비율          
          colors = pal2)

