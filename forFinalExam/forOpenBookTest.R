install.packages('ggplot2')
library(ggplot2)
data(mpg)
str(mpg)
dim(mpg)
head(mpg)
table(mpg$drv)
# qplot
######################## 히스토그램 histogram ##########################3
qplot(hwy, data=mpg)
qplot(hwy, data=mpg, fill=I("light blue"), col=I("black"))
qplot(hwy, data=mpg, fill=drv, col=I("black"))
qplot(hwy, data=mpg, fill=drv, col=I("black"), binwidth=2)
qplot(hwy, data=mpg, fill=drv, facets=.~ drv, binwidth=2)
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2)


qplot(clarity, data=diamonds, fill=cut, geom="bar")
qplot(clarity, data=diamonds, color=cut, geom="bar")


########################## 산점도 scatter plot ################################3

qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv)
qplot(displ, hwy, data=mpg, color=drv, facets=.~ drv)


head(mtcars)
qplot(wt, mpg, data=mtcars, color=factor(carb)) # t색상적용
qplot(wt, mpg, data=mtcars, color=factor(carb), size=qsec) # 크기적용
qplot(wt, mpg, data=mtcars, color=factor(carb), size=qsec, shape=factor(cyl)) #모양적용

qplot(wt, mpg, data=mtcars, geom="point")
qplot(wt, mpg, data=mtcars, geom="smooth")
qplot(wt, mpg, data=mtcars, geom=c("point","smooth"))
qplot(wt, mpg, data=mtcars, color=factor(cyl) ,geom=c("point","smooth"))
qplot(wt, mpg, data=mtcars, color=factor(cyl) ,geom="line")
qplot(wt, mpg, data=mtcars, color=factor(cyl) ,geom=c("point","line"))


#ggplot
######## basic setup ############
ggplot(diamonds)
ggplot(diamonds ,aes(x=carat))
ggplot(diamonds ,aes(x=carat, y=price))
ggplot(diamonds ,aes(x=carat, color=cut))
ggplot(diamonds ,aes(x=carat), color='steelblue')
ggplot(diamonds ,aes(x=carat, y=price, color=cut))

######## layers ############

ggplot(diamonds ,aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth()
ggplot(diamonds ,aes(x=carat, y=price, color=cut)) + geom_smooth(aes(x=carat, y=price, color=cut))
ggplot(diamonds ,aes(x=carat, y=price, color=cut)) + geom_point(aes(color=cut)) + geom_smooth()


############### labels ##################

gg = ggplot(diamonds ,aes(x=carat, y=price, color=cut)) + geom_point() + labs(title="Scatterplot", x="Carat", y="Price")
print(gg)

################### theme ########################3

gg1 = gg+ theme(plot.title=element_text(size=30, face="bold"),
                axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15),
                axis.title.x=element_text(size=25),
                axis.title.y=element_text(size=25) + scale_color_discrete(name="Cut of diamonds")
                )
print(gg1)

############### facets ########################

gg1 + facet_wrap(.~cut, ncol=3)
gg1 + facet_wrap(color ~ cut)
gg1 + facet_grid(color ~ cut)

######################3 Box plot #####################


library(datasets)
data(airquality)
airquality$Month = factor(airquality$Month,labels = c("May", "Jun", "Jul", "Aug", "Sep"))
p10 = ggplot(airquality, aes(x=Month, y=Ozone)) + geom_boxplot()
p10
p10 = p10 + scale_x_discrete(name="Month") + scale_y_continuous(name = "Mean ozon in \nparts per billion")
p10
p10 = p10 + scale_y_continuous(name = "Mean ozone in\nparts per billion", breaks = seq(0,175,25), limits=c(0,175))
p10

p10 = ggplot(airquality, aes(x = Month, y = Ozone)) + geom_boxplot(fill = "#4271AE", color = "#1F3552") + scale_y_continuous(name = "Mean ozone in \nparts per billion", breaks = seq(0,175,25), limits=c(0,175)) + scale_x_discrete(name = "Month") + ggtitle("Boxplot of mean ozone by month")
p10
p10 = p10 + theme_bw()
p10


bmiukb <- data.frame(
  method = factor(c("PRS","PRS+MTAG","LDpred","LDpred+MTAG","MCP","Lasso","MCP+CTPR",
                    "Lasso+CTPR","PRS","PRS+MTAG","LDpred","LDpred+MTAG","MCP","Lasso","MCP+CTPR","Lasso+CTPR"),levels=c("PRS","PRS+MTAG","LDpred","LDpred+MTAG","MCP","Lasso","MCP+CTPR","Lasso+CTPR")),
  cohort=factor(c("NHS/HPFS/PHS","NHS/HPFS/PHS","NHS/HPFS/PHS","NHS/HPFS/PHS",
                  "NHS/HPFS/PHS","NHS/HPFS/PHS","NHS/HPFS/PHS","NHS/HPFS/PHS","UKBiobank","UKBiobank","UKBiobank","UKBiobank","UKBiobank","UKBiobank","UKBiobank","UKBiobank"),levels=c("NHS/HPFS/PHS","UKBiobank")),
  pred = c(0.1427,0.14123,0.2217,0.2225,0.2609,0.2796,0.2922,0.2978,
           0.2232,0.2220,0.3653,0.3583,0.3775,0.3908,0.4247,0.4284)*100,
  se = c(0.000983528,0.0009826683,0.001116853,0.001109679,
         0.001144894,0.001150417,0.001157279,0.001158628,
         0.0010686,0.001068152,0.001160714,0.001162218,
         0.001154691,0.001150587,0.001133377,0.001132073)*100
)
bmiukb


ggplot(data=bmiukb, aes(x=cohort, y=pred, fill=method)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+ theme_bw() + 
  geom_errorbar(aes(ymin=pred-se, ymax=pred+se),width=.2,position=position_dodge(.9), colour="#606060") +
  xlab("Validation Dataset") + 
  ylab(quote(paste("Prediction ",R^2,"(%)"))) +
  labs(fill="", colour="", linetype="", title="HGT-BMI (N=436,898)") + 
  scale_fill_manual(values=c("#F3898B","#FF6666","#F9A825","#EF6C00","#42A5F5","#1976D2", "#5C6BC0","#283593")) + 
  theme(plot.title = element_text(lineheight=.8, size=16), axis.text=element_text(size=14),
        axis.title=element_text(size=14),legend.text=element_text(size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(file= 'bmi.pdf', width=7.5, height=7.5)
ggsave(file= 'bmi.jpg', dpi=200)
getwd()

###################### 10 장 mariadb #######################3

install.packages("DBI")
install.packages('RMySQL') # mysql 돌리기

library(DBI)
library(RMySQL)  # 맥에서 안돼서 다른방법찾을 찾음

# terminal에서 해줘야할 작업들
# mysql.server start
# mariadb -u root -p

# show databases; 데이터 베이스 이름 보기

# 데이터베이스 만들기
# create database ?????

# 데이터 베이스 사용하기
# use ?????

# 데이터베이스 테이블 보기
# show tables;


# 데이터 베이스에서 테이블 만들기
# create table goods (
# code int primary key,
# name varchar(20) not null,
# su int, 
# dan, int);

# 데이터베이스에서 만든 테이블 내부에 값들 널어주기
# insert into goods values (1, '냉장고', 2, 850000);

#데이터베이스 내부에 만든 테이블 읽기
# select * from goods;


# use work 이후에 바깥으로 다시 나오기


# 유저와 비밀번호 만들기
# create user 'scott'@'localhost' identified by 'tiger';


#에러가 나면 확인해보기
# select Host, User, Password from mysql.user;

# 만든 유저에 데이터베이스 접근권한 주기
# grant all privileges on work.* to 'scott'@'localhost';
# flush privileges;



drv <- dbDriver("MySQL")
conn <- dbConnect(drv, username="scott", password="tiger", dbname ="work", host="localhost")
dbSendQuery(conn, "SET NAMES utf8;")  # 콘솔에 한글 나오게 하기
dbSendQuery(conn, "SET CHARACTER SET utf8;")  # 콘솔에 한글 나오게 하기
dbSendQuery(conn, "SET character_set_connection=utf8;") # 콘솔에 한글 나오게 하기

# select 문장

dbGetQuery(conn, "select * from goods")
dbGetQuery(conn, "select code,name from goods where code=1 or code=2")

#create / alter 문장
# dbSendUpdate 대신 dbSendQuery 사용
dbSendQuery(conn, "create table goods1 as select * from goods")
dbGetQuery(conn, "select * from goods1")
dbSendQuery(conn, "alter table goods1 rename to goods_original")
dbGetQuery(conn, "select * from goods_original")

# insert, update, delete 문장

dbSendQuery(conn, "insert into goods values (5, '식기세척기', 1, 250000)")
dbGetQuery(conn, "select * from goods")
dbSendQuery(conn, "insert into goods values (6, 'test', 1,1000)")
dbGetQuery(conn, "select * from goods")

dbSendQuery(conn, "update goods set name='테스트' where code=6")
dbGetQuery(conn, "select * from goods")
dbSendQuery(conn, "update goods set su=3 where code=6")
dbGetQuery(conn, "select * from goods")
dbSendQuery(conn, "delete from goods where code=6")
dbGetQuery(conn, "select * from goods")

# inner / outer / left / right join
dbGetQuery(conn, "select * from goods_original inner join goods on
           goods_original.code=goods.code")

#write table
getwd()
setwd("/Users/bongsu/Downloads/data/part2")
# type.convert.default(data[[i]], as.is = as.is[i], dec = dec, : '<b3><b2><c0><da>' 에러 난오면 fileEncoding 넣어줘야함
recode = read.csv('recode.csv', fileEncoding = "CP949")
recode

dbWriteTable(conn, "goods_newss", recode)
#ERROR 3948 (42000): Loading local data is disabled; this must be enabled on both the client and server sides  오류 나오면
# terminal 에서 mysql -u root -p 해서 접속하고
# show global variables like 'local_infile'; 하면
# local_infile | off 돼있는데 이걸 on 해줘야함
# set global local_infile=true;  이렇게 하기
dbGetQuery(conn, "select * from goods_newss")


#disconnect 연결 끊기
dbDisconnect(conn)

############################### 11장 모평균 추론 ##########################3
####################3신뢰도 추정 ci#####################
set.seed(1)
n = 100
x = rnorm(n)
xbar = mean(x)
se = sd(x)/sqrt(n)
tval = qt(0.05/2, df=n-1)*(-1)
ci.x = c(xbar-tval*se, xbar+tval*se)
2-1*5
ci.x


############## 단일 모집단 모평균 추론 ######################3

setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("one_sample.csv", header=TRUE)
head(data)
str(data)
x = data$time
x1 = na.omit(x)
mean(x1)

shapiro.test(x1) # 샤피로 검정 shapiro test, 정규분포에 얼마나 가까운가?
hist(x1, freq=F, col="light blue", main="Histrogram of x1") # histogram만들기, 히스토그램
lines(density(x1), col="red") # 히스토그램에 라인 긋기

qqnorm(x1, pch=16, col="light blue") #qq plot 큐큐플롯, 큐큐plot, qqplot, qq플롯
qqline(x1, lty=1, col="red") # qqplot에 라인그리기 

res = t.test(x1, mu = 5.2) # 평균이 5.2인지 
res = t.test(x1, mu = 5.2, alter="two.side", conf.level = 0.95) # 위와 동일 양측에 0.95가 기본 옵션
res

res = t.test(x1, mu = 5.2, alter="greater", conf.level = 0.95) # 위와 동일 양측에 0.95가 기본 옵션
res

############ 대응 비교 모평균 추론 #############

setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("paired_sample.csv", header=TRUE)
head(data)

result = subset(data, !is.na(after), c(before, after))
x = result$before
y = result$after
mean(x)
mean(y, na.rm = T)

res = t.test(x, y, paired=TRUE)
res = t.test(x, y, paired=TRUE, alter="two.side", conf.level=0.95)
res

res = t.test(x, y, paired=TRUE, alter="less", conf.level=0.95)
res

############ 서로 독립인 두 집단에 대한 추론 ############
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("two_sample.csv", header=TRUE)
head(data)
summary(data)
result = subset(data, !is.na(score), c(method, score))
a = subset(result, method==1)$score
mean(a)
b = subset(result, method==2)$score
mean(b)

#분산 동질성 검정
var.test(a,b, alter="two.sided")

res = t.test(a,b)
res = t.test(a, b,  alter="two.side", conf.level=0.95)
res

res = t.test(a, b,  alter="less", conf.level=0.95)

############# 서로 독립인 세 집단 이상에 대한 분산 분석 ##################
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("three_sample.csv", header=TRUE)
data = subset(data, !is.na(score), c(method, score))


# outlier제거
plot(data$score)
data2 = subset(data, score <= 14)
boxplot(data2$score, col="light blue")

# 테이블 만들기
data2$method2[data2$method==1] = "Method1"
data2$method2[data2$method==2] = "Method2"
data2$method2[data2$method==3] = "Method3"
x = table(data2$method2)
y = tapply(data2$score, data2$method2, mean)
df = data.frame(Method = x, Score =y)
df

# 세 집단간 동질성 분석 (세 집단간 분포의 모양이 같다)
bartlett.test(score ~ method2, data=data2)

# 세 집단간 평균의 차이 검정 (세 집단의 평균이 같다.)
res = aov(score~method2, data= data2)
summary(res)


################## 모집단의 비율 추론 ########################3

# 단일집단의 비율 추론
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("one_sample.csv", header=TRUE)
x = data$survey
summary(x)
table(x)
table(x)/length(x)

binom.test(c(136,14), p=0.8)
binom.test(c(136,14), p=0.8, alternative = "two.sided", conf.level = 0.95)

binom.test(c(136,14), p=0.8, alternative = "greater", conf.level = 0.95)


# 두 모집단의 비율 추론
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("two_sample.csv", header=TRUE)
head(data)
summary(data)
x = data$method
y = data$survey
table(x)
table(y)

table(x,y, useNA='ifany')

prop.test(c(110, 135), c(150,150))
prop.test(c(110, 135), c(150,150), alternative="two.sided", conf.leve=0.95)

prop.test(c(110, 135), c(150,150), alternative="less", conf.leve=0.95)

# 세 모집단의 비율추론
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("three_sample.csv", header=TRUE)
head(data)
method= data$method
survey = data$survey
table(method, useNA='ifany')
table(method, survey, useNA='ifany')

prop.test(c(34,37,39), c(50,50,50))
prop.test(c(34,37,39), c(50,50,50), alternative = 'two.sided', conf.level = 0.95)



############################ 12장 #####################################3
# 적합도 검정

x = data.frame(matrix(c(1,2,3,4,5,41,30,51,71,61), ncol=2))
colnames(x) = c("prod", "freq")
x
x$prop = round(x$freq/sum(x$freq), 2)
x$prop

chisq.test(x$freq)
chisq.test(x$freq, p=c(0.2,0.1,0.2,0.3,0.2))

# 독립성 검정

install.packages('gmodels')
library(gmodels)
data = read.csv('cleanDescriptive.csv', header=TRUE, fileEncoding = 'CP949')
data

x = data$level2
y = data$pass2

chisq.test(x,y)
CrossTable(x,y,chisq=TRUE)


# 동질성 검정
setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("homogenity.csv", header=TRUE)
data
head(data)

table(data$method, data$survey, exclude=NULL)
x = data$method
y = data$survey


chisq.test(x,y)
CrossTable(x,y, chisq=TRUE)



tab1 = table(data$level2, data$pass2) # 독립성 검정
tab1 = table(data$method, data$survey) # 동질성 검정

compute.chisq = function(tab) {
  r = nrow(tab)
  c = ncol(tab)
  tab = cbind(tab,apply(tab,1,sum))
  tab = rbind(tab,apply(tab,2,sum))
  chi = 0
  for (i in 1:r) {
    for (j in 1:c) {
      eij = tab[r+1,c+1]*tab[i,j]/tab[r+1,j]*tab[i,j]/tab[i,c+1]
      chi = chi + (tab[i,j]-eij)^2/eij
    }
  }
  list(chi,pchisq(chi,df=(r-1)*(c-1),lower.tail=F))
}
compute.chisq(tab1)



############################# 크롤링 ##############################3


########################################
## 텍스트 마이닝 분석 
########################################

###############
# 1. 토픽 분석  
###############

# 텍스트마이닝을 위한 패키지 설치 

setwd("/Users/bongsu/Downloads/data/part2")
Sys.setenv(JAVA_HOME="/Users/bongsu/Downloads/jdk-17.0.1.jdk/Contents/Home")
dyn.load("/Library/Java/JavaVirtualMachines/temurin-16.jdk/Contents/Home/lib/server/libjvm.dylib")
Sys.getenv()
Sys.getenv("JAVA_HOME")
.jinit()



install.packages("rJava")
install.packages("multilinguer")
library(multilinguer)
multilinguer::install_jdk()
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force=TRUE)
library(KoNLP)


#setwd(readClipboard())

library(slam) 
library(RSQLite)
library(httr)
library(XML)
library(tm) # 영문 텍스트 마이닝 
library(wordcloud) # RColorBrewer()함수 제공

# 패키지 로딩


# 명사 추출 예 
extractNoun('안녕하세요. 홍길동 입니다.')

# (1) 텍스트 자료 가져오기 
facebook <- file("facebook_bigdata.txt", encoding="UTF-8")
facebook_data <- readLines(facebook) # 줄 단위 데이터 생성
head(facebook_data) # 앞부분 6줄 보기 - 줄 단위 문장 확인 
str(facebook_data) # chr [1:76]

# (2) 자료집(Corpus) 생성 
facebook_corpus <- Corpus(VectorSource(facebook_data))
facebook_corpus 
inspect(facebook_corpus) # 76개 자료집에 포함된 문자 수 제공 


# (3)단어 추가와 단어추출 
# 세종 사전에 없는 단어 추가
#install.packages('curl')
library(curl)
useSejongDic() # 세종 사전 불러오기
mergeUserDic(data.frame(c("R 프로그래밍","페이스북","소셜네트워크"), c("ncn"))) 
# ncn -명사지시코드

# (4) 단어추출 사용자 함수 정의 
# 사용자 정의 함수 작성 
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
# exNouns 함수 이용 단어 추출 
facebook_nouns <- sapply(facebook_corpus, exNouns) 
facebook_nouns[1] # 단어만 추출된 첫 줄 보기 

# (5) 추출된 단어 대상 전처리
# 추출된 단어 이용 자료집 생성
myCorputfacebook <- Corpus(VectorSource(facebook_nouns)) 
myCorputfacebook 
# 데이터 전처리 
myCorputfacebook <- tm_map(myCorputfacebook, removePunctuation) # 문장부호 제거
myCorputfacebook <- tm_map(myCorputfacebook, removeNumbers) # 수치 제거
myCorputfacebook <- tm_map(myCorputfacebook, tolower) # 소문자 변경
myStopwords = c(stopwords('english'), "사용", "하기")
myCorputfacebook <-tm_map(myCorputfacebook, removeWords, myStopwords) # 불용어제거
inspect( myCorputfacebook[1:5])

# (6) 단어 선별(단어 길이 2개 이상)
# 단어길이 2개 이상인 단어만 선별하여 matrix 자료구조로 변경
myCorputfacebook_term <- TermDocumentMatrix(myCorputfacebook, control=list(wordLengths=c(2,Inf)))

# matrix 자료구조를 data.frame 자료구조로 변경
myTermfacebook.df <- as.data.frame(as.matrix(myCorputfacebook_term)) 
dim(myTermfacebook.df) 

# (7) 단어 빈도수 구하기 - 빈도수가 높은 순서대로 내림차순 정렬
wordResult <- sort(rowSums(myTermfacebook.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
wordResult[1:10]

# (8) 단어 구름(wordcloud) 생성 - 디자인 적용 전
myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
par(family="AppleGothic")
wordcloud(myName, wordResult) # 단어구름 적성
warning()


# (9) 단어 구름에 디자인 적용(빈도수, 색상, 위치, 회전 등) 
# 단어이름과 빈도수로 data.frame 생성
word.df <- data.frame(word=myName, freq=wordResult) 
str(word.df) # word, freq 변수

# 단어 색상과 글꼴 지정
pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
# 폰트 설정세팅 : "맑은 고딕", "서울남산체 B"
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows

# 단어 구름 시각화 - 별도의 창에 색상, 빈도수, 글꼴, 회전 등의 속성 적용 
#x11( ) # 별도의 창을 띄우는 함수
wordcloud(word.df$word, word.df$freq, 
          scale=c(5,1), min.freq=3, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")


#################
## 2 연관어 분석 
#################

# 한글 처리를 위한 패키지 설치
#install.packages('rJava')
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151')
#library(rJava) # 아래와 같은 Error 발생 시 Sys.setenv()함수로 java 경로 지정
#install.packages("KoNLP") 
#library(KoNLP) # rJava 라이브러리가 필요함


# (1) 텍스트 파일 가져오기 ('빅데이터'키워드로 페이스북에서 검색한 결과파일일)
marketing <- file("marketing.txt", encoding="UTF-8")
marketing2 <- readLines(marketing) # 줄 단위 데이터 생성
# incomplete final line found on - Error 발생 시 UTF-8 인코딩 방식으로 재 저장
close(marketing) 


# (2) 줄 단위 단어 추출
lword <- Map(extractNoun, marketing2)  
length(lword) # [1] key = 472

lword <- unique(lword) # 중복제거1(전체 대상)
length(lword) # [1] 353 (19개 제거)

# (3) 중복단어 제거와 추출 단어 확인
lword <- sapply(lword, unique) # 중복제거2 (줄 단위 대상) 
length(lword) # [1] 353


# (4) 연관어 분석을 위한 전처리 

# 단어 필터링 함수 정의 (길이 2~4사이 한글 단어 추출)
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

filter2 <- function(x){
  Filter(filter1, x)
}

# 줄 단위로 추출된 단어 전처리 
lword <- sapply(lword, filter2)
lword

# (5) 트랜잭션 생성

# 연관분석을 위한 패키지 설치
install.packages("arules")
library(arules) 

# 트랜잭션 생성 
wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran 

# (6) 단어 간 연관규칙 산출 
# default (support지지도=0.1, confidence신뢰도=0.8, maxlen최대길이=10)
# 지지도와 신뢰도를 높이면 발견되는 규칙수가 줄어듦
tranrules <- apriori(wordtran, parameter=list(supp=0.25, conf=0.05)) 
tranrules <- apriori(wordtran, parameter=list(supp=0.25, conf=0.8)) 
tranrules <- apriori(wordtran, parameter=list(supp=0.3, conf=0.05)) 

# 연관규칙 생성 결과보기 
inspect(tranrules) # 연관규칙 생성 결과(59개) 보기

# (7)  연관어 시각화 

# 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules, ruleSep=" ")  
rules
class(rules)

# 문자열로 묶인 연관단어를 행렬구조 변경 
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
rules
class(rules) 

# 행 단위로 묶어서 matrix로 반환
rulemat <- do.call("rbind", rules)
rulemat
class(rulemat)

# 연관어 시각화를 위한 igraph 패키지 설치
install.packages("igraph") # graph.edgelist(), plot.igraph(), closeness() 함수 제공
library(igraph)   

# edgelist보기 - 연관단어를 정점 형태의 목록 제공 
ruleg <- graph.edgelist(rulemat[c(12:59),], directed=F) # [1,]~[11,] "{}" 제외
ruleg

#  edgelist 시각화
#X11()
par(family = 'D2Coding')

plot.igraph(ruleg, vertex.label=V(ruleg)$name,
            vertex.label.cex=1.2, vertex.label.color='black', 
            vertex.size=20, vertex.color='green', vertex.frame.color='blue')

#############################
# 3. 실시간 뉴스 수집과 분석
#############################

# 텍스트마이닝을 위한 패키지 설치 
#install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip")
#install.packages("Sejong")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("RSQLite")
#install.packages("hash")
#install.packages("tau")
#install.packages("httr")
#install.packages("XML")
#install.packages("rlang")

library(RSQLite)
library(KoNLP)
library(tm)
library(wordcloud)
library(httr)
library(XML)

# (1) URL 요청
url <- "https://news.naver.com/"
web <- GET(url)
web

# (2) HTML 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim=T, encoding="utf-8")
rootNode <- xmlRoot(html)
rootNode

# (3) 태그 자료 수집
# <div class="main_content_inner _content_inner">
news <- xpathSApply(rootNode, "//div[@class='main_content_inner _content_inner']", xmlValue)
news

# (4) 수집한 자료 전처리
news_pre <- gsub('[\r\n\t]', '', news) 
news_pre <- gsub('[a-z]','',news_pre)
news_pre <- gsub('[A-Z]','',news_pre)
news_pre <- gsub('\\s+',' ',news_pre)
news_pre <- gsub('[[:cntrl:]]','',news_pre)
news_pre <- gsub('[[:punct:]]','',news_pre)
news_pre <- gsub('\\d+',' ',news_pre)
news_pre

# (5) 토픽 분석
# 단어 추출
library(KoNLP)
news_noun <- extractNoun(news_pre)
news_noun

#말뭉치 생성
library(tm)
newsCorpus <- Corpus(VectorSource(news_noun))
TDM <- TermDocumentMatrix(newsCorpus, control=list(wordLengths=c(4,16)))
TDM
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
tdm.df

wordResult <- sort(rowSums(tdm.df),decreasing =T)
wordResult

wordResult <- wordResult[-c(1:6)]

library(wordcloud)
myNames <- names(wordResult)
df <- data.frame(word=myNames, freq=wordResult)
df
pal <- brewer.pal(12,"Paired")
X11()
wordcloud(df$word, df$freq, min.freq=2, random.order=F, scale=c(4,0.7), 
          rot.per=0.1, colors=pal, family="malgun")






