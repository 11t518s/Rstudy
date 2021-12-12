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
