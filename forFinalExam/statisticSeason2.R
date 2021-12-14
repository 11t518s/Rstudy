install.packages('ggplot2')
library(ggplot2)
head(diamonds)
dim(diamonds)
ggplot(diamonds, aes(x=carat, y=price, color=cut))

# layers
library(rJava)

g1 = ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth()
 
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + geom_smooth(aes(x=carat, y=price))
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut)) + geom_smooth() #????????? 媛????

g2 = ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut, shape=cut)) + geom_smooth()
                              
# labels
g3 = g1 + labs(title = "scatter plot", x = "Carat", y = "price")
print(g3)

#Theme

g4 = g3 + theme(plot.title=element_text(size=20, face="bold"), 
                axis.text.x = element_text(size=15),
                axis.text.y = element_text(size=20),
                axis.title.x = element_text(size=25),
                axis.title.y = element_text(size=25)) + 
  scale_color_discrete(name="Cut of Diamonds") # ??????????????? ???????????몄????? ????????? discrete ??몄?? continuous??몄?? 

# scale_color_discrete
# scale_color_continuous
# scale_shape_discrete
# scale_shape_continuous
print(g4)


# Facets

g5 = g3 + facet_wrap(color~cut, ncol=3)
print(g5)

g6 = g3 + facet_grid(color~cut)
print(g6)


# box plot

library(datasets)
data("airquality")
head(airquality)
dim(airquality)
str(airquality)


airquality$Month = factor(airquality$Month)

pp1 = ggplot(airquality, aes(x=Month, y=Ozone)) + geom_boxplot(fill="red", color="blue")
print(pp1)

pp2 = pp1 + scale_x_discrete(name="Month") + 
  scale_y_continuous(name="Mean Ozone in \nparts per bilion", breaks = seq(0,175, 25), limit(c(0,180))) +
  theme_bw()

pp2

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


############################################# 10강 DB ###################################3

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

dbWriteTable(conn, "goods_new", recode)
#ERROR 3948 (42000): Loading local data is disabled; this must be enabled on both the client and server sides  오류 나오면
# terminal 에서 mysql -u root -p 해서 접속하고
# show global variables like 'local_infile' 하면
# local_infile | off 돼있는데 이걸 on 해줘야함
# set golbal local_infile=true  이렇게 하기
dbGetQuery(conn, "select * from goods_new")


#disconnect 연결 끊기
dbDisconnect(conn)

################################### 11강 기초통계 배우기 ###################################

# 단일집단 모평균 추론


set.seed(1)
n = 100;
x = rnorm(n)
xbar = mean(x)
se = sd(x) / sqrt(n)

# (xbar-t_alpha_2_n_1 *se, xbar+t_alpha_2_n_1 *se) = confident interver(신뢰구간)

t_alpha_2_n_1 = qt(0.05/2, df=n-1)
t_alpha_2_n_1

ci.x = c(xbar-t_alpha_2_n_1 *se, xbar+t_alpha_2_n_1 *se)
ci.x

# compute mean
setwd("/Users/bongsu/Downloads/data/part3")

data = read.csv("one_sample.csv", header=T, fileEncoding = "CP949")
data
dim(data)
head(data)
mean(data$time, na.rm=T)
x = data$time
x1 = na.omit(x)
length(x)
length(x1)

# shapiro-wilk test (H0: x1,...,xn ~ Normaldist)
shapiro.test(x1)


# histogram
hist(x1, freq=F, col="light blue", main="Histogram")
lines(density(x1), col="red")


# QQ plot
length(x1)
qqnorm(x1, pch=16, col="light blue")

qqline(x1, lyt=1, col="red")

# y축 : sort(x1) : 1~109
# x축 : F^-1(i/(n-+1)) n=109 i=1,...,109
# F^-1(1/110),..., F^-1(109/110)

# 양측 검정 (H0 mu=5.2 vs H1: mu!=5.2)

mean(x1)
? t.test
res = t.test(x1, mu=5.2, alter="two.sided", conf.level=0.95) # two.sided는 양측검정 
res

# 단측 검정 (H0: mu=5.2 vs H1: mu>5.2)

res = t.test(x1, mu=5.2, alter="less") # mu>5.2 면 더 큰거니까 greater mu<5.2 면 less
res


################################ 대응 비교 ######################################33

setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("paired_sample.csv")
head(data)

sum(is.na(data))
result = na.omit(data)
dim(result)
mean(result$after)

x = result$before
y = result$after

# 양측 검정 (H0: mu1=mu2, H1: mu1 != mu2)
t.test(x,y,paired=T, alter="two.sided", conf.level = 0.95)
?t.test

# 단측 검정 (H0: mu1=mu2, H1: mu1 < mu2)
t.test(x,y, paired=T, alter="less", conf.level = 0.95)



#############################################

data = read.csv("two_sample.csv")
dim(data)
head(data)
sum(is.na(data[,1]))
sum(is.na(data[,2]))
sum(is.na(data[,3]))
sum(is.na(data[,4]))
sum(is.na(data[,5]))

result = na.omit(data)
dim(result)
table(result$method)

a = subset(result, method==1)$score
b = subset(result, method==2)$score

mean(a)
mean(b)

# 분산의 동질성 검정 ( H0: 두개의 그룹의 분산이 동일하다. H1: 두개의 ㅂ그룹의 분산이 동이랗지 않다.  )

var.test(a,b,alter="two.sided")


# 양측 검정 ( H0: mu1=mu2, H1: mu1!=mu2)
t.test(a,b,alter="two.sided", conf.level=0.95)


# 단측 검정 (H0: mu1=mu2, H2: mu1 < mu2)
t.test(a,b,alter="less", conf.level=0.95)


# 분산분석


data = read.csv("three_sample.csv")
dim(data)
head(data)
sum(is.na(data[,1]))
sum(is.na(data[,2]))
sum(is.na(data[,3]))
sum(is.na(data[,4]))


result = na.omit(data)
dim(result)
sort(result$score, decreasing = T)



plot(result$score)
data2= subset(result, score <=13)
dim(data2)
boxplot(data2$score, col="light blue")

head(data2)
data2$method2[data2$method==1] = "M1"
data2$method2[data2$method==2] = "M2"
data2$method2[data2$method==3] = "M3"

x = table(data2$method2)
y = tapply(data2$score, data2$method2, mean)


library(dplyr)
data2 %>% group_by(method2) %>% summarize(avg =mean(score))

library(plyr)
ddply(data2, .(method2), summarize, avg=mean(score))
#두가지 방법 다 가능


# 세 집단 간 동질성 검정 (H0: 세 집단 분포의 모양이 같다.)
bartlett.test(score~method2, data=data2)

# 분산분석
res = aov(score~method2, data=data2)
res
summary(res)



####################################모비율 추론############################################

#단일 집단 모비율 추론

setwd("/Users/bongsu/Downloads/data/part3")
data = read.csv("one_sample.csv")
data

sum(is.na(data$servey))
table(data$survey) # 0 : 불만족(14) , 1: 만족 (136)
sum(data$survey)/length(data$survey)

# 양측 검정 (H0: p=0.8, H1: p!=0.8)
x = data$survey
binom.test(136, 150, p=0.8, alternative = "two.sided", conf.level=0.95)
binom.test(c(136,14), p=0.8) # 같은 결과
# X ~ Bin(150, 0.8)
pbinom(135, 150, 0.8, lower.tail = F)*2 # *2 는 양ㄱ측검정 # 셋다 같은 결과 (조금의 오차는 있으나 거의 같은결과라 할 수 있음) 136-1 으로 135

# 단측 검정 (H0: p=0.8, H1: p>0.8)
binom.test(136,150, p=0.8, alternative = "greater", conf.level = 0.95)
pbinom(135,150,0.8,lower.tail=F) # 두개가 같음 위에 있는 것 까지 포함하여 어떤 방식으로 하던 다됨 




# 두 집단의 비율 추론
data = read.csv("two_sample.csv")
head(data)
x = data$method
y = data$survey
table(x,y)
#양측 검정 (H0: p1=p2, H1: p1!=p2)
prop.test(c(110,135),c(150,150), alternative = 'two.sided', conf.level=0.95)

#단측 검정 (H0: p1=p2, H1: p1<p2)
prop.test(c(110,135),c(150,150), alternative = 'less', conf.level=0.95)



#세집단의 비율 추론
data = read.csv("three_sample.csv")
head(data)
x = data$method
y = data$survey
table(x,y)

# 세 집단의 비율 차이 검정  (H0: p1=p2=p3, H1: not H0)
prop.test(c(34,37,39), c(50,50,50), alternative = "two.sided", conf.level = 0.95)




######################################## chapter 12#######################################

#적합도 검정

x = data.frame(matrix(c(1,2,3,4,5,41,30,51,71,61), ncol=2))
names(x) = c('prod', 'freq')
x$prop = x$freq / sum(x$freq)
x

# H0: p1=p2=p3=p4=p5=0.2(전부다 동일), H1: not H0
chisq.test(x$freq)

# H0: p1~p5=c(0.2,0.1,0.2,0.3,0.2), H1: not H0
chisq.test(x$freq, p=c(0.2,0.1,0.2,0.3,0.2))



###############독립성 검정(Independence Test)##################

install.packages("gmodels")

library(gmodels)
setwd("/Users/bongsu/Downloads/data/part3")

data = read.csv("cleanDescriptive.csv", fileEncoding = "CP949")
data
dim(data)

x = data$level2
y = data$pass2

table(x)
table(y)


# H0: 부모의 학력수준과 대학진학여부는 관련성이 없다.
# H1: 부모의 학력수준과 대학진학은 관련있다.

chisq.test(x,y)
? chisq.test
CrossTable(x,y, chisq=T)


# 동질성 검정

data = read.csv("homogenity.csv", header=T)
head(data)
dim(data)

CrossTable(data$method, data$survey, chisq=T)
chisq.test(data$method, data$survey)



# chisq test


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

########################################### oop #########################################3

install.packages("pryr")
library(pryr)
?pryr


df = data.frame(x=1:10, y=letters[1:10])
df
otype(df)

methods("mean")
methods("t.test")
methods(class="ts")




# S3 object
foo = structure(list(), class="foo")
class(foo)
inherits(foo, "foo")

# S3 object (2)
foo = list()
class(foo) = "foooo"
class(foo)
inherits(foo,"foooo")
otype(foo)


# S3 function
 
# create a generic function

f = function(x) UseMethod("f")
f

f.a = function(x) "Class a!!"

f.default = function(x) "Unknwon!!!"

obj1 = list(); class(obj1) = "a"
class(obj1)
otype(obj1)
f(obj1)


# create a linear model

mod = lm(log(mpg)~log(disp), data=mtcars)
mod
summary(mod)
class(mod)
otype(mod)
methods("summary")

class(mod) = "newlm"

summary(mod)

summary.newlm = function(object, ...) print("newlm")
summary(mod)

  #### oop 2 ####


# S4 object

library(methods)

setClass("Person", slots=list(name="character", age="numeric"))
setClass("Employee", slots = list(boss="Person"), 
         contains = "Person")

alice = new("Person", name="Alice", age=40)
john = new("Employee", name="john", age=20, boss=alice)
alice
john


alice@name
alice@age
john@boss@name


# create a generic function and method
union
methods("union")
setGeneric("union")
setMethod("union", c(x="data.frame", y="data.frame"), 
          function(x,y) {
            unique(rbind(x,y))
            }
          )
a =data.frame(a=10, b=20)
b =data.frame(a=10, b=30)

union(a,b)

# RC object



Acc1 = setRefClass("Account1", fields=list(balance="numeric"))
Acc2 = setRefClass("Account2", fields=list(balance="numeric"),
                   methods=list(
                     withdraw = function(x) {balance <<-balance - x},
                     deposit = function(x) {balance <<- balance + x}
                   ))

Acc3 = setRefClass("NoOverdraft", 
                   contains = "Account2",
                   methods=list(
                     withdraw = function(x) {
                       if(balance <x) stop("Not enough Money!!!")
                       balance <<- balance - x
                     }
                   ))


a1 = Acc1$new(balance=100)
a1$balance




a2 = Acc2$new(balance=100)
a2$deposit(100)
a2$balance
a2$withdraw(50)
a2$balance


a3  = Acc3$new(balance=100)
a3$deposit(50)
a3$balance
a3$withdraw(1000)


