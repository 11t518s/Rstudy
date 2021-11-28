install.packages('ggplot2')
library(ggplot2)
head(diamonds)
dim(diamonds)
ggplot(diamonds, aes(x=carat, y=price, color=cut))

# layers

g1 = ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth()
 
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + geom_smooth(aes(x=carat, y=price))
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut)) + geom_smooth() #?븘?옒?쐞 媛숈쓬

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
  scale_color_discrete(name="Cut of Diamonds") # ?씠?궛?삎?씠?떈 ?뿰?냽?뀳?씤吏??뿉 ?뵲?씪?꽌 discrete ?씤吏? continuous?씤吏? 

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
# install.packages("rJava")
# install.packages("RJDBC")

library(DBI)
library(RMySQL)  # 맥에서 rjava 랑 RDJBC가 안돼서 다른방법찾을 찾음

drv <- dbDriver("MySQL")
conn <- dbConnect(drv, username="scott", password="tiger", dbname ="work", host="localhost")
dbSendQuery(conn, "SET NAMES utf8;") 
dbSendQuery(conn, "SET CHARACTER SET utf8;") 
dbSendQuery(conn, "SET character_set_connection=utf8;")

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



################################################################################

getwd()
setwd("/Users/bongsu/Desktop/Rstudy")

