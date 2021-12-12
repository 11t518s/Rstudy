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

