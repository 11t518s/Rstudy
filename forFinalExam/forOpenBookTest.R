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
