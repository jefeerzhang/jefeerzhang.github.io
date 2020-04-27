library(stargazer)
data(mtcars)
lm1 <- lm(mpg ~ cyl, data = mtcars)
lm2 <- lm(mpg ~ cyl + hp, data = mtcars)


library(huxtable)
library(magrittr)
huxreg(lm1, lm2,
       coefs=c('Cylinders' = 'cyl',
               'Horsepower' = 'hp')) %>%
  # We can send it to the screen to view it instantly
  print_screen()


huxreg(lm1, lm2,
       coefs=c('Cylinders' = 'cyl',
               'Horsepower' = 'hp')) %>%
  # Let's make an HTML file
  quick_html(file = 'my_reg_output.rtf')



pacman::p_load(tidyquant,tidyverse,timetk,Tushare,plotly,DT ) #掉包，使用pacman可以一次调完

options(scipen = 100) 
options(digits = 3) 

api <- pro_api(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')
bar <- pro_bar(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')

ZS <- api(api_name = 'index_basic',market = 'SSE')
ZS[str_which(ZS$ts_code,'000905'),]  # 找到000905的具体ts_code

# 下面为读取数据
zz500 <- api(api_name = 'index_daily',ts_code='000905.SH',start_date="20140101",
             end_date='20200407') 

pingan <- bar(ts_code='601318.SH',start_date="20140101",
              end_date='20200407',adj="qfq")
zgrs <- bar(ts_code='601628.SH',start_date="20140101",
            end_date='20200407',adj="qfq")
zgtb <- bar(ts_code='601601.SH',start_date="20140101",
            end_date='20200407',adj="qfq")
gsyh <- bar(ts_code='601398.SH',start_date="20140101",
            end_date='20200407',adj="qfq")

df <- rbind(pingan,zgrs,zgtb,gsyh,zz500) # 合并数据

fun1 <- function(){
stock_long <- df %>%  # 数据处理，选择三列，将交易日转换为时间类型，收盘价转换为数值型
  select(ts_code, trade_date,close) %>% 
  mutate(trade_date = as.Date(trade_date,format="%Y%m%d"),close = as.numeric(close))
}

library(tidyfst)
fun2 <- function(){
stock <- df %>% 
  select_dt(ts_code, trade_date,close) %>%
  mutate_dt(trade_date = as.Date(trade_date,format="%Y%m%d"),close = as.numeric(close))
}

library(microbenchmark)
res <-microbenchmark(fun1(),fun2(),times = 1000)
res
autoplot(res) + theme_bw() + ggtitle('性能差异')
  



library(tidyverse)
library(tidyfst)

fun1 <- function(){
iris %>%
  mutate_dt(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>%
  select_dt(group,sl,sw) %>%
  filter_dt(sl > 5) %>%
  arrange_dt(group,sl) %>%
  summarise_dt(sw = max(sw),by = group)
}

fun2 <- function(){
  iris %>%
  mutate(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>%
  select(group,sl,sw) %>%
  filter(sl > 5) %>%
  arrange(group,sl) %>%
  group_by(group) %>%
  summarise(sw = max(sw))
}

res <-microbenchmark(fun1(),fun2(),times = 1000)
res
autoplot(res) + theme_bw() + ggtitle('性能差异')


pacman::p_load(tidyverse,gganimate)
df <- tibble(x= c(-100,30,60,40),y=2020:2023) %>% mutate(y = anytime::anydate(as.character(y)) )

 
p <- ggplot(df,aes(x=y,y=x))+geom_col(fill='blue',alpha =0.4,width = 60) + tidyquant::theme_tq()+
  tidyquant::scale_fill_tq() +geom_path() + geom_hline(yintercept = 0 ,color='red')+
  labs( x = '时间', y = '现金流',title='一个简单的例子')+
  geom_text(mapping = aes(label = x),size = 6, vjust =0, hjust=-0.5)+
  theme(plot.title = element_text(size=22))+
p
gif <- p + transition_reveal(y)
 
animate(gif,  duration=10)
anim_save('2.gif')





pacman::p_load(tidyverse,gganimate)
x <-c() # 建立两个空集，用于存储循环数据
y <-c()
for ( i in seq(0,0.2,by = 0.0001)){ # 建立for循环，起点终点步长
  npv = -100+ 30/(1+i)+60/(1+i)^2+40/(1+i)^3
  x = append(x,i)
  y = append(y,npv)
}
res <- tibble(irr=x,npv=y) #将循环数据存储为数据集
#作图
p2 <- ggplot(res,aes(x=irr,y=npv)) + geom_path(size=1.2)+
  geom_hline(yintercept = 0 ,color='red',alpha=0.6,size=1.2) +
  tidyquant::theme_tq() + xlab('贴现率')+ylab('对应NPV') +
  ggtitle('2000次模拟后IRR的求解思路') +theme(plot.title = element_text(size=22))
gif2 <- p2+ transition_reveal(npv)
animate(gif2,  duration=15)
anim_save('3.gif')



t= seq.Date(from = as.Date("2020/04/01",format = "%Y/%m/%d"), by = "month", length.out = 13)
cf <- tibble(cf=c(-50000,190,310,300,310,310,300,
                  310,300,310,310,280,50310),
             t=Date("2020/04/17",format = "%Y/%m/%d"), by = "month", length.out = 12)

cf_2 <- tibble(cf = c(-50000,rep(4324.16,12)),t=t)


p3 <- ggplot(cf_2,aes(x=t,y=cf)) + geom_path(size=1.1)+
  geom_hline(yintercept = 0 ,color='red',alpha=0.6) +
  tidyquant::theme_tq() + xlab('时间(月)')+ylab('现金流') +
  ggtitle('2000次模拟后IRR的求解思路') +theme(plot.title = element_text(size=22))+
annotate("text", x= , y= 250 )
p3



cf <- tibble(period= 0:65,exp = 0)
cf <- cf %>%
  mutate(exp = if_else(period %in% seq(0,9),-6000,0)) %>%
  mutate(income  = if_else(period %in% seq(3,65,by = 2),3000,0))
cf[66,"income"] <- 300000
#上一行是为最后一期赋值

#展示一下现金流
cf<-cf %>%  mutate(pmt = exp+income) %>%
  mutate(cum = cumsum(pmt)) 

# cf %>% datatable(rownames = F, caption = "该保险的现金流",
#                  colnames = c('年份','支出','收益','净收益','累计收益'))
#计算净收益的irr
irr <- cf %>%
  select(pmt) %>% 
  pull() %>%
  irr()
#利用jrv包进行计算，注意40行用了一个pull函数
paste("该保险产品的IRR是：",irr)
p<-ggplot(cf,aes(x=period,y=cum))+geom_path(size=1.1)+
theme_bw()+geom_hline(yintercept = 0,color='red')+
  xlab('时间')+ylab('累计收益')+ggtitle('该保险的非贴现的累计收益')+
  theme(plot.title = element_text(size=20))
p
fig <- p+transition_reveal(period)
animate(fig,  duration=14)
anim_save('4.gif')



pacman::p_load(tidyverse)
cf1 <- tibble(cf1=c(-50000,190,310,300,310,310,300,
                  310,300,310,310,280,50310),t=seq(0:12))

cf2 <- tibble(cf2 = c(-50000,rep(4324.16,12)),t=seq(1,13))

df <- cf1 %>% inner_join(cf2,by='t')
df <- df %>%
  pivot_longer(cols = -t,names_to = "group",values_to = "cf") 

p<-ggplot(df,aes(x=t,y=cf,color=group))+geom_path(size = 1.1,alpha=0.6) +
  tidyquant::theme_tq()  +
  xlab("时间") +ylab('现金流')+ggtitle('支付宝借呗两种还款方式')+
  theme(plot.title = element_text(size=20)) +
  labs(color='借呗两种还款方式')+
  scale_color_manual(labels = c("先利后本", "每月等额"), values = c("blue", "red"))
library(gganimate)
fig <- p+transition_reveal(t)
animate(fig,  duration=12)
anim_save('5.gif')      



pacman::p_load(tidyverse)
min_rank(runif(10))

n_distinct(diamonds['carat'])
           


mod.pres <- lm(prestige ~ log(income, 10) + 
                 poly(education, 3) + 
                 poly(women, 2),
               data=Prestige)    

a <- ggeffects::ggeffect(mod.pres,terms = 'education')
str(a)
plot(a) + tidyquant::theme_tq() + xlab('教育')


mod.cowles <- glm(volunteer ~ sex + 
                    neuroticism + extraversion +
                    neuroticism:extraversion,
                  data=Cowles, family=binomial)

a <- ggeffects::ggeffect(mod.cowles,terms = c("neuroticism","extraversion"))
plot(a) + tidyquant::theme_tq()
stargazer::stargazer(mod.cowles,type = 'text')
