library(Quandl)
library(jrvFinance)
duration(cf=c(100,250,300), rate=5e-2)
duration(cf=c(100,250,300),rate = 3e-2,modified = T)
bond.yield(settle="2012-04-15", mature="2022-01-01", coupon=8e-2,
           price=95) 
bond.duration(settle="2012-04-15", mature="2022-01-01", coupon=8e-2,
              yield=8.8843e-2, modified=TRUE)

bond.prices(settle="2012-01-01", mature=c("2022-01-01", "2032-01-01"),
            coupon=c(0.09, 0.08,0.07,0.06), yield=0.10)
bond.yields(settle="2012-01-01", mature=c("2022-01-01", "2032-01-01"),
            coupon=c(0.09, 0.08,0.07,0.06),
            price=c(94, 83, 81, 65))

pacman::p_load(tidyverse,tidyquant,ggfortify,plotly)
baoxian <- tq_get(c('601318.ss','601628.ss'),get='stock.prices',from='2010-01-01')

baoxian <- baoxian  %>% pivot_wider(names_from = symbol,values_from = value)

ggplot(baoxian) +
  geom_point(aes(x=date,y=`601318.ss`,color='red'))+geom_line(aes(x=date,y=`601318.ss`,color='red'))+
  geom_point(aes(x=date,y=`601628.ss`))+
  theme_bw()

p <- ggplot(pingan,aes(x=date,y=value))+geom_point()+geom_line(color='blue')+
  theme_bw() + ggtitle('中国平安分红')
fig <- ggplotly(p)
fig

pacman::p_load(tidyverse,wooldridge,broom,estimatr,stargazer,equatiomatic,performance)
res1 <- lm(lwage~educ+exper+tenure,data=wage1)
extract_eq(res1)
check_model(res1)
extract_eq(res1, use_coefs = TRUE)
  tidy(res1)
fit_2  <- lm_robust(lwage~educ+exper+tenure,data=wage1,se_type = 'stata')
tidy(res2)
stargazer(res1, se = starprep(res1, se_type = 'stata' ),type = 'text')



pacman::p_load(tidyverse,wooldridge,stargazer,equatiomatic)
wage1 <- wooldridge::wage1
res1 <- lm(lwage~educ+exper+tenure,data=wage1)

stargazer(res1,type = 'text')
df <- wage1 %>% select(lwage,educ,exper,tenure) %>% drop_na()
n <- nrow(df)
k <- 3
y <- df$lwage
x <- cbind(1,df$educ,df$exper,df$tenure)
bhat <- solve(t(x) %*% x ) %*% t(x) %*% y
bhat
uhat <- y - x %*% bhat
head(uhat,5)
sigsqhat <- as.numeric(t(uhat) %*% uhat/(n-k-1))
vbetahat <- sigsqhat * solve(t(x) %*% x)  
se <- sqrt(diag(vbetahat))
se






#测试一个动图命令
pacman::p_load(fivethirtyeight,tidyverse,nycflights13,ggthemes,
               gapminder,skimr,moderndive,scales)

gapminder_s <- gapminder %>%
  select(lifeExp, continent, gdpPercap,year)
gapminder_s %>% skim()

# gapminder_s <- gapminder_s %>%
#   group_by(continent,year) %>%
#   summarise(mean_gdp = mean(gdpPercap),mean_life = mean(lifeExp))

gapminder_s
library(barRacer)

bar_chart_race(gapminder_s,cat_col = continent,val_col = mean_life,time_col = year,fps=10,width=600,height=800,duration=10,title = "中文")

gap_tidy <- gapminder_s %>%
  mutate(lifeExp = scale(lifeExp),gdpPercap= scale(gdpPercap)) %>%
  group_by(continent) %>%
  summarise(avglife=mean(lifeExp),avggdp=mean(gdpPercap))%>%
  pivot_longer(col=-continent,names_to = 'name',values_to = 'value')

gap_tidy


p <- gap_tidy %>%
  ggplot(aes(x=continent,y=value,fill=name))+geom_col( position = position_dodge2()) +
  tidyquant::theme_tq() +tidyquant::scale_fill_tq()


library(gganimate)
aim <- p +
  transition_states(continent,1,1,wrap = F) + ease_aes('cubic-in-out') +
  ggtitle('2007年{closest_state}') + enter_fade() +exit_shrink()

aim



evals_ch6
evals_ch6 %>% sample_n(size = 5)

evals_ch6 %>% select(score, age, gender) %>% skim()
library(wooldridge)
data(affairs)
affairs <- as_tibble(affairs)
?affairs
affairs %>% skim()
affairs <- affairs %>%
  select(affair,educ,occup,age,male,vryhap,hapavg,unhap) %>%
  mutate(male= as.factor(male))
affairs %>% get_correlation(formula = affair~educ)



evals_ch6 <- evals %>%
  select(ID, score, age, gender)

p1<-ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_parallel_slopes(se = FALSE) + hrbrthemes::theme_ipsum()+ tidyquant::scale_color_tq()


p2<-ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = 'lm',se=F)+ hrbrthemes::theme_ipsum()+ tidyquant::scale_color_tq()

pacman::p_load(patchwork)

p1 + p2

pacman::p_load(ISLR)

credit_ch6 <- Credit %>% as_tibble() %>% 
  select(ID, debt = Balance, credit_limit = Limit, 
         income = Income, credit_rating = Rating, age = Age)
credit_ch6
credit_ch6 %>% select(debt, credit_limit, income) %>% skim()
credit_ch6 %>% get_correlation(debt ~ credit_limit)
credit_ch6 %>% get_correlation(debt ~ income)
credit_ch6 %>% 
  select(debt, credit_limit, income) %>% 
  cor()
pacman::p_load(corrplot,DT,tidyverse,GGally,gtsummary,psych,GPArotation)
ggpairs(credit_ch6 %>% select(debt, credit_limit, income)) + tidyquant::theme_tq


ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)", 
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)", 
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

tactile_prop_red

library(tidyverse)
library(moderndive)
library(infer)
pacman::p_load(effects,ggeffects,splines)

res <- sample(20:2993,size = 1)
print(res)
