pacman::p_load(tidyquant,tidyverse,timetk,Tushare,DT,plotly )
options(scipen = 100)
options(digits = 3)
api <- pro_api(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')
bar <- pro_bar(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')

ZS <- api(api_name = 'index_basic',market = 'SSE')
ZS[str_which(ZS$ts_code,'000905'),]  # 找到000905的具体ts_code


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

df <- rbind(pingan,zgrs,zgtb,gsyh,zz500)

stock_long <- df %>%
  select(ts_code, trade_date,close) %>% 
  mutate(trade_date = as.Date(trade_date,format="%Y%m%d"),close = as.numeric(close))

str(stock_long)

stock_long_m_r <-stock_long %>%
  group_by(ts_code) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = "Ra")


head(stock_long_m_r)
num_port <- 2000
all_wts <- matrix(nrow = num_port,
                 ncol = 5) # 这里用5，

port_returns <- vector('numeric', length = num_port)
port_risk <- vector('numeric', length = num_port)
sharpe_ratio <- vector('numeric', length = num_port)

for (i in seq_along(port_returns)){
  wts <- runif(5) ## 构成组合的资产数量
  wts <- wts/sum(wts)
  all_wts[i,] <- wts

  port_return <- stock_long_m_r %>%
    tq_portfolio(assets_col = ts_code,returns_col = Ra,
                 weights = wts,col_rename = "p")

  port_returns[i] <- mean(port_return$p)

  port_risk[i] <- sd(port_return$p)

  sharpe_ratio[i] <- mean(port_return$p)/sd(port_return$p)

}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- c('中国平安','中国人寿','中国太保','工商银行','中证500')
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
min_var
max_sr

min_var_long <- min_var %>%
  pivot_longer(cols = -c(Return,Risk,SharpeRatio) ,names_to = "names" , values_to = "weight") %>%
  mutate(names = as.factor(names))


g<-ggplot(min_var_long,aes(x = fct_reorder(names,weight), y = weight, fill = names)) +
  geom_bar(stat = 'identity') + theme_tq() + tidyquant::scale_fill_tq() + xlab('各类资产')+
  ylab('权重')+scale_y_continuous(labels = scales::percent) +
  ggtitle('模拟500次选出的投资组合方差最小的权重比例')

ggplotly(g)


max_sr_long <- max_sr %>%
  pivot_longer(cols = -c(Return,Risk,SharpeRatio) ,names_to = "names" , values_to = "weight") %>%
  mutate(names = as.factor(names))


g_2<-ggplot(max_sr_long,aes(x = fct_reorder(names,weight), y = weight, fill = names)) +
  geom_bar(stat = 'identity') + theme_tq() + tidyquant::scale_fill_tq() + xlab('各类资产')+
  ylab('权重')+scale_y_continuous(labels = scales::percent) +
  ggtitle('模拟500次选出的投资组合夏普比率最大的权重比例')

ggplotly(g_2)




g_3 <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  labs(x = '年化风险',
       y = '年化收益',
       title = "投资组合优化与有效边界")

ggplotly(g_3)

library(tseries)
# 将收益率数据转换为宽形
# 我们从stock_long_daily_r开始



wide_return_m <- stock_long_m_r %>%
  pivot_wider(names_from = ts_code,values_from = Ra)

wide_return_m <- wide_return_m %>%
  tk_xts(silent = T)

res<-tseries::portfolio.optim(wide_return_m)
weight_m<-res$pw
name <- c('中国平安','中国人寿','中国太保','工商银行','中证500')

wei <- tibble(weight = weight_m , name = name)
g5 <-ggplot(wei ,aes(x=name,y=weight,fill=name)) + geom_col() +
  theme_tq() + tidyquant::scale_fill_tq() +
  scale_y_continuous(labels = scales::percent)
print(res$pm/res$ps)
ggplotly(g5)

res_test<-tseries::portfolio.optim(wide_return_m, pm = 1.05*mean(wide_return_m)) 
weight_test<-res_test$pw
print(res)
print(res_test)

print(weight_test)
head(wide_return_m)
barplot(weight_m)


