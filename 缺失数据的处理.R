#使用pacman包加载包,先行安装pacman
# 加载包，加载接口
pacman::p_load(Tushare,tidyverse,tidyquant,DT,anytime)
api <- pro_api(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')
bar <- pro_bar(token = '5adce34e8c81bf7085828754a8e09590c3630032d0f61aad6483eaaa')

## 600318 600555海航创新 601601 中国太保
pingan <- bar(ts_code='601318.SH',start_date="20140101",
              end_date='20200407',adj="qfq")
hhcx <- bar(ts_code='600555.SH',start_date="20140101",
            end_date='20200407',adj="qfq")
zgtb <- bar(ts_code='601601.SH',start_date="20140101",
            end_date='20200407',adj="qfq")

nrow(pingan)
nrow(hhcx)
nrow(zgtb)

# 发现hhcx的行(交易天数只有1405天，其他两个样本交易天数基本相同)
# 样本的交易日期不一样，导致后面相关系数缺失
# 解决办法匹配时以最少的那个公司为基准

# 用交易日期作为key进行合并
df <- hhcx %>% left_join(pingan,by='trade_date',suffix=c('600555','601318') ) %>% left_join(zgtb,by='trade_date')
nrow(df) # 这下合并后的数据集只有1405行了

# 筛选df的一些列 tidyverse里面的select比较灵活，比如我们这里使用starts_with函数
# 选出以某些字符串开头的列,将tsrade_date转换为日期格式
df <- df %>% select(starts_with(c('trade','close'))) %>%
  as_tibble() %>% mutate(trade_date = as.Date(trade_date,format = '%Y%m%d'))
dim(df)
df # 看一眼数据
# 选择后df有7列，1405行
# 把最后一个close换一下名字
df <- df %>%
  rename(close601601 = close)
df
# 接下来进行长宽数据转换，将数据转化为长型
df_long <- df %>%
  pivot_longer(cols = starts_with("close") ,names_to = "code",
               values_to = 'close')
df_long 

df_long_m_ret <- df_long %>%
  group_by(code) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'm_return')


#月收益均值
df_long_m_ret_mean <- df_long_m_ret %>%
  group_by(code) %>%
  summarise(mean_ret = mean(m_return))
df_long_m_ret_mean

# 将收益率数据转化为日期类型，进而计算收益率方差协方差矩阵,先转换为宽数据，再转化为时间序列

j <- df_long_m_ret %>%
  pivot_wider(names_from = code , values_from = m_return) %>% tk_xts(silent = T)
str(j)
# 看一看我们转换的数据的结构
head(j)
#画图
pacman::p_load(corrplot)
j %>% cor() 
j%>%cor %>% corrplot(method = 'number' )
