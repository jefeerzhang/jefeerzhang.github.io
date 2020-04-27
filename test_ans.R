first_func <- function(num){
  count = 0  # 初始化
  for (i in num ){
    if (i %% 10 == 0){
      count = count +1 
    }
  }
  return(count)
}
first_func(c(0, 25, 30, 35, 40))



sec_func <- function(names){
  kong = c()
  for(i in names){
    k = paste("Hello, ",i)
    kong = append(kong,k)
  }
  return(kong)
}
sec_func(c('张','李','王'))


third_func <- function(lst){
  for (i in lst){
    if (i %% 2 == 0){
      lst = lst[-1]
    } else {break}
  }
  return(lst)
}
third_func(c(4, 8, 10, 11, 12, 15))
third_func(c(4,8,10))



fourth_func<-function(lst){
  new_list = c()
  for (i in seq(1:length(lst))){
    if (i %% 2 != 0 ){
      new_list = append(new_list,lst[i])
    }
  }
  return(new_list)
}
fourth_func(c(4, 3, 7, 10, 11, -2))



firth <- function(bases,powers) {
  res = c()
  for (i in bases){
    for (j in powers){
      res = append(res,i**j)
    }
  }
  return(res)
}
firth(c(2,3,4),c(1,2,3))