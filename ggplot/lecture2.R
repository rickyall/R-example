library('ggplot2')
data('diamonds')
set.seed(1410)#让样本可重复
dsmall <- diamonds[sample(nrow(diamonds),100),]

#2.3基本用法
qplot(carat,price,data = diamonds)
