###实际建模操作
###确定隐藏层节点数
wine=read.table("winequality-white.csv") # 本文默认数据以记事本格式存储于电脑D盘中
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")															# 为每一个变量命名
set.seed(71)
wine=wine[sample(1:4898,3000),]
nrow.wine=dim(wine)[1]

###原始数据归一化程序
scale01=function(x)
{
	ncol=dim(x)[2]-1
	nrow=dim(x)[1]
	new=matrix(0,nrow,ncol)
	for(i in 1:ncol)
	{
		max=max(x[,i])
		min=min(x[,i])
		for(j in 1:nrow)
		{
			new[j,i]=(x[j,i]-min)/(max-min)
		}
	}
	new
}
cha=0	# 设置中间变量对处理后的向量进行临时存储
for(i in 1: nrow.wine) # 针对每一个样本进行调整
{
	if(wine[i,12]>6)
	{
		cha[i]="good"	# 将品质大于6的样本品质定义为“good”
	}
	else if(wine[i,12]>5)
	{
		cha[i]="mid"	# 将品质大于5却不大于6的样本品质定义为“mid”
	}
	else
	{
		cha[i]="bad"	# 将品质不大于5的样本品质定义为“bad”
	}
}
wine[,12]=factor(cha)	# 将字符型变量转化为含有因子的变量并复制给数据集wine
set.seed(444)
samp=sample(1:nrow.wine, nrow.wine*0.7) 	# 从总样本集中抽取70%的样本作为训练集
wine[samp,1:11]=scale01(wine[samp,])		# 对训练集样本进行预处理
wine[-samp,1:11]=scale01(wine[-samp,])		# 对测试集样本进行预处理
r=1/max(abs(wine[samp,1:11]))			# 确定参数rang的变化范围
n=length(samp)
err1=0
err2=0
for(i in 1:17)
{
	set.seed(111)
	model=nnet(quality~.,data=wine,maxit=400,rang=r,size=i,subset=samp,decay=5e-4)
	err1[i]=sum(predict(model,wine[samp,1:11],type='class')!=wine[samp,12])/n
	err2[i]=sum(predict(model,wine[-samp,1:11],type='class')!=wine[-samp,12])/(nrow.wine -n)
}
plot(1:17,err1,'l',col=1,lty=1,ylab="模型误判率",xlab="隐藏层节点个数",ylim=c(min(min(err1),min(err2)),max(max(err1),max(err2))))
lines(1:17,err2,col=1,lty=3)
points(1:17,err1,col=1,pch="+")
points(1:17,err2,col=1,pch="o")
legend(1,0.53,"测试集误判率",bty="n",cex=1.5)
legend(1,0.35,"训练集误判率",bty="n",cex=1.5)
