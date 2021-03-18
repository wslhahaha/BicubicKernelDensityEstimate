bicubicKernelFunction <- function(x){
    #双核密度函数
    #Args:
    #    x: x轴范围
    #Returns:
    #    获得x对应的双核密度函数
    
    (1 - min(abs(x),1) ^ 3) ^ 3 * 70 / 81
}

f <- function(x,y,h){
    #密度函数
    #Args:
    #    x: x轴范围
    #    y: 随机生成的随机数
    #    h: h = 1.06 * S * n ^ (-1/5)
    #Returns:
    #    获得x对应的密度估计函数

    sum(apply(matrix((x - y) / h),1,bicubicKernelFunction)) / (h * length(y))
}

kde <- function(x,y,yTrue,titleOn){
    #kde作图
    #Args:
    #    x: x取值范围的100个等间隔格子点
    #    y: 随机生成的随机数
    #    yTrue: 真实的密度函数
    #    titleOn: 生成图的标题
    #Returns:
    #    None
    
    h = 1.06 * sd(y) * length(y) ^(-1 / 5) #窗宽
    z = apply(matrix(x),1,f,y,h)
    plot(x,z,col="#f0932b")
    lines(x, yTrue, col="#95afc0")
    legend("topright", c("拟合密度函数", "真实密度函数"), col = c("#f0932b", "#4834d4"),lty = c(1),text.font = 12)
    title(main = paste("双三核密度函数估计",titleOn))
}


set.seed(101)

#标准正态分布(参数,0,1)
y <- rnorm(1000)
x = seq(min(y),max(y),length.out = 100) #获得x取值范围的100个等间隔格子点
kde(x,y,dnorm(x),"(正态分布)")

#指数分布(默认参数为1)
y <- rexp(1000)
x = seq(min(y),max(y),length.out = 100) #获得x取值范围的100个等间隔格子点
kde(x,y,dexp(x),"(指数分布)")

#gamma分布(2,1)
y <- rgamma(1000,2,1)
x = seq(min(y),max(y),length.out = 100) #获得x取值范围的100个等间隔格子点
kde(x,y,dgamma(x,2,1),"(gamma分布)")

#beta(5,5)
y <- rbeta(1000,5,5)
x = seq(min(y),max(y),length.out = 100) #获得x取值范围的100个等间隔格子点
kde(x,y,dbeta(x,5,5),"(beta分布)")
