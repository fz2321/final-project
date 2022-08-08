library(ggplot2)
library(stringr)
library(TTR)

oil_ETF <- read.csv("oil _ETF.csv")
PPI <- read.csv("PPI.csv")
x_oil <- oil_ETF$Close
y_ppi <- PPI$Close
date <- oil_ETF$Date
df1 <- data.frame(date, x_oil, y_ppi)
fun1 <- lm(df1$y_ppi~df1$x_oil)
summary(fun1)
anova(fun1)

ggplot(data= df1, aes(x = x_oil, y = y_ppi)) + geom_smooth(method='lm')+geom_point() + labs(title = "linear regression of oil price and ppi")

fun2 <- function(column){
  n <- length(column)
  result <- vector(mode="numeric",length=n)
  for(i in 1:(n-1)){
    result[i+1] = column[i+1]/column[i]-1
  }
  return(result)
}
USO = fun2(df1$x_oil)
PPI = fun2(df1$y_ppi)

DATE <- sapply(str_split(df1$date,'/2022'),'[',1)
df2 = data.frame(DATE,USO, PPI) 

USO_swa = SMA(df2[,2],n=3)
PPI_swa = SMA(df2[,3],n=3)
df2 = cbind(df2,USO_swa,PPI_swa)

#Add the MA
ggplot(data= df2) + labs(title = "Scatter Plot") +
  geom_point(aes(x = DATE, y = PPI),color='darkred') + 
  geom_point(aes(x = DATE, y = USO),color='darkblue')+ 
  geom_line(aes(x = DATE, y = PPI_swa),linetype = "dashed",group = 1)+
  geom_line(aes(x = DATE, y = USO_swa),group = 1) 


#Only the legend
df2$class2 <- 'ppi'
df2$class1 <- 'uso'
dat1 <- data.frame(DATE = df2$DATE,value = df2$USO,swa = df2$USO_swa,class = df2$class1)
dat2 <- data.frame(DATE = df2$DATE,value = df2$PPI,swa = df2$PPI_swa,class = df2$class2)
df2 = rbind(dat1,dat2)
ggplot(data= df2) + labs(title = "Scatter Plot") +
  geom_point(aes(x = DATE, y = value,color= class)) 

#MA line and Legend
ggplot(data= df2) + labs(title = "Scatter Plot") +
  geom_point(aes(x = DATE, y = value,color= class)) + 
  geom_line(aes(x = DATE, y = swa, color=class),group = 1)