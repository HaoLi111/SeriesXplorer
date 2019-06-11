#Fibnacci
n=7
x=numeric(n)
x[1]=as.integer(1)
x[2]=as.integer(1)
for(i in 3:n){
  x[i]=as.integer(x[i-1]+x[i-2])
}








#logistic Map


nIteration=1000
#x=.4
x = numeric(nIteration)

x[1]=.4
a=.4





L=1.3
for (i in 2:nIteration){
  x[i]=a
  a=1-L*a^2
}


require(ggplot2)
require(RColorBrewer)
require(ggthemes)
require(scales)

plotCobWeb(x[1:500],updateDf=data.frame(xb=(-100:100)/100,
                                 yf=1-L*((-100:100)/100)^2))+ ggtitle(as.expression(paste0("Lambda=",L)))


CobDf =cobDf(x)
upDateDf=data.frame(xb=(-100:100)/100,
                    yf=1-L*((-100:100)/100)^2)
g =plotCobDf(CobDf,nClip =NULL,
             limType =1,
          xbase = (-100:100)/100,
          updateDf = upDateDf,
          engine = 'gg')

g











RocDf = rocDf(x,AssumingSingleConv = T)

RocDf = hcatSeq(RocDf)

plot(RocDf$n,RocDf$Dx,type = 'l')
plot(RocDf$logAbsDx,type='l')
plot(RocDf$log10AbsDx,type='l')
plot(RocDf$n,RocDf$x,type='l',lty=1,col = 'red')
plot(x)

nCons = 2

lines(Df_cmin$n,Df_cmin$x,lty=3,col='grey')
lines(Df_cmax$n,Df_cmax$x,lty=3,col='grey')



for(nCons in 2:5){
  p_cmin = is.Cons_Min(x,nCons)
  p_cmax = is.Cons_Max(x,nCons)

  Df_cmin=RocDf[as.vector(ifelse(!is.na(p_cmax) &p_cmin==1,TRUE,FALSE)),]
  Df_cmax=RocDf[ifelse(!is.na(p_cmax) & p_cmax==1,TRUE,FALSE),]
  lines(Df_cmin$n,Df_cmin$x,lty=3,col='blue')
  lines(Df_cmax$n,Df_cmax$x,lty=3,col='blue')
}


plot(RocDf$n,RocDf$Dx,type='b')

p_cmin = is.Cons_Min(RocDf$Dx,nCons)
p_cmax = is.Cons_Max(RocDf$Dx,nCons)

Df_cmin=RocDf[as.vector(ifelse(!is.na(p_cmax) &p_cmin==1,TRUE,FALSE)),]

Df_cmax=RocDf[ifelse(!is.na(p_cmax) & p_cmax==1,TRUE,FALSE),]
lines(Df_cmin$n,Df_cmin$Dx,lty=3,col='blue')
lines(Df_cmax$n,Df_cmax$Dx,lty=3,col='blue')

#RocDf = hcatSeq(RocDf)

plot(RocDf$n,RocDf$xErr,type='b')


str(RocDf)

g=ggplot(data = RocDf, aes(x = n,y=x,color = n)) +
  geom_point(size=.2,alpha = .85) +
  geom_path(size=.1,alpha = .2) +
  scale_color_distiller(direction=-1
                        ,palette = "Spectral") +
  theme_pander()
g



plotRocDfDx(RocDf)
plotRocDfLogAbsDx(RocDf)

