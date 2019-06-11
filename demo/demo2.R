require(ggplot2)
require(RColorBrewer)
require(ggthemes)
require(scales)


nIteration=20
x=.4
x = numeric(nIteration)

x[1]=.4
a=.4
L=.1
for (i in 1:nIteration){
  x[i]=a
  a=1-L*a^2
}

plotCobDf(cobDf(x)[1:16,],nClip=NULL,limType = 0,xbase=(-100:100)/100, updateDf =data.frame(xb=(-100:100)/100,yf = 1-L*((-100:100)/100)^2),
          parseTitle=F,engine='gg')

