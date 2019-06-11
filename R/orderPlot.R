#map of orders
MapOrderPlot = function(FUN,base=seq(from=-1,to=1,by=.01),n=5){
  l = length(base)
  xb= rep(base,time = n)
  n_order = rep(1:n,each = l)
  #n_order
  yf = base
  yfRec =NULL
  for(i in 1:n){
    yf= FUN(yf)
    yfRec = c(yfRec,yf)
  }
  orderDf = data.frame(xb =xb,yf = yfRec,n = n_order)

  library(ggplot2)
  library(ggthemes)
  library(RColorBrewer)
  ggplot(orderDf,aes(x = xb,y = yf,col = n))+ geom_point(alpha = .75)+
    geom_path(alpha=.2) +
    scale_color_distiller(direction=-1
                          ,palette = "Spectral") +
    theme_pander()
}

#MapOrderPlot(FUN = ( function(x) 1-1.7*x^2))


