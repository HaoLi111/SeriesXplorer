plotSeries=function(x,...) UseMethod("plotSeries")

plotSeries.data.frame = function(x,#Grutils/ggplot2/plot2Drgl
                                  colVar = 1,
                                  aspect = "default"
                                  #1-nIter;2-Value;0-NULL
                                  ){
  if(identical(aspect,"default")){

    require(ggplot2)
    require(ggthemes)
    require(RColorBrewer)
    p=ggplot(data =x, aes(x = n,y=x,color = n)) +
      geom_point(size=.2,alpha = .85) +
      geom_path(size=.45,alpha = .65) +
      scale_color_distiller(direction=-1
                            ,palette = "Spectral") +
      theme_pander()
  }
  if(identical(aspect,"2dMap")){
    nr =nrow(x)
    DF = hcatSeq(data.frame(x = as.numeric(x$x)[-nr,drop=T],x_nPlus1 = as.numeric(x$x)[-1,drop=T]))
    p = ggplot(data=DF,aes(x = x,y = x_nPlus1,col = n)) + geom_point(size=.2,alpha = .85) +
      geom_path(size=.45,alpha = .3) +
      scale_color_distiller(direction=-1
                            ,palette = "Spectral") +
      theme_pander()
  }

  return(p)
}

