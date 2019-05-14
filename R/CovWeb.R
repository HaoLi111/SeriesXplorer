#CobWeb Visualizer
requireNamespace('ggplot2')
requireNamespace('RColorBrewer')
requireNamespace('ggthemes')
requireNamespace('scales')

require(ggplot2)
require(RColorBrewer)
require(ggthemes)
require(scales)


cobDf = function(x){
  l = length(x)



  nIter = rep(1:l,each=2)
  xcob = rep(x,each = 2)
  ycob = c(0,xcob[3:l],NA)
  data.frame(nIter = nIter, xcob=xcob,ycob=c(0,xcob[3:l],NA),
                     isVertical=rep(1:0,times = l))
}


#xbase = seq(from = -1, to = 1,by=1e-2)
updateF = function(x) 1-.1*x^2


#Title =paste0("lambda%==%",L)


plotCobDf = function(CobDf,xbase=NULL,nClip = 6,limType=0,
                     Title='Cob Web Method',
                     updateF=NULL,
                     parseTitle=T,
                     updateDf=NULL,engine ="gg"){

  if(is.null(xbase)) xbase= updateDf$xb
  if(!is.null(nClip)) CobDf=CobDf[(1:nClip),]

  if(engine == "gr"){

    if(limType==0){
      #plot with x,y lim of  Cob Web
      plot(CobDf$xcob,CobDf$ycob,lty=3,col=3,type="b",
           xlab = expression(x[n]),ylab = expression(x[n+1]))
      lines(xbase,xbase,lty=2,col=2)
      if(is.null(updateF)){
        lines(updateDf$xb,updateDf$yf,type="l",col=1,lty=1)
      }else{
        lines(xbase,updateF(xbase),type="l",col=1,lty=1)
      }
    }else{
      #plot with  x, y lim of xbase Df

      plot(xbase,xbase,lty=2,col=2)
      if(is.null(updateF)){
        lines(updateDf$xb,updateDf$yf,type="l",col=1,lty=1)
      }else{
        lines(xbase,updateF(xbase),type="l",col=1,lty=1)
      }
      lines(CobDf$xcob,CobDf$ycob,lty=3,col=3,type="b",
            xlab = expression(x[n]),ylab = expression(x[n+1]))
    }
    plot(CobDf$xcob,CobDf$ycob,lty=3,col=3,type="b",
         xlab = expression(x[n]),ylab = expression(x[n+1]))
    lines(xbase,xbase,lty=2,col=2)
    if(is.null(updateF)){
      lines(updateDf$xb,updateDf$yf,type="l",col=1,lty=1)
    }else{
      lines(xbase,updateF(xbase),type="l",col=1,lty=1)
    }

    grid()

    if(isTRUE(parseTitle)){
      title(parse(text=Title))
    }else{
      title(Title)
    }
  }else if(engine =='gg'){

    if(limType==0){

    requireNamespace('ggplot2')
    requireNamespace('RColorBrewer')
    requireNamespace('ggthemes')
    requireNamespace('scales')
    #Clip #CobDf = CobDf[1:12,]
    #Cscale =brewer.pal(l-1,"Spectral")
    #xbase=seq(from=min(xbase),to=max(xbase),length.out = l-1)




    lineDf = data.frame(xb=xbase,yb=xbase)
    if(is.null(updateDf)) updateDf = data.frame(xb=xbase,yf=updateF(xbase))
    g=ggplot(data=CobDf) +
      geom_point(aes(x=xcob,y=ycob,col=nIter),size = 3.5,
                 alpha = .6) +
      geom_path(aes(x=xcob,y=ycob,col = nIter),alpha =1,size=1.25) +
      scale_color_distiller(direction=-1
                            ,palette = "Spectral")

    g=g+
      geom_abline(slope=1,col = "red",size=1.25,lty=3,alpha=.5) +
      geom_line(data=updateDf,aes(x=xb,y=yf),color='black',size=1.25,lty=2,alpha=.5)


      g=g +
      coord_cartesian(xlim = c(min(CobDf$xcob,na.rm=T),max(CobDf$xcob,na.rm=T)),
                      ylim = c(min(CobDf$ycob,na.rm=T),max(CobDf$ycob,na.rm=T)))


    g=g+xlab(expression(x[n])) + ylab(expression(x[n+1]))


    if(isTRUE(parseTitle)){
      g=g+ggtitle(as.expression(Title))
    }else{

      g=g+ggtitle(Title)
    }



    g=g+theme_pander()#theme_solarized(light=F)#theme_solarized_2(light = FALSE)
    return(g)
    #ggsave("g.tif",g,device = 'tiff')
    }else{


      requireNamespace('ggplot2')
      requireNamespace('RColorBrewer')
      requireNamespace('ggthemes')
      requireNamespace('scales')

      lineDf = data.frame(xb=xbase,yb=xbase)
      if(is.null(updateDf)) updateDf = data.frame(xb=xbase,yf=updateF(xbase))
      g=ggplot(data=CobDf) +
        geom_point(aes(x=xcob,y=ycob,col=nIter),size = 3.5,
                   alpha = .6) +
        geom_path(aes(x=xcob,y=ycob,col = nIter),alpha =1,size=1.25) +
        scale_color_distiller(direction=-1
                              ,palette = "Spectral")

      g=g+
        geom_abline(slope=1,col = "red",size=1.25,lty=3,alpha=.5) +
        geom_line(data=updateDf,aes(x=xb,y=yf),color='black',size=1.25,lty=2,alpha=.5)


      #g=g +
       # coord_cartesian(xlim = c(min(CobDf$xcob,na.rm=T),max(CobDf$xcob,na.rm=T)),
       #                 ylim = c(min(CobDf$ycob,na.rm=T),max(CobDf$ycob,na.rm=T)))


      g=g+xlab(expression(x[n])) + ylab(expression(x[n+1]))
      if(isTRUE(parseTitle)){
        g=g+ggtitle(as.expression(Title))
      }else{

        g=g+ggtitle(Title)
      }
      g=g+theme_pander()#theme_solarized(light=F)#theme_solarized_2(light = FALSE)
      return(g)
    }
  }
}


#plotCobDf(cobDf(x)[1:12,],nClip=NULL,xbase=(-100:100)/100,updateF=updateF,Title,engine='gg')
