#ROC Df

rocDf = function(x,
                 AssumingSingleConv = F,
                 xSymConv=NULL){
  Dx = c(NA,diff(x));absDx = abs(Dx);logAbsDx = log(absDx)
  log10AbsDx =(log10(absDx))
  if(isFALSE(AssumingSingleConv)){
    data.frame(x=x,
               Dx=Dx,
               absDx = absDx,
               logAbsDx=logAbsDx,
               log10AbsDx=log10AbsDx)
  }else{
    if(is.null(xSymConv)){
      xSymConv=x[length(x)]
      stopifnot((!is.na(xSymConv)))


    }

    xErr = x-xSymConv
    absxErr =abs(xErr)
    logAbsxErr = log(absxErr)
    log10AbsxErr = log10(absxErr)


    data.frame(x=x,
               Dx=Dx,
               absDx = absDx,
               logAbsDx=logAbsDx,
               log10AbsDx=log10AbsDx,
               xErr =xErr,
               absxErr =absxErr,
               logAbsxErr=logAbsxErr,
               log10AbsxErr=log10AbsxErr)
  }

}


extractConsBoundDf = function(x,Df,nCons){
  p_cmin = is.Cons_Min(x,nCons)
  p_cmax = is.Cons_Max(x,nCons)
  Df_cmin=Df[as.vector(ifelse(!is.na(p_cmax) &p_cmin==1,TRUE,FALSE)),]
  Df_cmax=Df[ifelse(!is.na(p_cmax) & p_cmax==1,TRUE,FALSE),]

  list(Df_cmin = Df_cmin,
       Df_cmax = Df_cmax)
}
#listCons(x,3)

#find max in n consecutives

plotRocDfDx = function(RocDf){
  ggplot(RocDf,aes(x = n,y = Dx,color = n)) +
    geom_point(size=.2,alpha = .85) +
    geom_path(size=.1,alpha = .2) +
    scale_color_distiller(direction=-1
                          ,palette = "Spectral") +
    theme_pander()

}


plotRocDfLogAbsDx = function(RocDf){
  ggplot(RocDf,aes(x = n,y = logAbsDx,color = n)) +
    geom_point(size=.2,alpha = .85) +
    geom_path(size=.1,alpha = .2) +
    scale_color_distiller(direction=-1
                          ,palette = "Spectral") +
    theme_pander()
}
