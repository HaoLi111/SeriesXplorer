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
    if(is.NULL(xSymConv)){
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


#Fibnacci
x=numeric(20)
x[1]=1
x[2]=1
for(i in 3:10){
  x[i]=x[i-1]+x[i-2]
}


plotRocDf_Dx = function(Df)
