is.chaotic = function(x,report = F,plot=F){
  L = 1/length(x)-1*sum(log(abs(diff(x))))
  if(isFALSE(report)) return(L>0)
  #report =true\
  #layout(1:2)
  PowerSpectra = psd::pspectrum(ts(x,frequency=1),plot = plot)
  dim_B = dim_B_1d(x,s = c(diff(range(x))/length(x)/10,diff(range(x))/length(x)/5),frame = T)
  #print(ggdim_B_Df(dim_B))
  #plot_dim_B_Df(dim_B)
  p =detect_nPeriod(x,ths =length(x)-1,tlr = 1e-6)
  return(list(Lyapunov_Lambda =L,
       PSpectrum = PowerSpectra,
       Recurrence.with.ths = p,
       dim_B = dim_B))

}
#is.chaotic(x)
#library(psd)
#is.chaotic(x)

parIs.chaotic = function(x){
  re = foreach(i = 1:4,.combine =list,.packages=c('psd','SeriesXplorer')) %dopar%{
    if(i == 1){
      L = 1/length(x)-1*sum(log(abs(diff(x))))
      return(L)
    }
    if(i==2){
      PowerSpectra = psd::pspectrum(ts(x,frequency=1),plot = F)
      return(PowerSpectra)
    }
    if(i==3){
      Recurrence.with.ths = detect_nPeriod(x,ths =length(x)-1,tlr = 1e-6)
      return(Recurrence.with.ths)
    }
    if(i==4){
      dim_B = dim_B_1d(x,s = c(diff(range(x))/length(x)/10,diff(range(x))/length(x)/5),frame = T)
      #print(ggdim_B_Df(dim_B))
      return(dim_B)
    }
  }
  return(re)

}

#require(foreach)
#install.packages("doParallel")
#require(doParallel)
#registerDoParallel(4)

#system.time(is.chaotic(x,report =T))
#system.time(parIs.chaotic(x))

#is.chaotic(x,report =T)
#report = parIs.chaotic(x)
#report[[1]][[2]]
