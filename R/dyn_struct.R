#Struct sequence

dyn_struct = function(FUN,Level = 0,n=1000,Init = NULL){
  if(Level ==0){
    return(sapply(1:n,FUN))
  }
  if(Level>0){
    stopifnot(!is.null(Init))

    for(i in (Level+1):n ) Init = c(Init,FUN(Init[(length(Init)-Level+1):length(Init)]))
    return(Init)
  }
}
