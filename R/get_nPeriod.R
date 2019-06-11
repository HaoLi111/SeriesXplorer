detect_nPeriod = function(x,ths = 2^6,tlr = 1e-6){
  l=length(x)
  if(ths>l) message("threshold value larger than length of x!")
  xl =x[l]
  for(i in 1:ths){
    p=i
    if(abs(x[l-i]-xl)<=tlr) break
  }
  p
}

#detect_nPeriod(rep(1:100,4))
