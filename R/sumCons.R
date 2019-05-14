sumCons = function(x,n = 3){
  r=0
  l=length(x)
  for(i in 1:n){
    r = r + x[i:(l-n+i)]
  }
  r
}
#avgCons = function(x,n=3) sumCons(x,n=n)/n
#sumCons(1:10)
#sumCons(1:10,5)

listCons = function(x,nSeq){
  r = list()
  for (i in seq_along(nSeq)){
    r[[i]] = x[i:(l-n+i)]
  }
}
