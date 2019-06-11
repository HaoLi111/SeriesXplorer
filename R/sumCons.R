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

listCons = function(x,n){
  r = list()
  l=length(x)
  for (i in 1:n){
    r[[i]] = x[i:(l-n+i)]
  }
  return(r)
}


hcatCons = function(x,n){
  r = matrix(NA,length(x)-n+1,n)
  l=length(x)
  for (i in 1:n){
    r[,i] = x[i:(l-n+i)]
  }
  return(r)
}

#hcatCons(x,3)


is.Cons_Min = function(x,n){
  ind = apply(hcatCons(x,n),1,which.min)
  ind[ind!=1]=0
  c(ind,rep(NA,n-1))
}

is.Cons_Max = function(x,n){
  ind = apply(hcatCons(x,n),1,which.max)
  ind[ind!=1]=0
  c(ind,rep(NA,n-1))
}

