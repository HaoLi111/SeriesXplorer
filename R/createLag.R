createLag = function(x,n=1,Fill=NA) c(rep(Fill,n),x[1:(length(x)-n)])

mapLags = function(x,nSeq){

  #x=1:10;n=1:3
  r = matrix(NA,nrow = length(x),ncol = length(nSeq))
  for(i in nSeq){
    r[,i] = c(rep(NA,i),x[1:(length(x)-i)])
  }
  r
}

parMapLags = function(x,nSeq){
  require("doParallel")
  foreach(i = nSeq,.combine = cbind) %dopar% {
    c(rep(NA,i),x[1:(length(x)-i)])
  }
}

#createLag(1:10,3)
#mapLags(1:10,1:3)
#parMapLags(1:10,1:3)

#registerDoParallel(6)
#System.time({mapLags(500:800,1:20)})
#system.time({parMapLags(500:800,1:20)})
