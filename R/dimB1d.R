boxCount = function(x,...) UseMethod('boxCount')
boxCount.numeric = function(x,grid){
  n=0
  for(i in 2:length(grid)){
    if(any(x < grid[i] & x>=grid[i-1])) n=n+1
  }
  n
}

#boxCount.numeric(c(1,2,2,2,5,8,10-.5),0:10)

#n = size/s
#log(s,(1/n))

dim_B_1d =function(x,s=NULL,
                   frame =F){
  n<-numeric(length(s))

  if(is.null(s)) s = diff(range(x))/length(x)/10
  for(i in seq_along(s)){
    Grid = seq(from = min(x),to=max(x),by = s[i])
    Grid = c(Grid,Grid[length(Grid)]+s)
    n[i] = boxCount(x=x,Grid)
  }

  if(isTRUE(frame)) return(data.frame(s=s,n=n,dim = log(n,base = 1/s)))
  boxDim_Hausdorff = log(n,base = 1/s)
  boxDim_Hausdorff
}
#dim_B_1d(x = 1:10,seq(from =.1,to=2,by=.1),frame=F)

ggdim_B_Df = function(x){
  ggplot2::ggplot(data=as.data.frame(x),aes(x =log(1/s),y = log(n),color = dim)) + geom_point() +
    geom_smooth(method = 'lm')
}

#library(ggplot2)
#ggdim_B_Df(dim_B_1d(x = 1:10,seq(from =.1,to=2,by=.1),frame=T))
#dim_B_1d(1:10,.0000001)


