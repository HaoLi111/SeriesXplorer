hcatSeq = function(x,...) UseMethod("hcatSeq")
hcatSeq.numeric=function(x,...) data.frame(n = seq_along(x),x=x)
hcatSeq.data.frame = function(x,...){
  n = data.frame(n = seq_along(x$x))
  cbind(n,x)
}
