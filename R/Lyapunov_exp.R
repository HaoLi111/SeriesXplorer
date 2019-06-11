Lyapunov_exp = function(x){
  dx = diff(x)
  n = length(dx)
  1/n*sum(log(abs(dx)))#/x[-length(x)]
}
