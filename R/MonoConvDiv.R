is.MonoDecreasing = function(x,na.rm=F) all(diff(x)<0,na.rm = na.rm)
is.MonoIncreasing = function(x,na.rm=F) all(diff(x)>0,na.rm = na.rm)

is.Converging = function(x,na.rm = F)  is.MonoDecreasing(abs(diff(x)))
is.Diverging = function(x,na.rm = F)  is.MonoIncreasing(abs(diff(x)))

