#Minkowsky distance
minkowsky = function(x,y,p){
  if(p >= 1)
  {
      d_inc = sum((abs(x - y)**p), na.rm=T)
      d = d_inc**(1/p)
      return(d)
  }
  else{
    return(NaN)
  }

}

# p = c(1, 2, 3)
# q = c(4, 5, 6)
# type = 3
# # 
# # # type < 1, triangle inequality violation
# # # type = 1, Manhattan
# # # type = 2, Euclidian
# # # type -> inf, Chebyshev
# # 
# minkowsy_dist = minkowsky(p,q, type)
# minkowsy_dist
