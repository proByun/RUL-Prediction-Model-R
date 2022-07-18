# WMSE based Hatmatrix (solve)
W_Hatmatrix_solve = function(x, y, weight){
  library(MASS)
  x = as.matrix(x)
  y = as.matrix(y)
  
  w = as.matrix(weight)
  beta0 = matrix(1, nrow(x), 1)
  x = cbind(beta0, x)  
  
  # WMSE based Hatmatrix
  w_beta = solve(t(x)%*%w%*%x) %*% t(x)%*%w %*% y

  ret = w_beta
  return(w_beta)  
}

# WMSE based Hatmatrix (ginv)
W_Hatmatrix_ginv = function(x, y, weight){
  library(MASS)
  x = as.matrix(x)
  y = as.matrix(y)
  
  w = as.matrix(weight)
  beta0 = matrix(1, nrow(x), 1)
  x = cbind(beta0, x)  
  
  # WMSE based Hatmatrix
  w_beta = ginv(t(x)%*%w%*%x, tol=sqrt(.Machine$double.eps)^10) %*% t(x)%*%w %*% y
  
  ret = w_beta
  return(w_beta)  
}