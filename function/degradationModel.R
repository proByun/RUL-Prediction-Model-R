degradation_model = function(residual){
  Comment = "
    @Description
      Degradation Factor
      Degradation Factor is used to transformed Anomaly scores and applied to predict Remaining Useful Lifecycle
      
    @Param
      anomalyScore : anomalyScore
      
    @Return
      degradation statistics
  "
  
  residual = as.matrix(residual)
  # residual euclidean norm
  residual_ed_norm = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    residual_ed_norm[,i] =  sqrt(residual[,i]^2)
  }
  # accumulated degradation model
  accum_degradation = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    accum_degradation[,i] =  cumsum(residual_ed_norm[,i])
  }
  
  # degradation model
  n = 1:nrow(residual)
  degradation = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    degradation[,i] =  cumsum(residual_ed_norm[,i])/n
  }
  return(degradation)
}