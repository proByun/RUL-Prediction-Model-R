curve_model = c('WLR', 'WPR', 'WER')

# Information of RUL
RUL <-  function(train_x, train_y, test_x, test_y, time, weight, mean_life, failure_threshold, alpha=0.05, model=model, CL, info=T){
  train_x = as.matrix(train_x)
  train_y = as.matrix(train_y)
  test_x = as.matrix(test_x)
  test_y = as.matrix(test_y)
  t_hat = 0
  model_param = list()
  
  # curve model
  if(model==curve_model[1]){ # Weighted Linear Regression
    WLR = WLR_model(test_x, test_y, time, weight, failure_threshold)
    t_hat = WLR$t_hat
    model_param = WLR
  }else if(model==curve_model[2]){
    train_x = cbind(train_x, train_x^2) # 다항회귀분석(x변수 제곱형태 자동화)
    test_x = cbind(test_x, test_x^2)
    WPR = WPR_model(test_x, test_y, time, weight, failure_threshold)
    t_hat = WPR$t_hat
    model_param = WPR
  }else if(model==curve_model[3]){
    WER = WER_model(test_x, test_y, time, weight, failure_threshold)
    t_hat = WER$t_hat
    model_param = WER
  }
  
  if(test_y[time]>CL){ # predict
    predict_time = t_hat
    rul = predict_time - time
  }else{
    predict_time = mean_life
    rul = predict_time-time
  }
    if(info==T){
      RUL_information_text(time, round(predict_time, 3), round(rul, 3)) # text code
    }else{}
  
    ret = list(
    train_x = train_x,
    train_y = train_y,
    test_x = test_x,
    test_y = test_y,
    model_param = model_param,
    CL = CL,
    mean_life = mean_life,
    failure_threshold = failure_threshold,
    current_time = time,
    predict_failure_time = predict_time,
    rul = rul,
    model = model
  )
  return(ret)
}

# Visualization of RUL
RULplot <-  function(rul, ...){
  plot(rul$test_x[1:rul$current_time,1], rul$test_y[1:rul$current_time], xlab='Time', ylab='Degradation', pch=16,...)
  grid()
  abline(h=rul$CL, col='blue', lwd=2) # Anomaly line: alpha
  abline(h=rul$failure_threshold, col='red', lwd=2) # faulure criteria
  abline(v=rul$current_time, lwd=2, col=adjustcolor(2, alpha=0.5), lty=2)
  #text(1132-20, 0.02, paste('현재시간: ', rul$current_time), cex=1.3)
  #text(1150-20, 0.018, paste('잔여수명: ', round(rul$rul,2)), cex=1.3)
  if (rul$test_y[rul$current_time]>rul$CL){
    if(rul$model  == 'WLR'){
      abline(a=rul$model_param$intercept, b=rul$model_param$slope, col=adjustcolor(11, alpha=0.7), lwd=3)  
    }
    else if(rul$model  == 'WPR'){
      fx <- function(x){
        rul$model_param$slope2*x^2 + rul$model_param$slope1*x + rul$model_param$intercept
      }
      curve(fx, 1, nrow(rul$test_x)+1000, lwd=3, col=adjustcolor(11, alpha=0.7), add=T)      
    }else if(rul$model =='WER'){
    fx <- function(x){
      rul$model_param$a * exp(rul$model_param$b*x)
    }
    curve(fx, 1, nrow(rul$test_x)+1000, lwd=3, col=adjustcolor(11, alpha=0.7), add=T)
    }else{}
  }
}
