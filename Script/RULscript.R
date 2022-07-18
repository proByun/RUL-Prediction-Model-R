# rm(list=ls())


src_dir = 'E:\\R\\RUL(정밀가공 과제)\\function\\' # 경로 설정
src_file = list.files(src_dir)
for(i in 1:length(src_file)) source(paste(src_dir, src_file[i], sep=''), encoding='utf-8')

wd =  'E:\\R\\RUL(정밀가공 과제)\\data\\RMS_bearing.csv' # 경로 설정
data = read.csv(wd)

# 1~600개 데이터는 정상데이터라 가정
train = data[1:600,]
train_mat = as.matrix(train)

test = data
test_mat = as.matrix(test)

# MSET linear regression
msetLR= mset_regress(train_mat, test_mat)

# degradation model with mset linear regression
trDegradation = degradation_model(msetLR$residual_tr)
tsDegradation = degradation_model(msetLR$residual_ts)

# RUL Parameter
train_x = 1:400
train_y = rowSums(trDegradation)
test_x = 1:nrow(tsDegradation)
test_y = rowSums(tsDegradation)

# control limit
cl = bootlimit(train_y, alpha=0.05, m=1000)

# 가중선형회귀 모델 가중치
# alpha가 클수록 현재상태 많이 반영
# alpha가 작을수록 과거시점 많이 반영 
w = exponential_weight(test_x, test_y, alpha=0.05)

# 고장 기준
failure_threshold = max(test_y) 

# RUL 예측 코드
# model : 
# - weighted Linear Regression : WLR
# - weighted Polynomial Regression : WPR
# - weighted Exponential regression : WER
rul = RUL(train_x, train_y, test_x, test_y, time=720, weight=w, mean_life=1000, failure_threshold, alpha=0.05, model='WLR', CL=cl, info=T)

# visualization of RUL
RULplot(rul, ylim=c(min(rul$test_y), max(rul$test_y)), xlim=c(0,1000))
