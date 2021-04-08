setwd('--')

##### Libraries and data import #####
library('readxl')
library('scorecard')
library('dplyr')
library('data.table')
library('caret')
library('e1071')
library('xtable')

data <- read_excel('Project 2 - Data.xls')
data <- na.omit(data)
PRIME_DATA = data


##### Data adjustments #####

setDT(data)[, .N,keyby = CUSTOMER_ID][, .N, keyby = N]  
# clearly many clients have applied multiple times
# however all the observations are relevant because factors reasoning (dis)approval may have changed
# and consequently client is somewhat independent of himself with respect to time

####
# setDT zamienilo to na data.table, wiec wygodniej juz tym sortowac. Ponadto, kod jest bardziej czytelny
####

data <- data[,c('CUSTOMER_ID','ASSESSMENT_YEAR'):=NULL]          #we delete CUSTOMER_ID and ASSESSMENT_YEAR as they are irrelevant for our analysis
data <- data[, INDUSTRY := as.factor(as.character(INDUSTRY))]
data <- data[, DEFAULT_FLAG := as.numeric(DEFAULT_FLAG)]
data <- data[, GROUP_FLAG := as.numeric(GROUP_FLAG)]
str(data)

##### Heatmap #####

corrCols <- colnames(data)[which(as.vector(data[,lapply(.SD, class)]) == "numeric")]
heatmap(cor(as.matrix(data[,..corrCols])))
# ACCESS_CREDIT is highly correlated with MEDIUM and SHORT_TERM_LIQUIDITY

##### Subseting; data analysis in groups ####
set.seed(1)
# we divide sample into train (80%) and test (20%) #
inTrain <- data[,sample(.N, floor(.N*.8))]
Train <- data[inTrain]
Test <- data[-inTrain]
# now we check if distribution is comparable between the samples:
par(mfrow = c(1,1))
#regressand (dependant variable)
barplot(c(table(Train$DEFAULT_FLAG)/sum(table(Train$DEFAULT_FLAG)),table(Test$DEFAULT_FLAG)/sum(table(Test$DEFAULT_FLAG))), 
        main = "Split of data", 
        names.arg = c('0 Train', '1 Train', '0 Test', '1 Test'),
        xlab="Independently between Train and Test",
        ylim = c(0,1),
        font.lab=1,
        font.axis=2,
        col.lab="orange", 
        cex.lab=1,
        col = c('gray', 'black'))

#some of regressors
par(mfrow = c(2,4))
regressors = c('GROUP_FLAG', 'ACCESS_CREDIT', 'PROFITABILITY', 'SHORT_TERM_LIQUIDITY')
for(i in regressors){
  hist(as.numeric(unlist(Train[,..i])), main = 'Train', xlab = i)
  hist(as.numeric(unlist(Test[,..i])), main = 'Test', xlab = i)
}
#distribution of defaults is more or less similar in test and train sample 

#now we calculate Information Value of each regressor 

##### IV #####
IV <- iv(Train, y="DEFAULT_FLAG")
IV

##### Logisitic regression #####
#Let's try to build a logistic regression model with the usage of all variables 
# full model
m_full <- glm(DEFAULT_FLAG~.,family=binomial(),data=Train)
summary(m_full)
pred_Test_full <- predict(m_full,type='response', Test)
pred_Test_full <- ifelse(pred_Test_full>0.5,1,0)

#without variables with low IV and strong correlation (TURNOVER,GROUP_FLAG,SHORT_TERM_LIQUIDITY,ACCESS_CREDIT)

##### Brute force analysis (results in nest section) #####
model_accuracy = function(pred, actuals){
  DT = data.table(pred = pred, actuals = actuals)
  M = matrix(c(DT[pred == 1 & actuals == 1, .N],
               DT[pred == 0 & actuals == 1, .N],
               DT[pred == 1 & actuals == 0, .N],
               DT[pred == 0 & actuals == 0, .N]), 2, 2)
  TP = M[1,1]; FN = M[2,1]; FP = M[1,2]; TN = M[2,2]
  return((TP+TN)/(TP+TN+FP+FN))
}

model_creator = function(Train, Test, arg = 'DEFAULT_FLAG'){
  cols = colnames(Train)
  cols = cols[cols != arg]
  
  DT = rbindlist(sapply(1:(length(cols)), function(i) as.data.frame(t(combn(cols, i)))), fill = TRUE)
  
  x = which(DT[1,!is.na(.SD)])
  vect = c(arg,unlist(unname(DT[1,..x])))
  DT_temp = Train[,..vect]
  
  glm.out = glm(DEFAULT_FLAG ~., family=binomial(logit), data=DT_temp)
  
  pred = ifelse(predict(glm.out,type = 'response', Test)>0.5,1,0)
  score = model_accuracy(pred, Test$DEFAULT_FLAG)
  
  for(i in 2:nrow(DT)){
    x = which(DT[i,!is.na(.SD)])
    vect0 = c(arg,unlist(unname(DT[i,..x])))
    DT_temp0 = Train[,..vect0]
    
    glm.out0 = glm(DEFAULT_FLAG ~., family=binomial(logit), data=DT_temp0)
    
    pred0 = ifelse(predict(glm.out0,type = 'response', Test)>0.5,1,0)
    score0 = model_accuracy(pred0, Test$DEFAULT_FLAG)
    
    if(score0 > score){
      vect = vect0; DT_temp = DT_temp0
      glm.out = glm.out0; pred = pred0; score = score0
      
    }
  }
  return(glm.out)
}

###
# AIC + accuracy #
###
model_creator2 = function(Train, Test, arg = 'DEFAULT_FLAG'){
  cols = colnames(Train)
  cols = cols[cols != arg]
  
  DT = rbindlist(sapply(1:(length(cols)), function(i) as.data.frame(t(combn(cols, i)))), fill = TRUE)
  
  x = which(DT[1,!is.na(.SD)])
  vect = c(arg,unlist(unname(DT[1,..x])))
  DT_temp = Train[,..vect]
  
  glm.out = glm(DEFAULT_FLAG ~., family=binomial(logit), data=DT_temp)
  
  pred = ifelse(predict(glm.out,type = 'response', Test)>0.5,1,0)
  score = model_accuracy(pred, Test$DEFAULT_FLAG)
  aic = AIC(glm.out)
  
  for(i in 2:nrow(DT)){
    x = which(DT[i,!is.na(.SD)])
    vect0 = c(arg,unlist(unname(DT[i,..x])))
    DT_temp0 = Train[,..vect0]
    
    glm.out0 = glm(DEFAULT_FLAG ~., family=binomial(logit), data=DT_temp0)
    
    pred0 = ifelse(predict(glm.out0,type = 'response', Test)>0.5,1,0)
    score0 = model_accuracy(pred0, Test$DEFAULT_FLAG)
    aic0 = AIC(glm.out0)
    
    if(score0 > score & aic0 < aic){
      vect = vect0; DT_temp = DT_temp0
      glm.out = glm.out0; pred = pred0; score = score0
      
    }
  }
  best_model_regressors <<- vect
  return(glm.out)
}

##### Brute force analysis results #####
mc = model_creator(Train,Test)
summary(mc)
# according to accuracy criterion, this one is the best

best_model = model_creator2(Train,Test)
summary(best_model)
# according to accuracy combined with aic criterion, it stays the same

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
xtable(summary(best_model))

# With the Indstry changed #
dTrain = as.data.frame(Train)
dTest = as.data.frame(Test)

Train[,INDUSTRY_Machinery_and_Computer_Ind := as.numeric(INDUSTRY == 'Office Machinery and Computer Industries')]
Train[,INDUSTRY_Property_and_Construction_Sectors := as.numeric(INDUSTRY == 'Property and Construction Sectors')]
Train[,INDUSTRY := NULL]

Test[,INDUSTRY_Machinery_and_Computer_Ind := as.numeric(INDUSTRY == 'Office Machinery and Computer Industries')]
Test[,INDUSTRY_Property_and_Construction_Sectors := as.numeric(INDUSTRY == 'Property and Construction Sectors')]
Test[,INDUSTRY := NULL]

model_best2 = model_creator2(Train,Test)
xtable(summary(model_best2))
summary(model_best2)

# Hopefully stays the same #
Train_new = Train
Test_new = Test

Train = as.data.table(dTrain)
Test = as.data.table(dTest)




pred_Test_best = ifelse(predict(best_model,type = 'response', Test)>0.5,1,0)

##### By hand approach #####
m_filtered <- glm(DEFAULT_FLAG~ + PRODUCT_DEMAND 
                  + OWNERS_MANAGEMENT 
                  + PROFITABILITY 
                  + MEDIUM_TERM_LIQUIDITY 
                  + INDUSTRY,
                  family=binomial(),data=Train)
summary(m_filtered)

#we can see that statistically significant level of factor variable INDUSTRY is
#"Property and Construction Sectors" 
#in both models, so we can try to modify this factor so as it tells us whether
#the company is/isn't in this particular sector 

#now let's check the accuracy of our models 


pred_Test_filt <- predict(m_filtered,type='response',Test)
#we change the probabilities into binomial variable 
pred_Test_filt <- ifelse(pred_Test_filt>0.5,1,0)
#Now we use confusion matrix and related statistics to compare our models


##### More in detail analysis #####
cm_full <- caret::confusionMatrix(factor(Test$DEFAULT_FLAG) , factor(pred_Test_full))
cm_filt <- confusionMatrix(factor(Test$DEFAULT_FLAG) , factor(pred_Test_filt))
cm_best <- confusionMatrix(factor(Test$DEFAULT_FLAG), factor(pred_Test_best))

print(cm_full)
print(cm_filt)
print(cm_best)

##### AIC approach for full and by hand model #####
#we can observe a small rise in accuracy of our model
#now we use akaike criterion to 

m_full_akaike <- step(m_full, direction="both", trace=FALSE)
m_filt_akaike <- step(m_filtered,direction='both',trace=FALSE)
summary(m_full_akaike)
summary(m_filt_akaike)
#so as to finally check whether it improves the accuracies of our models

pred_full_Ak <- predict(m_full_akaike,type='response', Test)
pred_filt_Ak <- predict(m_filt_akaike,type='response',Test)

pred_full_Ak <- ifelse(pred_full_Ak>0.5,1,0)
pred_filt_Ak <- ifelse(pred_filt_Ak>0.5,1,0)

cm_full_ak <- caret::confusionMatrix(factor(Test$DEFAULT_FLAG) , factor(pred_full_Ak))
cm_full_ak
cm_filt_ak <- confusionMatrix(factor(Test$DEFAULT_FLAG) , factor(pred_filt_Ak))
cm_filt_ak

##### Probit Regression #####

mp_full <- glm(DEFAULT_FLAG~.,family=binomial(link = 'probit'),data=Train)
summary(mp_full)

mp_filt <- glm(DEFAULT_FLAG~ + PRODUCT_DEMAND 
               + OWNERS_MANAGEMENT 
               + PROFITABILITY 
               + MEDIUM_TERM_LIQUIDITY 
               + INDUSTRY,
               family=binomial(link='probit'),data=Train)
summary(mp_filt)

##### Cross Validation #####
#now we perform repeated cross-validation with 10 folds, which is optimal
#number of folds if we want to maintain the reasonable level of bias and variance

### tutaj ustawiamy zmienna pomocnicza do funkcji train, wybieramy liczbe warstw i liczbe losowan calego podzialu
### czyli u nas 5 razy wykonujemy 10 warstwowa crosswalidacje za kazdym razem na innym podziale

set.seed(123)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# We cross-validate all 3 models 

model_full <- train(factor(DEFAULT_FLAG) ~., data = Train, method = "glm", family = binomial(),
                    trControl = train.control)

#######Niestety trzeba recznie podawac te modele w tej funkcji za kazdym razem

model_filtered <- train(factor(DEFAULT_FLAG) ~ + PRODUCT_DEMAND 
                        + OWNERS_MANAGEMENT 
                        + PROFITABILITY 
                        + MEDIUM_TERM_LIQUIDITY 
                        + INDUSTRY,  data = Train, method = "glm", family = binomial(),
                        trControl = train.control)

model_best <- train(factor(DEFAULT_FLAG) ~.,data = Train[,..best_model_regressors], method = "glm", family =binomial(),trControl = train.control)

# We summarize the results

print(model_full)
print(model_filtered)
print(model_best)

# The kappa statistic is a measurement of the accuracy of a  
# model while taking into account chance. The closer the value is to 1 the better.
# We can observe that the models get very similar results. 
# Even though a slight advantage suggests to use model_filtered, we may hold to using model_best
# because it uses less regressors
# However one may still debate which one is superior

##### Graphic tools #####
dataset <- var_filter(PRIME_DATA, "DEFAULT_FLAG", return_rm_reason = FALSE)
bins <- woebin(dataset, "DEFAULT_FLAG")
dev.off()
woebin_plot(bins)  


# Plots with WoE profile:
for (i in 1 : length(bins)){
  x = ggplot(data.frame(x = bins[[i]]$bin, y = bins[[i]]$woe),aes(x=x,y=y))+
    geom_bar(stat="identity", fill="steelblue")+
    labs(title = bins[[i]]$variable[1])+ xlab('Bin') +ylab(NULL)+
    theme_bw()
  print(x)
}

##### Linear regression #####
model_creator_lm = function(Train, Test, arg = 'DEFAULT_FLAG'){
  cols = colnames(Train)
  cols = cols[cols != arg]
  
  DT = rbindlist(sapply(1:(length(cols)), function(i) as.data.frame(t(combn(cols, i)))), fill = TRUE)
  
  x = which(DT[1,!is.na(.SD)])
  vect = c(arg,unlist(unname(DT[1,..x])))
  DT_temp = Train[,..vect]
  
  lm.out = lm(DEFAULT_FLAG ~., data=DT_temp)
  
  pred = ifelse(predict(lm.out,type = 'response', Test)>0.5,1,0)
  score = model_accuracy(pred, Test$DEFAULT_FLAG)
  aic = AIC(lm.out)
  
  for(i in 2:nrow(DT)){
    x = which(DT[i,!is.na(.SD)])
    vect0 = c(arg,unlist(unname(DT[i,..x])))
    DT_temp0 = Train[,..vect0]
    
    lm.out0 = lm(DEFAULT_FLAG ~., data=DT_temp0)
    
    pred0 = ifelse(predict(lm.out0,type = 'response', Test)>0.5,1,0)
    score0 = model_accuracy(pred0, Test$DEFAULT_FLAG)
    aic0 = AIC(lm.out0)
    
    if(score0 > score & aic0 < aic){
      vect = vect0; DT_temp = DT_temp0
      lm.out = lm.out0; pred = pred0; score = score0
      
    }
  }
  best_model_regressors <<- vect
  return(lm.out)
}

l_model_full = lm(DEFAULT_FLAG ~., data=Train)

xtable(summary(l_model_full))

l_model_best = model_creator_lm(Train,Test)

xtable(summary(l_model_best))

summary(l_model_full)
summary(l_model_best)

dTrain = as.data.frame(Train)
dTest = as.data.frame(Test)

Train[,INDUSTRY_Machinery_and_Computer_Ind := as.numeric(INDUSTRY == 'Office Machinery and Computer Industries')]
Train[,INDUSTRY_Property_and_Construction_Sectors := as.numeric(INDUSTRY == 'Property and Construction Sectors')]
Train[,INDUSTRY := NULL]

Test[,INDUSTRY_Machinery_and_Computer_Ind := as.numeric(INDUSTRY == 'Office Machinery and Computer Industries')]
Test[,INDUSTRY_Property_and_Construction_Sectors := as.numeric(INDUSTRY == 'Property and Construction Sectors')]
Test[,INDUSTRY := NULL]

summary(lm(DEFAULT_FLAG~.,data=Train))

l_model_best = model_creator_lm(Train,Test)
xtable(summary(l_model_best))

Train[,PROFITABILITY := NULL][,INDUSTRY_Machinery_and_Computer_Ind := NULL][,MEDIUM_TERM_LIQUIDITY:= NULL][,ACCESS_CREDIT := NULL][,TURNOVER:=NULL]
l_model_filtered = lm(DEFAULT_FLAG~., data = Train)
summary(l_model_filtered)
xtable(summary(l_model_filtered))

l_model_best = model_creator_lm(Train,Test)
summary(l_model_best)

Train_new = Train
Test_new = Test

Train = as.data.table(dTrain)
Test = as.data.table(dTest)

##### Experts' model #####
Test[,Expert_Prediction := 1/(1+exp(-0.1*(.2*PRODUCT_DEMAND + .1*OWNERS_MANAGEMENT + .1*ACCESS_CREDIT + .15*PROFITABILITY +
                                            .25*SHORT_TERM_LIQUIDITY + .2*MEDIUM_TERM_LIQUIDITY)))]
min(Test[,Expert_Prediction])
max(Test[,Expert_Prediction])

func = function(data, threshold){
  pred = ifelse(data[,Expert_Prediction]>threshold, 1, 0)
  actuals = data[,DEFAULT_FLAG]
  DT = data.table(pred = pred, actuals = actuals)
  M = matrix(c(DT[pred == 1 & actuals == 1, .N],
               DT[pred == 0 & actuals == 1, .N],
               DT[pred == 1 & actuals == 0, .N],
               DT[pred == 0 & actuals == 0, .N]), 2, 2)
  TP = M[1,1]; FN = M[2,1]; FP = M[1,2]; TN = M[2,2]
  TPR = TP/(TP+FN)
  FPR = FP/(FP+TN)
  return(c(FPR,TPR))
}

x = lapply(seq(0,1,0.005),func, data = Test)
x = unlist(x)
df = data.frame(FPR = x[seq(1,length(x),2)], TPR = x[seq(1,length(x),2)+1])
AUC = sum((-df$FPR[-1]+df$FPR[-length(df$FPR)])*(df$TPR[-1]+df$TPR[-length(df$FPR)])/2)
plot = ggplot(df,(aes(x = FPR, y = TPR)))+
  geom_point(colour = 'red', size = 1.5)+
  geom_line(colour = 'blue', size = .5)+
  labs(title = paste('ROC curve', "Experts' model", '  AUC =', AUC))+
  theme_bw()
print(plot)

model_accuracy(ifelse(Test[,Expert_Prediction]>0.5, 1, 0), Test$DEFAULT_FLAG)
Test = as.data.table(dTest)

##### ROC Curve #####
roc = function(model, data, regressand){
  creator_FPR_TPR = function(pred, actuals){
    DT = data.table(pred = pred, actuals = actuals)
    M = matrix(c(DT[pred == 1 & actuals == 1, .N],
                 DT[pred == 0 & actuals == 1, .N],
                 DT[pred == 1 & actuals == 0, .N],
                 DT[pred == 0 & actuals == 0, .N]), 2, 2)
    TP = M[1,1]; FN = M[2,1]; FP = M[1,2]; TN = M[2,2]
    TPR = TP/(TP+FN)
    FPR = FP/(FP+TN)
    return(c(FPR,TPR))
  }
  predicitator = function(threshold, model, data){
    prediction = predict(model, type = 'response', data)
    prediction[which(prediction<0)] = 0
    prediction = ifelse(prediction>=threshold,1, 0)
    return(prediction)
  }
  all_together = function(threshold, model, data, regressand){
    return(creator_FPR_TPR(predicitator(threshold,model,data), unlist(data[,..regressand])))
  }
  x = lapply(seq(0,1,0.005),all_together, model = model, data = data, regressand = regressand)
  x = unlist(x)
  df = data.frame(FPR = x[seq(1,length(x),2)], TPR = x[seq(1,length(x),2)+1])
  AUC = sum((-df$FPR[-1]+df$FPR[-length(df$FPR)])*(df$TPR[-1]+df$TPR[-length(df$FPR)])/2)
  plot = ggplot(df,(aes(x = FPR, y = TPR)))+
    geom_point(colour = 'red', size = 1.5)+
    geom_line(colour = 'blue', size = .5)+
    labs(title = paste('ROC curve', substitute(model), '  AUC =', AUC))+
    theme_bw()
  return(plot)
}

# For subsequent models:
# Best model
print(roc(m_full, Test, 'DEFAULT_FLAG'))

print(roc(best_model, Test, 'DEFAULT_FLAG'))

print(roc(m_filtered, Test, 'DEFAULT_FLAG'))

print(roc(m_filt_akaike, Test, 'DEFAULT_FLAG'))
# probit
print(roc(mp_filt, Test, 'DEFAULT_FLAG'))

# Linear
print(roc(l_model_full, Test, 'DEFAULT_FLAG'))

print(roc(l_model_best, Test_new, 'DEFAULT_FLAG'))

print(roc(l_model_filtered, Test_new, 'DEFAULT_FLAG'))

##### Useful calculations #####
model_accuracy(ifelse(predict(m_full, type = 'response', Test) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(m_filtered, type = 'response', Test) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(best_model, type = 'response', Test_new) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(mp_filt, type = 'response', Test) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(l_model_full, type = 'response', Test) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(l_model_filtered, type = 'response', Test_new) > 0.5, 1, 0), Test$DEFAULT_FLAG)
model_accuracy(ifelse(predict(l_model_best, type = 'response', Test_new) > 0.5, 1, 0), Test$DEFAULT_FLAG)


auc = function(model, data, regressand = 'DEFAULT_FLAG'){
  creator_FPR_TPR = function(pred, actuals){
    DT = data.table(pred = pred, actuals = actuals)
    M = matrix(c(DT[pred == 1 & actuals == 1, .N],
                 DT[pred == 0 & actuals == 1, .N],
                 DT[pred == 1 & actuals == 0, .N],
                 DT[pred == 0 & actuals == 0, .N]), 2, 2)
    TP = M[1,1]; FN = M[2,1]; FP = M[1,2]; TN = M[2,2]
    TPR = TP/(TP+FN)
    FPR = FP/(FP+TN)
    return(c(FPR,TPR))
  }
  predicitator = function(threshold, model, data){
    prediction = predict(model, type = 'response', data)
    prediction[which(prediction<0)] = 0
    prediction = ifelse(prediction>=threshold,1, 0)
    return(prediction)
  }
  all_together = function(threshold, model, data, regressand){
    return(creator_FPR_TPR(predicitator(threshold,model,data), unlist(data[,..regressand])))
  }
  x = lapply(seq(0,1,0.005),all_together, model = model, data = data, regressand = regressand)
  x = unlist(x)
  df = data.frame(FPR = x[seq(1,length(x),2)], TPR = x[seq(1,length(x),2)+1])
  AUC = sum((-df$FPR[-1]+df$FPR[-length(df$FPR)])*(df$TPR[-1]+df$TPR[-length(df$FPR)])/2)
  return(AUC)
}
