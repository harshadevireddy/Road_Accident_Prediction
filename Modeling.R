#Exploratory data analysis
library(stats)
str(merged_aadt)
aov_test_vars<-c("FUNC_CLASS_DESC","K_FACTOR","D_FACTOR","NUM_LANES","AADT","AAWDT"
                 ,"MOTORCYCLE","CAR_PICKUP","LARGE_TRUCK","MUNICIPALITY_Flag","SEC_LEN","Exit")
aov_test_vars<-as.data.frame(aov_test_vars)
for(var in 1:nrow(aov_test_vars)){
  fmla<-paste0("Freq~",aov_test_vars[var,1])
  output<-aov(as.formula(fmla),data = merged_aadt)
  aov_test_vars[var,"p_val"]<-summary(output)[[1]][1,5]
}

aov_test_vars_1<-aov_test_vars[order(aov_test_vars$p_val),]

sig_aov_vars<-aov_test_vars_1[aov_test_vars_1$p_val<0.05,]

sig_aov_vars_usable<-as.character(sig_aov_vars$aov_test_vars[c(1,3:8,10)])

hist(merged_aadt$Freq[merged_aadt$Freq>100],100)

plot(merged_aadt$Freq,merged_aadt$AADT)

plot(log(merged_aadt$Freq+1),log(merged_aadt$AADT))

plot(merged_aadt$SEC_LEN,merged_aadt$Freq)

plot(merged_aadt$NUM_LANES,merged_aadt$Freq)

boxplot(merged_aadt$NUM_LANES,merged_aadt$Freq)



#linear regression

#lmfmla<-as.formula(paste0("Freq~",paste(sig_aov_vars_usable,collapse = "+")))
lmfmla<-as.formula("Freq~FUNC_CLASS_DESC2+K_FACTOR+D_FACTOR+SEC_LEN+NUM_LANES+MUNICIPALITY_Flag+Exit")
library(caret)
library(klaR)

train_control <- trainControl(method="repeatedcv", number=10,repeats=10)
model <- train(lmfmla, data=merged_aadt, trControl=train_control, method="lm")
print(model)
summary(model)
AIC(model)

#Initial Linear model
model_lm<-lm(lmfmla,data=merged_aadt)
summary(model_lm)
plot(model_lm)
vif(model_lm)
sq_vifs<-sqrt(vif(model_lm))
logLik(model_lm)



# #variable selection
# library(leaps)
# leaps=regsubsets(lmfmla,data=merged_aadt, nvmax =20)
# df<-summary(leaps)
# #best by Adj_R2
# a<-which.max(summary(leaps)$adjr2)
# adj_sub<-summary(leaps)$which[a,]
# adj_sub<-names(adj_sub[adj_sub==TRUE])[2:length(names(adj_sub[adj_sub==TRUE]))]
# max(summary(leaps)$adjr2)
# 
# #best by BIC
# a<-which.min(summary(leaps)$bic)
# bic_sub<-summary(leaps)$which[a,]
# bic_sub<-names(bic_sub[bic_sub==TRUE])[2:length(names(bic_sub[bic_sub==TRUE]))]
# min(summary(leaps)$bic)
# 
# #best by Cp


lmfmla2<-as.formula("log(freq_per_mile+0.00001)~FUNC_CLASS_DESC+AADT+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")

model3<-lm(lmfmla2,data=merged_aadt)
logLik(model3)
summary(model3)
plot(model3)
print(model3)


mod<-mle(lmfmla,data = merged_aadt)
summary(mod)
BIC(mod)


#applying logarithm to response variable
lgfmla<-as.formula("log(Freq+1)~FUNC_CLASS_DESC+AADT+SEC_LEN+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")

train_control <- trainControl(method="repeatedcv", number=10,repeats=10)
model <- train(lgfmla, data=merged_aadt, trControl=train_control, method="lm")
print(model)


lg_mod<-lm(lgfmla,data = merged_aadt)
summary(lg_mod)
BIC(lg_mod)

#applying logarithm to response variable and a predictor

lgpfmla<-as.formula("log(Freq+1)~FUNC_CLASS_DESC+log(AADT)+log(SEC_LEN)+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")

train_control <- trainControl(method="repeatedcv", number=10,repeats=10)
model <- train(lgpfmla, data=merged_aadt, trControl=train_control, method="lm")
summary(model)
print(model)


lgp_mod<-lm(lgpfmla,data = merged_aadt)
summary(lgp_mod)
BIC(lgp_mod)
l<-logLik(lgp_mod)



#libraries for datasplit


split<-0.90
trainIndex <- createDataPartition(merged_aadt$Freq, p=split, list=FALSE)
data_train <- merged_aadt[trainIndex,]
data_test<-merged_aadt[-trainIndex,]
#linear model with logarithm to response variable
mod<-lm(fmla,data=data_train)
summary(mod)
alpha<0.05
barplot(sort(summary(mod)$coefficients[summary(mod)$coefficients[, 4] < alpha, 4], 
             decreasing = T), horiz = F, las = 1, main = "Log Linear",
        xlab = "p-value")
sort(summary(mod)$coefficients[summary(mod)$coefficients[, 4] < alpha, 4], 
     decreasing = T)

predictions<-predict(mod,data_test)

length(predictions)

error=abs(predictions-log(data_test$Freq+2.8))
per_error=error/log(data_test$Freq+2.8)
mean(per_error)

# #loglinear model
# lglfmla<-as.formula("Freq~FUNC_CLASS_DESC+AADT+SEC_LEN+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag")
# mod.loglm<-loglm(lglfmla,data=data_train)

#BOOTSTRAP
train_control <- trainControl(method="boot", number=10)
model <- train(fmla, data=merged_aadt, trControl=train_control, method="lm")
print(model)

#CV
train_control <- trainControl(method="repeatedcv", number=10,repeats=10)
model <- train(fmla, data=merged_aadt, trControl=train_control, method="lm")
print(model)

summary(model)

#poisson regression
poisfmla<-as.formula("freq_per_mile~FUNC_CLASS_DESC+AADT+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")
mod.pois<-glm(lmfmla,data=merged_aadt,family = poisson)
summary(mod.pois)
mod.pois_tr<-glm(lmfmla,data=data_train,family = poisson)
m<-mean(mod.pois_tr$residuals^2)
pr<-predict(mod.pois_tr,newdata=data_test)

resid_sq=mean((pr-data_test$Freq)^2)

resid_sq=mean((mod.pois_tr$fitted.values-data_train$Freq)^2)

plot(mod.pois)
anova(mod.pois,test = "Chisq")

coeftest(mod.pois, vcov = sandwich)


chi=(1 - pchisq(summary(mod.pois)$deviance, summary(mod.pois)$df.residual))



mn_cnt<-cbind(merged_aadt, Mean = predict(mod.pois, newdata=merged_aadt, type="response"), SE = predict(mod.pois, newdata=merged_aadt, type="response", se.fit=T)$se.fit)
cv.err_pois<-cv.glm(merged_aadt,mod.pois,K=10)$delta

#quasi Poisson 
qpoisfmla<-as.formula("Freq~FUNC_CLASS_DESC2+K_FACTOR+D_FACTOR+SEC_LEN+NUM_LANES+MUNICIPALITY_Flag+Exit")
mod.qpois<-glm(lmfmla,data=merged_aadt,family = quasipoisson)
summary(mod.qpois)
cv.err_qpois<-cv.glm(merged_aadt,mod.qpois,K=5)$delta

#rpart poisson
library(rpart)
treePois <- rpart(poisfmla, data = data_train, method = "poisson")


op <- par(mar = c(1, 0, 1, 0))
plot(treePois, main = "Poisson Regression Tree")
text(treePois, use.n=TRUE, cex = .8)
par(op)


#Negative binomial
library(MASS)
mod.nb<-glm.nb(lmfmla,data=merged_aadt)
summary(mod.nb)
logLik(mod.nb)
anova(mod.nb,test = "Chisq")
chi.nb=(1 - pchisq(summary(mod.nb)$deviance, summary(mod.nb)$df.residual))

mn_var<-cbind(data_test,Mean = predict(mod.nb, newdata = data_test, type = "response"),SE = predict(mod.nb, newdata = data_test, type = "response", se.fit = T)$se.fit)

library(boot)
cv.err<-cv.glm(merged_aadt,mod.nb,K=10)$delta


#-------------------------------------------------------------------------------
# In this section, we summarized useful variables in each model (pois, nb)
#-------------------------------------------------------------------------------
alpha <- 0.05
summary(mod.pois)$coefficients[summary(mod.pois)$coefficients[, 4] < alpha, ]
summary(mod.nb)$coefficients[summary(mod.nb)$coefficients[, 4] < alpha, ]

barplot(sort(summary(mod.pois)$coefficients[summary(mod.pois)$coefficients[, 4] < alpha, 4], 
             decreasing = T), horiz = F, las = 1, main = "Poisson Regression",
        xlab = "p-value")
barplot(sort(summary(mod.nb)$coefficients[summary(mod.nb)$coefficients[, 4] < alpha, 4], 
             decreasing = T), horiz = F, las = 1, main = "Negative Binomial",
        xlab = "p-value")


#Hurdle Model
hurdfmla<-as.formula("Freq~FUNC_CLASS_DESC2+SEC_LEN+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")
hurdle_nb <- hurdle(lmfmla, data = merged_aadt, dist = "negbin")
logLik(hurdle_nb)
table(merged_aadt$FUNC_CLASS_DESC)
summary(hurdle_nb)


#Hurdle poisson
hurdle_p <- hurdle(lmfmla, data = merged_aadt, dist = "poisson")
summary(hurdle_p)

# Zero-inflated Negative-Binomal model
zinfl<-as.formula("Freq~FUNC_CLASS_DESC2+SEC_LEN+K_FACTOR+D_FACTOR+NUM_LANES+MUNICIPALITY_Flag+Exit")
library(pscl)
model.zinb = zeroinfl(lmfmla, data = merged_aadt, dist="negbin")
logLik(model.zinb)
summary(model.zinb)
print(model.zinb)

#cnt_zero<-cbind(data_test, Count = predict(model.zinb, newdata = data_test, type = "count"),Zero = predict(model.zinb, newdata = data_test, type = "zero"))

library(mpath)
zipath_mod<-conv2zipath(model.zinb, family=c("negbin"))
fm_zip<-cv.zipath(zinfl,merged_aadt,nfolds = 10,family="negbin")

## Zero-inflated Poisson model
zinflp<-as.formula("Freq~SEC_LEN+K_FACTOR+D_FACTOR+NUM_LANES")
library(pscl)
model.zip = zeroinfl(lmfmla, data = merged_aadt)
summary(model.zip)
print(model.zip)

cnt_zero_p<-cbind(data_test, Count = predict(model.zip, newdata = data_test, type = "count"),Zero = predict(model.zip, newdata = data_test, type = "zero"))

#Comparison of MOdels

fm <- list("ML-Pois" = mod.pois, "Quasi-Pois" = mod.qpois, "NB" = mod.nb,
           "Hurdle-P" = hurdle_p,"Hurdle-NB" = hurdle_nb,"ZINP"=model.zip, "ZINB" = model.zinb)

fm <- list("ML-Pois" = mod.pois, "Quasi-Pois" = mod.qpois, "NB" = mod.nb,
           "Hurdle-NB" = hurdle_nb, "ZINB" = model.zinb)


Coefficients<-sapply(fm, function(x) coef(x)[1:13])

cbind("ML-Pois" = sqrt(diag(vcov(mod.pois))),
      "Adj-Pois" = sqrt(diag(sandwich(mod.pois))),
      sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:13]))
model_comps<-rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
            Df = sapply(fm, function(x) attr(logLik(x), "df")),
            AIC=sapply(fm, function(x) round(AIC(x), digits = 0)),
            BIC=sapply(fm, function(x) round(BIC(x), digits = 0))
                       )

zeros_identified<-round(c("Obs" = sum(merged_aadt$Freq < 1),
                    "ML-Pois" = sum(dpois(0, fitted(mod.pois))),
                    "NB" = sum(dnbinom(0, mu = fitted(mod.nb), size = mod.nb$theta)),
                    "Hurdle_P"= sum(predict(hurdle_p, type = "prob")[,1]),
                    "NB-Hurdle" = sum(predict(hurdle_nb, type = "prob")[,1]),
                    "ZIP" = sum(predict(model.zip, type = "prob")[,1]),
                    "ZINB" = sum(predict(model.zinb, type = "prob")[,1])))
num_mat<-data.frame()
for (i in 0:9){
          nums_identified<-round(c( "Obs" = sum(merged_aadt$Freq < i+1 & merged_aadt$Freq >= i ),
                                    "ML-Pois" = sum(dpois(i, fitted(mod.pois))),
                                    "NB" = sum(dnbinom(i, mu = fitted(mod.nb), size = mod.nb$theta)),
                                    "Hurdle_P"= sum(predict(hurdle_p, type = "prob")[,i+1]),
                                    "NB-Hurdle" = sum(predict(hurdle_nb, type = "prob")[,i+1]),
                                    "ZIP" = sum(predict(model.zip, type = "prob")[,i+1]),
                                    "ZINB" = sum(predict(model.zinb, type = "prob")[,i+1])))
          num_mat<-rbind(num_mat,nums_identified)
}

#out of sample zeros identified
zeros_identified_out<-round(c("Obs" = sum(data_test$Freq < 1),
                          "ML-Pois" = sum(dpois(0, fitted(mod.pois))),
                          "NB" = sum(dnbinom(0, mu = fitted(mod.nb), size = mod.nb$theta)),
                          "Hurdle_P"= sum(predict(hurdle_p, type = "prob")[,1]),
                          "NB-Hurdle" = sum(predict(hurdle_nb, type = "prob")[,1]),
                          "ZIP" = sum(predict(model.zip, type = "prob")[,1]),
                          "ZINB" = sum(predict(model.zinb, type = "prob")[,1])))


dt=predict(model.zinb,newdata = data_test,type="prob")

write.csv(Coefficients,"coefficients.csv")
write.csv(model_comps,"model_comps.csv")
write.csv(num_mat,"num_mat.csv")

#testing to zeros accuracy
cutoff=0.5
c(nrow(cnt_zero_p[cnt_zero_p$Zero>cutoff&cnt_zero_p$Freq==0,]),nrow(cnt_zero_p[cnt_zero_p$Zero>cutoff,]))
c(nrow(cnt_zero[cnt_zero$Zero>cutoff&cnt_zero$Freq==0,]),nrow(cnt_zero[cnt_zero$Zero>cutoff,]))

with(merged_aadt,table(Freq))

hist(merged_aadt$Freq)
ave=mean(merged_aadt$Freq)
num<-(merged_aadt$Freq-ave)^3
s=sd(merged_aadt$Freq)
skew<-sum(num)/(1036*(s^3))

hist(merged_aadt$Freq,50)
write.csv(merged_aadt,'merged_aadt.csv')

#correlation analysis
cor_mat_data<-merged_aadt[,c("K_FACTOR","D_FACTOR","NUM_LANES","AADT","Freq","SEC_LEN")]
cor(cor_mat_data)


plot(merged_aadt$FUNC_CLASS_DESC,log(merged_aadt$AADT))
a<-aov(AADT~FUNC_CLASS_DESC,data=merged_aadt)
summary(a)
library(psych)
plot(m, v, xlab="Mean", ylab="Variance",main="Mean-Variance Relationship")


#calculating error stats
rm_mp<-RMSE(mod.pois$fitted.values,merged_aadt$Freq)
rm_qp<-RMSE(mod.qpois$fitted.values,merged_aadt$Freq)
rm_nb<-RMSE(mod.nb$fitted.values,merged_aadt$Freq)
rm_hp<-RMSE(hurdle_p$fitted.values,merged_aadt$Freq)
rm_hnb<-RMSE(hurdle_nb$fitted.values,merged_aadt$Freq)
rm_zip<-RMSE(model.zip$fitted.values,merged_aadt$Freq)
rm_zinb<-RMSE(model.zinb$fitted.values,merged_aadt$Freq)
rm_lm<-RMSE(model_lm$fitted.values,merged_aadt$Freq)

lmfmla2<-as.formula("Freq~FUNC_CLASS_DESC2+K_FACTOR+D_FACTOR+SEC_LEN+NUM_LANES+MUNICIPALITY_Flag+Exit")
lmfmla3<-as.formula("Freq~FUNC_CLASS_DESC2+K_FACTOR+SEC_LEN+NUM_LANES+MUNICIPALITY_Flag+Exit")

model_lm2<-lm(lmfmla3,data=data_train)


mod.nb2<-glm.nb(lmfmla3,data = data_train)

mod.pois2<-glm(lmfmla3,data = data_train,family = poisson)

hurdle_nb2 <- hurdle(lmfmla3, data = data_train, dist = "negbin")
hurdle_p2 <- hurdle(lmfmla3, data = data_train, dist = "poisson")
model.zinb2 = zeroinfl(lmfmla3, data = data_train, dist="negbin")
model.zip2= zeroinfl(lmfmla3, data = data_train, dist="poisson")
rm_mp2<-RMSE(mod.pois2$fitted.values,data_train$Freq)
rm_nb2<-RMSE(mod.nb2$fitted.values,data_train$Freq)
rm_mp2<-RMSE(predict(mod.pois2,newdata = data_test),data_test$Freq)
rm_nb2<-RMSE(predict(mod.nb2,newdata = data_test),data_test$Freq)
rm_hnb2<-RMSE(predict(hurdle_nb2,newdata = data_test),data_test$Freq)
rm_hp2<-RMSE(predict(hurdle_p2,newdata = data_test),data_test$Freq)

rm_zinb2<-RMSE(predict(model.zinb2,newdata = data_test),data_test$Freq)
rm_zip2<-RMSE(predict(model.zip2,newdata = data_test),data_test$Freq)

rm_lm2<-RMSE(predict(model_lm2,newdata = data_test),data_test$Freq)


paste(rm_mp2,rm_nb2,rm_hp2,rm_hnb2,rm_zinb2,rm_zip2)

model.zinb = zeroinfl(lmfmla, data = merged_aadt, dist="negbin")

zero_ceoffs<-t(sapply(fm[4:5], function(x) round(x$coefficients$zero, digits = 3)))
write.csv(zero_ceoffs,"zero_coeffs.csv")

write.csv(merged_aadt,"Final data file.csv")
