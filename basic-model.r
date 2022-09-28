## NUMBER 1 (DISTANCE)
## RECOMMENDED FORMULA for Distance eREC - non-rapid studies, 21 studies total

library(tidyverse)
library(brms)  
options(mc.cores = parallel::detectCores())
library(data.table) 
###Read data########################################################################
data1<- read.csv("non-raab-crosswalk_final.csv") 
#############################################################################
#Filter the data to meet the conditions for a, b and c in  eREC formula
################################################################
df2<- dplyr::mutate(data1, better_distance_acuity_presenting  = pmin(Pres_DVA_RE_logmar,Pres_DVA_LE_logmar, na.rm = T))
df1 <- dplyr::mutate(df2, better_distance_acuity_corrected  = pmin(BC_DVA_RE_logmar,BC_DVA_LE_logmar, na.rm = T))
df<- dplyr::mutate(df1, better_distance_acuity_uncorrected  = pmin(Unc_DVA_RE_logmar,Unc_DVA_LE_logmar, na.rm = T))

##### To exclude participants for the 6/12 cut-off #######################################################----

df<- df %>% mutate(
  a_6.12  =  case_when(df$Glasses_status_Pres_DVA== TRUE &df$better_distance_acuity_presenting <=0.3&df$better_distance_acuity_uncorrected>0.3 ~ TRUE, TRUE ~ FALSE),
  c_6.12= case_when(df$Glasses_status_Pres_DVA == TRUE & df$better_distance_acuity_presenting >0.3&df$better_distance_acuity_uncorrected>0.3 & df$better_distance_acuity_corrected <=0.3 ~ TRUE, TRUE ~ FALSE), 
  d_6.12= case_when(df$Glasses_status_Pres_DVA == FALSE & df$better_distance_acuity_uncorrected >0.3  &df$better_distance_acuity_corrected <=0.3 ~ TRUE, TRUE ~ FALSE),
  
  exclusion6.12.F = case_when(Glasses_status_Pres_DVA==FALSE & 
                                better_distance_acuity_presenting <= 0.3 ~ TRUE, TRUE ~ FALSE),
  exclusion6.12.D = case_when(Glasses_status_Pres_DVA==FALSE & 
                                better_distance_acuity_presenting > 0.3& better_distance_acuity_corrected> 0.3~ TRUE, TRUE ~ FALSE),
  exclusion6.12.E = case_when(Glasses_status_Pres_DVA==TRUE & 
                                better_distance_acuity_presenting > 0.3& better_distance_acuity_corrected> 0.3~ TRUE, TRUE ~ FALSE))
##########################################################################
#To include participants 50 years of age or older#############################
df<- df %>% filter(Age %in% (50:100) )
#To discretize the age range 
df<-df%>%mutate(ageD=
                  cut(Age,breaks=c(seq(49,80,10),150),labels=c("50-59","60-69","70-79","80+"),
                      include.lowest=T,right=F))
##########################################################################
#To converts numeric columns ro factors###################################
df$gender <- as.factor(df$gender) 
df$country <- as.factor(df$country) 
df$gbd_region <- as.factor(df$gbd_region) 
df$year = (df$year - 2010) / 10
df$gbd_region <- as.factor(df$gbd_region)
df$region <- as.factor(df$region)
#To represent the condition expressions##############################################################
df = df %>% mutate(category = factor(case_when(a_6.12 ~ "Met Need", c_6.12 ~ "Unmet Need", d_6.12 ~ "Unmet Need", 
                                               exclusion6.12.F ~ "No need",
                                               exclusion6.12.D ~ "Non-refractive",
                                               exclusion6.12.E ~ "Non-refractive"), 
                                     levels=c("No need","Met Need", "Unmet Need", "Non-refractive"), ordered=T))


table(df$category) #To create frequency tables
levels(df$category) #To provides access to the levels attributes

### Direct estimates####################################################################
data.table(df)[, list(eREC = round(sum(category == "Met Need")/sum(category == "Met Need" | category == "Unmet Need"),2),
                      met = round(mean(category == "Met Need"),2),
                      unmet=round(mean(category == "Unmet Need"),2),noneed=round(mean(category == "No need"),2),
                      nonrefractive=round(mean(category == "Non-refractive"),2)), by=list(gbd_region)]

### fit models ############################################################################
prior <- c(prior(normal(0,1), class = "b")) #To create an S3 object of class bayesQR.prior that contains all necessary prior information to estimate a Bayesian quantile regression model.
#To produce a subset of data frame,retaining all rows that satisfy the specified conditions.
  
df = df %>% filter(category %in% c("Met Need", "Unmet Need"))
data.table(df)[, list(eREC = mean(category == "Met Need")),by=gbd_region]
df$eREC = as.numeric(df$category == "Met Need")
#Fit Bayesian generalized (non-)linear multivariate multilevel models using Stan for full Bayesian inference
M = brm(eREC ~ ageD + gender + year + (1|gbd_region),
        data = df,
        family = bernoulli(link = "logit"),iter=4000, thin=4, prior=prior)

summary(M)#To quickly summarize the values in a vector, data frame, regression model, or ANOVA model

#Save and print the results#############################################################

PID = abs(round(rnorm(1)*1e6))
saveRDS(M,sprintf("results/M-dataset-logistic-1-%d.rds",PID))
saveRDS(df,sprintf("results/df-dataset-logistic-1-%d.rds",PID))

print(sprintf("saved in results/df-dataset-logistic-1-%d.rds",PID))
