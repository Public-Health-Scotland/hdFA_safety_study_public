library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)

fit_wts <- readRDS(paste0(folder_data_path, "outputs/sens_any_model_lineartime.rds"))
df_long <- readRDS(paste0(folder_data_path, "working_data/sensitivity_any_long_data.rds"))

##stabilised weights####
##time as factor
fit_wts <- fit_wts
#summary(fit_wts)
robustSE <-coeftest(fit_wts, vcov=vcovHC(fit_wts, type="HC1")) # To get robust SE estimates
xpcoeff<-exp(coef(fit_wts))
#confidence intervals
CIs <- lmtest::coefci(fit_wts, vcov. = sandwich::vcovHC)
CIs<- as.data.frame(CIs)
CIs$varname <-rownames(CIs)
CIs <- CIs %>% select(varname, `2.5 %`, `97.5 %`)
trtCI <- CIs %>% filter(varname=="treated") %>% select(-varname)
coeff_trt <- xpcoeff["treated"]
results_stab <- cbind(coeff_trt, exp(trtCI))
str(CIs)
results_stab 

# Expand baseline so it contains a visit at each time point for every individual
# Create 'baseline' - data collected at visit 0
# model for factor time, no additional covariables
fit_wts <- fit_wts

baseline <-df_long %>%
  dplyr::filter(as.numeric(time_interval) == 0)
# where the baseline information has been carried forward at each time
# Sample size
n <- length(unique(df_long$pregnancy_id))
n
df_treated <- baseline[rep(1:n,each=13),]
df_treated$time_interval <- rep(0:12, times=n) # This recreates the time variable
df_treated <- df_treated %>%
  mutate(
    # Set the treatment assignment to '1' for each individual and
    treated = 1, 
  ) 


# 'predict' returns predicted "density" of survival at each time
# conditional on covariates
# Turn these into predicted survival density by subtracting from 1
df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')
df_treated_SE <-  predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)
df_treated$p_se <- df_treated_SE$se.fit
# We calculate survival by taking the cumulative product by individual
df_treated <- df_treated %>%
  dplyr::arrange(pregnancy_id,time_interval)%>%
  group_by(pregnancy_id)%>%
  mutate(
    s = cumprod(p)
  )

#  Create simulated data where everyone receives placebo
# When simulating data in the placebo arm, only difference from treated is 
# in the randomization assignment, and resulting interaction terms
placebo <- baseline[rep(1:n,each=13),]
placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
placebo <- placebo%>%
  mutate(
    # Set the treatment assignment to '0' for each individual and
    treated = 0, 
 
  ) 

# 'predict' returns predicted "density" of survival at each time
# conditional on covariates
# Turn these into predicted survival density by subtracting from 1
placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')

##standard errors for later CIs? 
placebo_SE <-  predict(fit_wts, newdata=placebo, type='response',se.fit = TRUE)
placebo$p_se <- placebo_SE$se.fit
head(placebo)
# We calculate survival by taking the cumulative product by individual
placebo <- placebo %>%
  dplyr::arrange(pregnancy_id,time_interval)%>%
  group_by(pregnancy_id)%>%
  mutate(
    s = cumprod(p)
  )


# calculate standardized survival at each time
# Create concatenated dataset, only keep s, rand, and visit
both <- dplyr::bind_rows(df_treated, placebo)
both <- both[,c('s', 'treated', 'time_interval')]


# Calculate the mean survival at each visit within each treatment arm
results <- both%>%
  group_by(time_interval, treated)%>%
  dplyr::summarize(mean_survival = mean(s))


# Edit results data frame to reflect that our estimates are for the END of the interval [t, t+1)
# Add a row for each of Placebo and Treated where survival at time 0 is 1.
results <- results%>%
  dplyr::ungroup()%>%
  mutate(
    time_interval = time_interval+1
  )

results<-dplyr::bind_rows(c(time_interval = 0, treated = 0, mean_survival =  1),
                          c(time_interval = 0, treated = 1, mean_survival =  1), results)

# Add a variable that treats randomization as a factor
results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))

saveRDS(results, paste0(folder_data_path, "outputs/sens_any_predictions_fitwts_l.rds"))
#  Plot the results
p2 <- ggplot(results, aes(x=time_interval, y=mean_survival))+
  geom_line(aes(colour=randf)) +
  geom_point(aes(colour=randf))+
  xlab("Number of Visits") +
  #  scale_x_continuous(limits = c(0, 15), breaks=seq(0,15,2)) +
  ylab("Probability of Survival") +
  ggtitle("Survival Curves Standardized for IPTW") +
  labs(colour="Treatment Arm") +
  theme_bw() +
  theme(legend.position="bottom")
p2


# Calculate risk difference and hazard ratio
# Transpose the data so survival in each treatment arm is separate
wideres <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')
head(wideres)

# Create summary statistics
wideres <- wideres%>%
  mutate(
    RD = (1-Treated) - (1-Untreated),
    logRatio = log(Treated)/log(Untreated),
    CIR = (1-Treated)/ (1-Untreated)
  )
wideres$logRatio[1] <- NA
wideres$cHR <- sapply(0:12, FUN=function(x){mean(wideres$logRatio[wideres$time_interval <= x], na.rm=T)})


saveRDS(wideres, paste0(folder_data_path,"outputs/sens_anypredict_surv_weighted.rds"))
wideres <- readRDS(paste0(folder_data_path,"outputs/sens_anypredict_surv_weighted.rds"))

