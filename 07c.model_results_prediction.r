library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)

fit_wts_spl_stab <- readRDS(paste0(folder_data_path, "outputs/model_stabwts_lineartime_spline.rds"))

df_long2 <- read_parquet(paste0(folder_data_path,
                                "working_data/timeseries_1yr_start_birth_details_cleaned.parquet"))

# . Create simulated data where everyone is treated
# predict uses a lot of memory - that plus large datasets in memory means using >40k
# 50k is enough (think its just over 40k used)


fit_wts <- fit_wts_spl_stab
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
results_spl_stab <- cbind(coeff_trt, exp(trtCI))
results_spl_stab

##predict w splines and stabilsed weigts####
fit_wts <- fit_wts_spl_stab

baseline <-df_long2 %>%
  dplyr::filter(as.numeric(time_interval) == 0)
# where the baseline information has been carried forward at each time
# Sample size
n <- length(unique(df_long2$pregnancy_id))
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
placebo <- baseline[rep(1:n,each=13),]
placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
placebo <- placebo%>%
  mutate(
    # Set the treatment assignment to '0' for each individual and
    treated = 0, 
    
  ) 
#
placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')

##standard errors for later CIs? 
placebo_SE <-  predict(fit_wts, newdata=placebo, type='response',se.fit = TRUE)
placebo$p_se <- placebo_SE$se.fit
#

placebo <- placebo %>%
  dplyr::arrange(pregnancy_id,time_interval)%>%
  group_by(pregnancy_id)%>%
  mutate(
    s = cumprod(p)
  )


# Calculate standardized survival at each time
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

saveRDS(results, paste0(folder_data_path, "outputs/predictions_fitwts_spline.rds"))

# Plot the results
p2_x <- ggplot(results, aes(x=time_interval, y=mean_survival))+
  geom_line(aes(colour=randf)) +
  geom_point(aes(colour=randf))+
  xlab("Number of Visits") +
  ylab("Probability of Survival") +
  ggtitle("Predicted survival, stabilised weights time as spline") +
  labs(colour="Treatment Arm") +
  theme_bw() +
  theme(legend.position="bottom")
p2_x

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
wideres$cHR <- sapply(0:13, FUN=function(x){mean(wideres$logRatio[wideres$time_interval <= x], na.rm=T)})

saveRDS(wideres, paste0(folder_data_path,"outputs/predict_surv_spline.rds"))

