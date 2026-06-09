##Bootstrap#

library(renv)
library(dplyr)
library(janitor)
library(hablar)
library(lubridate)
library(phsmethods)
library(arrow)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(cobalt)
library(WeightIt)
library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)
folder_data_path <- "/conf/FolicAcid/data/"


#preprep not in loop#
df<- readRDS(paste0(folder_data_path, "working_data/Sens_ASM_dataset_clean_wts.rds"))
df <- df %>%
  mutate(follow_days = case_when(as.numeric(follow_days)==0 ~1, T~as.numeric(follow_days))) %>%
  mutate(
    follow_years = follow_days / 365.25,
    n_int = ceiling(follow_years)  #1-year intervals
  ) %>%
  mutate(treated = hdfa_preg)
##combine last two years
df <- df %>% mutate(n_int = case_when(n_int==14 ~13, T~n_int))

df <-df %>% select(pregnancy_id,
                    treated,n_int,follow_years,
                   cancer_outcome,hdfa_preg, year_conception, maternal_age_conception,
                     maternal_simd , mat_ethnicity_broad_groups, 
                     ind_ntd , ind_preexist_diabetes ,
                     mtx_flag ,obese, ind_coeliac, ind_sickle_cell, 
                     ind_thalassaemia , cancer_history ,
                     any_comorb, maternal_smoking)
# Register parallel backend####
if(!require(doParallel)) { install.packages("doParallel"); require(doParallel)}
if(!require(foreach)) { install.packages("foreach"); require(foreach)}
registerDoParallel(cores=8)
getDoParWorkers() # Print number of cores

##setup 500 bootids####
B <- 500 # 
set.seed(123)
# Set up bootstrap samples
ids <- (df$pregnancy_id)
nind <- length(ids)
bootids <- matrix(ids[sample.int(nind, nind*B, replace=T)], nrow=nind, ncol=B)
# Now bootstrap the results

system.time(bootres <- foreach(i=1:B, .combine=rbind) %dopar% {

  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                maternal_simd + mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + 
                                                mtx_flag +obese + ind_coeliac + ind_sickle_cell + 
                                                ind_thalassaemia +
                                                cancer_history +
                                                any_comorb +maternal_smoking, data = bootdf,
                                              estimand = "ATE",  # Find the ATE
                                              method = "ps", 
                                              stabilize=TRUE)$weights

  bootdf <- bootdf %>% select(  pregnancy_id,
                           treated,
                          cancer_outcome,
                          n_int,
                          stabil_weights)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
 
  pp <-  bootdf  %>%
    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  

  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,3)+ treated,
                 weights = stabil_weights, data = pp,  family=binomial())#
 

  # where the baseline information has been carried forward at each time
  # Sample size
  n <- length(unique(pp$pregnancy_id))
  df_treated <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  df_treated <- df_treated[rep(1:n,each=13),]
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
  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
  placebo <- placebo%>%
    mutate(
      # Set the treatment assignment to '0' for each individual and
      treated = 0, 
    ) 
  
  # 'predict' returns predicted "density" of survival at each time
  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')
  placebo_SE <-  predict(fit_wts, newdata=placebo, type='response',se.fit = TRUE)
  placebo$p_se <- placebo_SE$se.fit
  
  # We calculate survival by taking the cumulative product by individual
  placebo <- placebo %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  # Calculate standardized survival at each time

  both <- dplyr::bind_rows(df_treated, placebo)
  both <- both[,c('s', 'treated', 'time_interval')]

  results <- both%>%
    group_by(time_interval, treated)%>%
    dplyr::summarize(mean_survival = mean(s))
  

  results <- results%>%
    dplyr::ungroup()%>%
    mutate(
      time_interval = time_interval+1
    )
  
  results<-dplyr::bind_rows(c(time_interval = 0, treated = 0, mean_survival =  1),
                            c(time_interval = 0, treated = 1, mean_survival =  1), results)
  

  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
 
# Calculate risk difference and hazard ratio at interval 12, 32, 54
  # Transpose the data so survival in each treatment arm is separate
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')

  # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

 
   return(results)
})

saveRDS(bootres, paste0(folder_data_path, "outputs/sens_ASM_bootres1_500.rds"))
