##Bootstrap CIs#

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
df<- readRDS(paste0(folder_data_path, "working_data/main_dataset_clean_wts.rds"))
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
                     ind_ntd , ind_preexist_diabetes , asm_flag,
                     mtx_flag ,obese, ind_coeliac, ind_sickle_cell, 
                     ind_thalassaemia , cancer_history ,
                     any_comorb, maternal_smoking)

# Register parallel backend####
if(!require(doParallel)) { install.packages("doParallel"); require(doParallel)}
if(!require(foreach)) { install.packages("foreach"); require(foreach)}
registerDoParallel(cores=8)
getDoParWorkers() # Print number of cores

##setup 100 bootids####
B <- 100 # 
set.seed(123)
# Set up bootstrap samples
ids <- (df$pregnancy_id)
nind <- length(ids)
bootids <- matrix(ids[sample.int(nind, nind*B, replace=T)], nrow=nind, ncol=B)
# Now bootstrap the results


B<-50 ## test with small number
system.time(bootres <- foreach(i=1:B, .combine=rbind) %dopar% {
 # require(dplyr)
#  i<-1
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd + mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
                                                ind_thalassaemia +
                                                cancer_history +
                                                any_comorb +maternal_smoking, data = bootdf,
                                              estimand = "ATE",  # Find the ATE
                                              method = "ps", 
                                              stabilize=TRUE)$weights
#  
  bootdf <- bootdf %>% select(  pregnancy_id,
                           treated,
                          cancer_outcome,
                          n_int,
                          stabil_weights,
                          # OPTIONAL covariates for outcome model:
                          maternal_age_conception,
                          year_conception,
                          any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
 
  pp <-  bootdf  %>%

    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  
  

  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
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
  
  # 
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')
  df_treated_SE <-  predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)
  df_treated$p_se <- df_treated_SE$se.fit
  # 
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo
  #
  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
  placebo <- placebo%>%
    mutate(
    
      treated = 0, 
    ) 
  
  #
  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')

  placebo_SE <-  predict(fit_wts, newdata=placebo, type='response',se.fit = TRUE)
  placebo$p_se <- placebo_SE$se.fit
  
  # We
  placebo <- placebo %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  # Calculate standardized survival at each time
  # 
  both <- dplyr::bind_rows(df_treated, placebo)
  both <- both[,c('s', 'treated', 'time_interval')]
  # Calculate the mean survival at each visit within each treatment arm
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
  
  # Add a variable that treats randomization as a factor
  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
 
    #. Calculate risk difference and hazard ratio 
  # Transpose the data so survival in each treatment arm is separate
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')
  #head(wideres)
  # Create summary statistics
  results <- results %>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

   return(results)
})

saveRDS(bootres, paste0(folder_data_path, "outputs/bootres1_50.rds"))

##runs 51-100###
system.time(bootres <- foreach(i=51:100, .combine=rbind) %dopar% {
 
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd +
                                                mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
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
                                stabil_weights,
                                # OPTIONAL covariates for outcome model:
                                maternal_age_conception,
                                year_conception,
                                any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
  
  pp <-  bootdf  %>%

    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  
  

  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
                 weights = stabil_weights, data = pp,  family=binomial())#

  

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
  
  # 
  
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')
  df_treated_SE <-  predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)
  df_treated$p_se <- df_treated_SE$se.fit
  #
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo
 
  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
  placebo <- placebo%>%
    mutate(
      # Set the treatment assignment to '0' for each individual and
      treated = 0, 
    ) 
  
  
  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')

  placebo_SE <-  predict(fit_wts, newdata=placebo, type='response',se.fit = TRUE)
  placebo$p_se <- placebo_SE$se.fit
  

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
  
  # Calculate risk difference and hazard ratio 
  # Transpose the data so survival in each treatment arm is separate
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')
 
  
  # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

  
  return(results)
})
saveRDS(bootres, paste0(folder_data_path, "outputs/bootres51_100.rds"))

### Create the remaining 400 boot IDS####
##setup 400 bootids####
B <- 400 # try a small number of reps to test code
set.seed(321) # new seed
# Set up bootstrap samples
ids <- (df$pregnancy_id)
nind <- length(ids)
bootids <- matrix(ids[sample.int(nind, nind*B, replace=T)], nrow=nind, ncol=B)
# Now bootstrap the results

system.time(bootres <- foreach(i=1:100, .combine=rbind) %dopar% {
  # require(dplyr)
  #  i<-1
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd +
                                                mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
                                                ind_thalassaemia +
                                                cancer_history +
                                                any_comorb +maternal_smoking, data = bootdf,
                                              estimand = "ATE",  # Find the ATE
                                              method = "ps", 
                                              stabilize=TRUE)$weights
  #  return(summary(bootdf$stabil_weights))
  bootdf <- bootdf %>% select(  pregnancy_id,
                                treated,
                                cancer_outcome,
                                n_int,
                                stabil_weights,
                                # OPTIONAL covariates for outcome model:
                                maternal_age_conception,
                                year_conception,
                                any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
  
  pp <-  bootdf  %>%

    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  

  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
                 weights = stabil_weights, data = pp,  family=binomial())#

  # 
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
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')
  df_treated$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  # 
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo

  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) #
  placebo <- placebo%>%
    mutate(
      # Set the treatment assignment to '0' for each individual and
      treated = 0, 
    ) 
  
  # 'predict' returns predicted "density" of survival at each time
  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')
 
  placebo$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  
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
  # Calculate the mean survival at each visit within each treatment arm
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
  
  # Add a variable that treats randomization as a factor
  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
  
  #  Calculate risk difference and hazard ratio
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')
 
    # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})
 
  return(results)
})
saveRDS(bootres, paste0(folder_data_path, "outputs/bootres101_200.rds"))

##201-300####
system.time(bootres <- foreach(i=101:200, .combine=rbind) %dopar% {
  # require(dplyr)
  #  i<-1
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd +
                                                mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
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
                                stabil_weights,
                                # OPTIONAL covariates for outcome model:
                                maternal_age_conception,
                                year_conception,
                                any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
  
  pp <-  bootdf  %>%
 
    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  
  
   ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
                 weights = stabil_weights, data = pp,  family=binomial())#
 

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
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')

  df_treated$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  # We calculate survival by taking the cumulative product by individual
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo

  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) #
  placebo <- placebo%>%
    mutate(
      treated = 0, 
    ) 
  
  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')

  placebo$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  
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
  
  # Add a variable that treats randomization as a factor
  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
  
  # Calculate risk difference and hazard ratio 
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')

  # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

  
  return(results)
})
saveRDS(bootres, paste0(folder_data_path, "outputs/bootres201_300.rds"))

#301-400####
system.time(bootres <- foreach(i=201:300, .combine=rbind) %dopar% {
  # require(dplyr)
  #  i<-1
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  
  
  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd +
                                                mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
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
                                stabil_weights,
                                # OPTIONAL covariates for outcome model:
                                maternal_age_conception,
                                year_conception,
                                any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
  
  pp <-  bootdf  %>%

    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  
  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
                 weights = stabil_weights, data = pp,  family=binomial())#


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
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')

  df_treated$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  # We calculate survival by taking the cumulative product by individual
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo

  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
  placebo <- placebo%>%
    mutate(
      # Set the treatment assignment to '0' for each individual and
      treated = 0, 
    ) 
  

  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')
  placebo$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  
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
  
  # Add a variable that treats randomization as a factor
  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
  
  # Calculate risk difference and hazard ratio 
  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')

  # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

  
  return(results)
})
saveRDS(bootres, paste0(folder_data_path, "outputs/bootres301_400.rds"))
##301-400####
system.time(bootres <- foreach(i=301:400, .combine=rbind) %dopar% {
  # require(dplyr)
  #  i<-1
  ids <- as.data.frame(bootids[,i])
  names(ids) <- "pregnancy_id"
  bootdf <- dplyr::left_join(ids, df)
  

  bootdf$stabil_weights <- WeightIt::weightit(hdfa_preg ~
                                                year_conception + as.numeric(maternal_age_conception)+
                                                year_conception*maternal_simd + mat_ethnicity_broad_groups + 
                                                ind_ntd + ind_preexist_diabetes + asm_flag +
                                                mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
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
                                stabil_weights,
                                # OPTIONAL covariates for outcome model:
                                maternal_age_conception,
                                year_conception,
                                any_comorb)
  
  bootdf <- bootdf  %>% mutate(pregnancy_id = paste0("ID",1:nrow( bootdf )))
  
  pp <-  bootdf  %>%
    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
    mutate(
      tstart = time_interval - 1L,
      event = as.integer(time_interval == n_int & cancer_outcome == 1)
    ) %>%
    mutate(time_interval = time_interval-1)
  

  ##refit regression####
  fit_wts <- glm(event ~  time_interval + rcs(time_interval,4)+ 
                   treated,
                 weights = stabil_weights, data = pp,  family=binomial())#

  
  # 
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
  
  df_treated$p <- 1 - predict(fit_wts, newdata=df_treated, type='response')
  # df_treated_SE <-  predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)
  df_treated$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit
  # We calculate survival by taking the cumulative product by individual
  df_treated <- df_treated %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  
  #  Create simulated data where everyone receives placebo
  placebo <-pp %>%
    dplyr::filter(as.numeric(time_interval) == 0)
  placebo <- placebo[rep(1:n,each=13),]
  placebo$time_interval <- rep(0:12, times=n) # This recreates the time variable
  placebo <- placebo%>%
    mutate(
      # Set the treatment assignment to '0' for each individual and
      treated = 0, 
    ) 
  

  placebo$p <- 1 - predict(fit_wts, newdata=placebo, type='response')
  placebo$p_se <- predict(fit_wts, newdata=df_treated, type='response',se.fit = TRUE)$se.fit

  placebo <- placebo %>%
    dplyr::arrange(pregnancy_id,time_interval)%>%
    group_by(pregnancy_id)%>%
    mutate(
      s = cumprod(p)
    )
  #  Calculate standardized survival at each time
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
  
  # Add a variable that treats randomization as a factor
  results$randf <- factor(results$treated, labels = c("Untreated", "Treated"))
  
  #  Calculate risk difference and hazard ratio

  results <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')
  # Create summary statistics
  results <- results%>%
    mutate(
      RD = (1-Treated) - (1-Untreated),
      logRatio = log(Treated)/log(Untreated),
      Cum_inc_trt = 1-Treated, 
      Cum_inc_untrt = 1-Untreated, 
      CIR = (1-Treated)/ (1-Untreated)
    )
  results$logRatio[1] <- NA
  results$cHR <- sapply(0:13, FUN=function(x){mean(results$logRatio[results$time_interval <= x], na.rm=T)})

  
  return(results)
})
saveRDS(bootres, paste0(folder_data_path, "outputs/bootres401_500.rds"))
