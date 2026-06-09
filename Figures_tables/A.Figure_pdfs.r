source("00.setup.r")
library(devEMF)
source("i.functions_for_excel.r")
convert_YN <- function(x){case_when(x==1 ~ "Yes", 
                                    x==0 ~ "No", 
                                    is.na(x)~ "No", 
                                    T~ as.character(x))}
quibble <- function(x, q = c(0.025,0.5, 0.975), dropNA = TRUE) {
  tibble(x = quantile(x, q, na.rm = dropNA), q = q)
}
##Figure 1 flowchart####
library(flowchart)
flowdata<- readRDS(paste0(folder_data_path, "outputs/flowchart_data.rds"))
#flowchart

n_nonlive <- flowdata %>% filter(livebirth==0) %>% nrow()

n_multi_live <- flowdata %>% filter(livebirth==1) %>% filter(multiple==1) %>% nrow()

##n of invalid chis within the LB singletons
n_invalid_chi <- flowdata %>% filter(livebirth==1 & singleton==1 & valid_chis==0)%>% nrow()
##N lb singltons with valid chi, with non scot postcode.
n_non_scot_pc <- flowdata %>% filter(livebirth==1 & singleton==1 & valid_chis==1 & non_scot_pc==1)%>% nrow()

fc1 <- flowdata |>
  as_fc(label = "All pregnancies to women age 18-49\nEDC 2010-04-01 to 2022-12-31") |>
  fc_filter(livebirth==1 & singleton==1,
            label = "All singleton live birth babies >=20 weeks", 
            show_exc = TRUE, round_digits=1,
            label_exc = paste0("Pregnancies with outcome(s) other\nthan live births at least 20 weeks gestation\n", 
                               n_nonlive,"\nMultiple live births\n", n_multi_live ), 
            text_pattern = "{label}\n {n} ", 
            text_pattern_exc = "{label} ") |>
  fc_filter(valid_chis==1 & non_scot_pc==0,round_digits=1,
            label = "Singleton live births available for linkage to \nexposure covariate and outcome data",
            show_exc = TRUE, 
            label_exc = paste0("Maternal and/or baby unique patient ID missing\n", n_invalid_chi,
                               "\n Mother resident outwith Scotland at antenatal\n booking and/or end of pregnancy\n ", n_non_scot_pc),
            text_pattern =  "{label}\n {n} ", 
            text_pattern_exc = "{label} ") |>
  fc_split(hdfa_preg, label= c("Treated with high dose (5mg daily) folic acid\nat any point from 12 weeks prior to\n EDC to end of pregnancy", 
                               "Not treated with high dose folic acid\n may have received standard dose 400mg\nfolic acid or no folic acid"), round_digits=1) |> 
  fc_split(cancer_outcome, label= c("No childhood cancer", 
                                    "Childhood cancer at any point\nfrom birth to earliest of\n 31st December 2023\n emigration or death"),round_digits=1) |>
  fc_modify(  ~ . |>
                dplyr::mutate(x = ifelse(id %in% c(3,5), x + 0.11, x), 
                              y = ifelse(id %in% c(9,11), y - 0.02, y),
                             
                              text_fs=10)
  ) 

fc1|>
  fc_draw()

fc_export(fc1|>
            fc_draw(), paste0(folder_data_path,"outputs/Figure1_flowchart.pdf"),res=300, 
          format="pdf", width = 15, height=11, units="in")

## Figure2 : Survival curve main analysis####
bootres <- readRDS("/conf/FolicAcid/data/outputs/main_bootres.rds" )

trt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Treated = list(quibble(Treated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Treated) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Treated")

untrt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Untreated = list(quibble(Untreated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Untreated)%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Untreated")

bsCI <- rbind(trt, untrt)

results <- readRDS(paste0(folder_data_path, "outputs/predictions_fitwts_spline.rds"))
results <- left_join(results,bsCI)

#change lebelling to hdfa vs std care
results <- results %>% mutate(randf = case_when(randf =="Treated" ~ "hdFA", 
                                                randf =="Untreated" ~ "Standard care" ))
p <- ggplot() +
  geom_line(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+
  geom_point(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+

  geom_ribbon(aes(x=results$time_interval, ymin= results$`CI2.5%`, ymax= results$`CI97.5%`,
                  colour= results$randf), linetype=2, alpha=0.1) +
  xlab("Years") +
  ylab("Probability of cancer free survival") +
  labs(colour="Treatment group") +
  ylim(0.990,1)+
  scale_y_continuous(limits = c(0.99,1), n.breaks = 3)+
  xlab("Age (Years)") +
  ylab("Probability of cancer free survival") +
  labs(colour="Treatment group") +
  theme_bw(base_size=10) 
p
pdf( paste0(folder_data_path,"outputs/Fig2_Surv_main.pdf"), width = 8, height=6)
p +
  theme_bw(base_size=12) 
dev.off()

#supplementary figures####
##eFigure 1: Gestation in weeks at first prescription of hdFA, by time period####
df<- readRDS(paste0(folder_data_path, "working_data/main_dataset.rds"))
## data prep####
##remove invalid chis
df <- df %>%  mutate(check_mother_chi = chi_check(mother_upi), check_baby_chi = chi_check(baby_upi)) %>%
  filter(check_mother_chi=="Valid CHI" & check_baby_chi=="Valid CHI") %>%
  filter(maternal_age_conception >=18 & maternal_age_conception <=49) %>%
  filter(gest_end_pregnancy>=20)
scottish_pc <- c("AB", "DD", "DG", "EH", "FK", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", 
                 "IV", "HS" , "KA", "KW", "KY", "ML", "PA", "PH", "TD", "ZE", "NK")# also allow nk for not known
df <- df %>% 
  mutate(non_scot_pc_booking = case_when(!is.na(maternal_postcode_booking) &
                                           !substr(maternal_postcode_booking,1,2) %in% scottish_pc~1, T~0)) %>%
  mutate(non_scot_pc_end= case_when(!is.na(maternal_postcode_end_preg) &
                                      !substr(maternal_postcode_end_preg,1,2) %in% scottish_pc~1, T~0)) %>%
  filter(non_scot_pc_booking==0 & non_scot_pc_end==0)
#remove cancer outcomes after emigration date
df<- df %>% mutate(cancer_outcome = case_when(incidence_date > DATE_TRANSFER_OUT~0, T~cancer_outcome))
##check deaths after censor date
df<- df %>%
  mutate(end_follow_type = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31") & end_follow_type=="censor - death" ~ "end study", 
                     T~end_follow_type),
         end_follow = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31")  ~ as.POSIXct("2023-12-31"), T~end_follow)  )
##ethnicity mapping
df <- df %>% mutate(mat_ethnicity_mapped = case_when(maternal_ethnicity %in% c("4D",  "4Y") ~ "4X", 
                                                     maternal_ethnicity %in% c("5C", "5D", "5Y") ~ "5X", 
                                                     maternal_ethnicity %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                     maternal_ethnicity=="5Z" ~"6Z",
                                                     maternal_ethnicity %in% c("1E", "1F") ~ "1B",
                                                     maternal_ethnicity== "1J" ~ "1C",
                                                     T~ maternal_ethnicity)) %>%
  ##for comparisons - also do infilled ethnicity
  mutate(mat_ethnicity_mapped_infill = case_when(maternal_ethnicityinfilled %in% c("4D",  "4Y") ~ "4X", 
                                                 maternal_ethnicityinfilled %in% c("5C", "5D", "5Y") ~ "5X", 
                                                 maternal_ethnicityinfilled %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                 maternal_ethnicityinfilled=="5Z" ~"6Z",
                                                 maternal_ethnicityinfilled %in% c("1E", "1F") ~ "1B",
                                                 maternal_ethnicityinfilled== "1J" ~ "1C",
                                                 T~ maternal_ethnicityinfilled)) %>%
  mutate(maternal_ethnicity_desc = ethnicity_labels(mat_ethnicity_mapped)) %>% 
  mutate(maternal_ethnicity_desc_infill = ethnicity_labels(mat_ethnicity_mapped_infill)) %>% 
  mutate(year_conception = year(est_date_conception)) %>%
  mutate(cancer_history = case_when(is.na(cancer_history) ~ 0, T~cancer_history)) %>%
  mutate(maternal_smoking = case_when(is.na(maternal_smoking) ~ "Unknown", 
                                      maternal_smoking=="smoker" ~ "Current smoker", 
                                      maternal_smoking=="ex-smoker" ~ "Former smoker", 
                                      maternal_smoking=="non-smoker" ~ "Never smoked", 
                                      T~maternal_smoking)) %>% 
  mutate(maternal_simd = case_when(is.na(maternal_simd) ~"Unknown", T~maternal_simd)) %>%
  mutate(maternal_simd = paste0("SIMD ", maternal_simd)) %>%
  mutate(baby_sex = case_when(is.na(baby_sex) ~ "Unknown", T~baby_sex)) %>%

  mutate(ind_sickle_cell = case_when(is.na(ind_sickle_cell) ~ 0, T~ind_sickle_cell)  ) %>%

  mutate(mtx_flag = case_when(mtx_PIS_conception==1 & mtx_PIS_to_12_wks==1 ~1, T~0),
         asm_flag = case_when(asm_conception==1 & asm_to_12wks==1 ~ 1, T~0))


df <- df %>% mutate(asm_anytime= convert_YN(asm_anytime),
                    asm_flag= convert_YN(asm_flag),
                    asm_conception = convert_YN(asm_conception),
                    asm_to_12wks = convert_YN(asm_to_12wks),
                    asm_after_12_wks = convert_YN(asm_after_12_wks),
                    mtx_flag= convert_YN(mtx_flag),
                    mtx_PIS_conception = convert_YN(mtx_PIS_conception),
                    mtx_PIS_to_12_wks = convert_YN(mtx_PIS_to_12_wks),
                    mtx_PIS_after_12wks = convert_YN(mtx_PIS_after_12wks)) 

source("ii.Exposures_timing.r")


byweek <-  df_tabs  %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  mutate(time_period = case_when(year(est_date_conception)<=2014 ~ "2010-2014", 
                                 year(est_date_conception)>=2015 & 
                                   year(est_date_conception)<=2018 ~ "2015-2018",
                                 year(est_date_conception)>=2019~ "2019-2022", T~"NA")) %>%
  group_by(first_hdfa_week, time_period) %>% count()

p<- ggplot(byweek, aes(x=as.numeric(first_hdfa_week), y=n, group=time_period)) +
  geom_line(aes(colour=time_period))+ylim(0,4000)+xlab("Gestation first exposed (weeks)")+
  ylab("N exposed pregnancies")+  labs(color = "Year of conception") +
  theme_bw()+
  theme(text = element_text(size = 10))

p

pdf( paste0(folder_data_path,"outputs/eFig1_timing_hdfa.pdf"), width = 10, height=8)
p
dev.off()

## Survival curves Any indication####
#
bootres <- readRDS( paste0(folder_data_path, "outputs/sens_any_bootres1_500.rds"))

trt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Treated = list(quibble(Treated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Treated) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Treated")

untrt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Untreated = list(quibble(Untreated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Untreated)%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Untreated")

bsCI <- rbind(trt, untrt)

results <- readRDS(paste0(folder_data_path, "outputs/sens_any_predictions_fitwts_l.rds"))
results <- left_join(results,bsCI)


#change lebelling to hdfa vs std care
results <- results %>% mutate(randf = case_when(randf =="Treated" ~ "hdFA", 
                                                randf =="Untreated" ~ "Standard care" ))
p2 <- ggplot() +
  geom_line(aes(x=results$time_interval, y = results$`50%`, colour= results$randf), linewidth=.4)+
  geom_point(aes(x=results$time_interval, y = results$`50%`, colour= results$randf), size=.6)+
  #  geom_line(aes(x=time, y = ci_lower,colour = group))+
  geom_ribbon(aes(x=results$time_interval, ymin= results$`CI2.5%`, ymax= results$`CI97.5%`,
                  colour= results$randf), linetype=2, alpha=0.1,  linewidth=.4) +
  ylim(0.990,1)+
  scale_y_continuous(limits = c(0.99,1), n.breaks = 3)+
  xlab("Age (Years)")+
  ylab("Probability of cancer free survival") +
#  ggtitle("Bootstrapped predicted IPTW Survival Curves: original data model and 95% CI") +
  labs(colour="Treatment group") +
  theme_bw(base_size=8) 
p2

pdf( paste0(folder_data_path,"outputs/eFigx_survival_any_ind.pdf"), width = 10, height=8)
p2
dev.off()
jpeg( paste0(folder_data_path,"outputs/eFigx_survival_any_ind.jpeg"), 
      width = 14, height=8, units="cm", res=300)
p2
dev.off()

emf( paste0(folder_data_path,"outputs/eFigx_survival_any_ind.emf"), width = 7, height=5)
p2 + theme_bw(base_size=10)
dev.off()

##Survival curves ASM indicator ####
bootres <- readRDS( paste0(folder_data_path, "outputs/sens_ASM_bootres1_500.rds"))

trt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Treated = list(quibble(Treated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Treated) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Treated")

untrt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Untreated = list(quibble(Untreated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Untreated)%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Untreated")

bsCI <- rbind(trt, untrt)

results <- readRDS(paste0(folder_data_path, "outputs/sens_ASM_predictions_fitwts_spline.rds"))
results <- left_join(results,bsCI)


#change lebelling to hdfa vs std care
results <- results %>% mutate(randf = case_when(randf =="Treated" ~ "hdFA", 
                                                randf =="Untreated" ~ "Standard care" ))
p <- ggplot() +
  geom_line(aes(x=results$time_interval, y = results$`50%`, colour= results$randf), linewidth=.4)+
  geom_point(aes(x=results$time_interval, y = results$`50%`, colour= results$randf), size=0.4)+
  geom_ribbon(aes(x=results$time_interval, ymin= results$`CI2.5%`, ymax= results$`CI97.5%`,
                  colour= results$randf), linetype=2, alpha=0.1, linewidth=.4)+
  ylim(0.990,1)+  xlab("Age (Years)") +
  ylab("Probability of cancer free survival") +
  labs(colour="Treatment group") +
  theme_bw(base_size=8) + scale_y_continuous(limits = c(0.99,1), n.breaks = 3)
p

pdf( paste0(folder_data_path,"outputs/eFigx_survival_ASM_ind.pdf"), width = 10, height=8)
p
dev.off()

p <- ggplot() +
  geom_line(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+
  geom_point(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+
  #  geom_line(aes(x=time, y = ci_lower,colour = group))+
  geom_ribbon(aes(x=results$time_interval, ymin= results$`CI2.5%`, ymax= results$`CI97.5%`,
                  colour= results$randf), linetype=2, alpha=0.1) +
  ylim(0.990,1)+  xlab("Age (Years)") +
  ylab("Probability of cancer free survival") +
  #  ggtitle("Bootstrapped predicted IPTW Survival Curves: mean and 95% CI") +
  labs(colour="Treatment group") +
  theme_bw(base_size = 8)
p


jpeg( paste0(folder_data_path,"outputs/eFigx_survival_ASM_ind.jpeg"),
      width =14, height=10, res=300, units = "cm" )
p
dev.off()

### Love plot main ####
library(cobalt)
library(WeightIt)

library(renv)
library(odbc)
library(dplyr)
library(janitor)
library(hablar)
library(lubridate)
library(phsmethods)
library(arrow)
library(readr)
library(tidyr)
library(ggplot2)

folder_data_path <- "/conf/FolicAcid/data/"


df<- readRDS(paste0(folder_data_path, "working_data/main_dataset_clean.rds"))
df <- df %>% mutate(hdfa_preg = case_when(hdfa_preg=="exposed_hdfa" ~1, 
                                          hdfa_preg=="unexposed_hdfa"~0))



##make nicer names for plots
df <- df %>%
  mutate(maternal_age  = as.numeric(maternal_age_conception)) %>%
  select(-maternal_ethnicity) %>%
  rename(SIMD = maternal_simd, maternal_ethnicity = mat_ethnicity_broad_groups, 
         history_NTD = ind_ntd, diabetes = ind_preexist_diabetes, 
         taking_ASM = asm_flag, taking_methotrexate = mtx_flag, 
         coeliac_disease = ind_coeliac ,  sickle_cell = ind_sickle_cell, 
         thalassaemia = ind_thalassaemia, obesity = obese)

W.out1 <- WeightIt::weightit(hdfa_preg ~
                               year_conception + maternal_age  +
                               year_conception*SIMD  + maternal_ethnicity + 
                               history_NTD + diabetes +  taking_ASM +
                               taking_methotrexate +obesity*year_conception  +
                               coeliac_disease + sickle_cell  + 
                               thalassaemia  +
                               cancer_history +
                               any_comorb +maternal_smoking, data = df,
                             estimand = "ATE",  # Find the ATE
                             method = "ps", stabilize=TRUE) 


b<-bal.tab(W.out1, stats = c("m", "v"),
           thresholds = c(m = .1), un=FALSE)

b2<-bal.tab(W.out1, stats = c("m", "v"),
           thresholds = c(m = .1), un=FALSE, binary="std")

bal_table1 <- b$Balance
bal_table1$varname <- rownames(bal_table1)
rownames(bal_table1) <- NULL
b2<- bal_table1 %>%
  mutate(varname = case_when(grepl("maternal_age_conception", varname) ~"maternal_age",
                             grepl("SIMD", varname) ~substr(varname,15,100), 
                             grepl("African, Scottish African or British African", varname) ~paste0("ethn", substr(varname,27,33)), 
                             grepl("Asian, Scottish Asian", varname) ~paste0("ethn", substr(varname,27,32)), 
                             grepl("Mixed or multiple ethnic groups", varname) ~paste0("ethn", substr(varname,27,32)), 
                             grepl("Caribbean or Black", varname) ~paste0("ethn", substr(varname,27,45)), 
                             grepl("Other ethnic group", varname) ~paste0("ethn", substr(varname,27,32)), 
                             grepl("groups_White", varname)~paste0("ethn", substr(varname,27,32)), 
                             grepl("groups_Unknown", varname) ~paste0("ethn", substr(varname,27,33)), 
                             T~varname)) %>% select(varname,Diff.Adj ,M.Threshold, V.Ratio.Adj)

new.names <- c(any_comorb = "Any comorbidity", 
               obesity = "Obesity",
               prop.score = "Propensity score",
               cancer_history = "Maternal history of cancer", 
               coeliac_disease = "Maternal coeliac disease", 
               sickle_cell  = "Maternal sickle cell disease",
               thalassaemia = "Maternal thalassaemia major",
               diabetes = "Pre-existing diabetes", 
               history_NTD = "Family history of NTD", 
               maternal_age = "Maternal age at conception",
               `maternal_ethnicity_African, Scottish African or British African` = 
                 "Ethnicity African Scottish African\n or British African", 
               `maternal_ethnicity_Asian, Scottish Asian or British Asian` =
                 "Ethnicity Asian, Scottish Asian\n or British Asian", 
               `maternal_ethnicity_Caribbean or Black` = "Ethnicity Caribbean or Black",
               `maternal_ethnicity_Mixed or multiple ethnic groups` = 
                 "Ethnicity Mixed or Multiple ethnic groups",
               `maternal_ethnicity_Other ethnic group` = 
                 "Ethnicity Other ethnic group", 
               `maternal_ethnicity_Unknown/unclassified`= "Ethnicity not known", 
               `maternal_ethnicity_White` = "Ethnicity White", 
               `maternal_smoking_Current smoker` = "Smoking status - current", 
               `maternal_smoking_Former smoker` = "Smoking status - former",
               `maternal_smoking_Never smoked` = "Smoking status - never",
               `maternal_smoking_Unknown` = "Smoking status - unknown",
               `SIMD_SIMD 1 (most deprived)` = "SIMD 1 (most deprived)",
               `SIMD_SIMD 2` = "SIMD 2",
               `SIMD_SIMD 3` = "SIMD 3",
               `SIMD_SIMD 4` = "SIMD 4",
               `SIMD_SIMD 5 (least deprived)` = "SIMD 5 (least deprived)",
               `SIMD_SIMD Unknown` = "SIMD Unknown", 
               taking_ASM = "Taking ASM",
               taking_methotrexate = "Taking methotrexate",
               year_conception = "Year of conception"
               ) 


love.plot(W.out1, thresholds = c(m = .05), 
          var.order = "alphabetical",  var.names = new.names)+
  theme(axis.text = element_text(size=8),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )



emf(file = paste0(folder_data_path,"outputs/eFigx_loveplot.emf"),
    width = 6.5, height=8.5,emfPlus = FALSE, emfPlusFontToPath=TRUE, family = "Arial")
love.plot(W.out1, thresholds = c(m = .1),
          stars= "raw", stats=c("m"), binary="std",
          var.names = new.names, var.order = "alphabetical")+
  theme(axis.text = element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )+
  xlab("Standardised Mean Differences")

dev.off()
love.plot(W.out1, thresholds = c(ks = .1),
          stars= "raw", stats= "ks",binary = "std",
          var.names = new.names, var.order = "alphabetical")+
  theme(axis.text = element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )#+
 # xlab("Standardised Mean Differences")

love.plot(W.out1, thresholds = c(m = .1),
          stars= "raw", stats= "m",
          var.names = new.names, var.order = "alphabetical")+
  theme(axis.text = element_text(size=10),binary = "std",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )#+
# xlab("Standardised Mean Differences")
###Plot of distribution of propensity scores ####
ps <- W.out1$ps
treat <- W.out1$treat
W.ate.stab <- W.out1$weights

wt.df <- as.data.frame(cbind(treat, ps, W.ate.stab))

wt.df <-wt.df  %>%
  mutate(treat_p1 = case_when(treat==1 ~ps , T~NA), 
         treat_p0 = case_when(treat==0 ~ps, T~NA))

ggplot(wt.df) + 
  geom_histogram(bins = 100,  aes(x = treat_p1,  fill="hdFA")) + 
  geom_histogram(bins = 100, aes(x = treat_p0, y = -..count.., fill="standard care")) + 
  ylab("N pregnancies") + xlab("propensity score") +
  geom_hline(yintercept = 0, lwd = 0.5) +
  scale_y_continuous(label = abs) +
  ggtitle("Propensity score distributions - count")+ 
  guides(fill=guide_legend(title="hdFA status"))


p_ipw <-ggplot(wt.df) + 
  geom_histogram(bins = 100,  aes(x = treat_p1, y = ..density.., fill="hdFA")) + 
  geom_histogram(bins = 100, aes(x = treat_p0, y = -..density.., fill="standard care")) + 
  ylab("% of group") + xlab("propensity score") +
  geom_hline(yintercept = 0, lwd = 0.5) +
  scale_y_continuous(label = abs) +
#  ggtitle("Propensity score distributions - proportions")+ 
  guides(fill=guide_legend(title="hdFA status"))+theme_bw()+
  theme(axis.text = element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )
p_ipw
emf(file = paste0(folder_data_path,"outputs/eFigx_propscores_wt.emf"),
    width = 6.5, height=6.5,emfPlus = FALSE, emfPlusFontToPath=TRUE, family = "Arial")
p_ipw +
  theme(axis.text = element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2)  )
dev.off()

###prescribing over time by indication####
df<- readRDS(paste0(folder_data_path, "working_data/main_dataset.rds"))
## data prep####
##remove invalid chis
df <- df %>%  mutate(check_mother_chi = chi_check(mother_upi), check_baby_chi = chi_check(baby_upi)) %>%
  filter(check_mother_chi=="Valid CHI" & check_baby_chi=="Valid CHI") %>%
  filter(maternal_age_conception >=18 & maternal_age_conception <=49) %>%
  filter(gest_end_pregnancy>=20)
scottish_pc <- c("AB", "DD", "DG", "EH", "FK", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", 
                 "IV", "HS" , "KA", "KW", "KY", "ML", "PA", "PH", "TD", "ZE", "NK")# also allow nk for not known
df <- df %>% 
  mutate(non_scot_pc_booking = case_when(!is.na(maternal_postcode_booking) &
                                           !substr(maternal_postcode_booking,1,2) %in% scottish_pc~1, T~0)) %>%
  mutate(non_scot_pc_end= case_when(!is.na(maternal_postcode_end_preg) &
                                      !substr(maternal_postcode_end_preg,1,2) %in% scottish_pc~1, T~0)) %>%
  filter(non_scot_pc_booking==0 & non_scot_pc_end==0)
#remove cancer outcomes after emigration date
df<- df %>% mutate(cancer_outcome = case_when(incidence_date > DATE_TRANSFER_OUT~0, T~cancer_outcome))
##check deaths after censor date
df<- df %>%
  mutate(end_follow_type = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31") & end_follow_type=="censor - death" ~ "end study", 
                     T~end_follow_type),
         end_follow = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31")  ~ as.POSIXct("2023-12-31"), T~end_follow)  )
##ethnicity mapping - 2011-2022 versions
df <- df %>% mutate(mat_ethnicity_mapped = case_when(maternal_ethnicity %in% c("4D",  "4Y") ~ "4X", 
                                                     maternal_ethnicity %in% c("5C", "5D", "5Y") ~ "5X", 
                                                     maternal_ethnicity %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                     maternal_ethnicity=="5Z" ~"6Z",
                                                     maternal_ethnicity %in% c("1E", "1F") ~ "1B",
                                                     maternal_ethnicity== "1J" ~ "1C",
                                                     T~ maternal_ethnicity)) %>%
  ##for comparisons - also do infilled ethnicity
  mutate(mat_ethnicity_mapped_infill = case_when(maternal_ethnicityinfilled %in% c("4D",  "4Y") ~ "4X", 
                                                 maternal_ethnicityinfilled %in% c("5C", "5D", "5Y") ~ "5X", 
                                                 maternal_ethnicityinfilled %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                 maternal_ethnicityinfilled=="5Z" ~"6Z",
                                                 maternal_ethnicityinfilled %in% c("1E", "1F") ~ "1B",
                                                 maternal_ethnicityinfilled== "1J" ~ "1C",
                                                 T~ maternal_ethnicityinfilled)) %>%
  mutate(maternal_ethnicity_desc = ethnicity_labels(mat_ethnicity_mapped)) %>% 
  mutate(maternal_ethnicity_desc_infill = ethnicity_labels(mat_ethnicity_mapped_infill)) %>% 
  mutate(year_conception = year(est_date_conception)) %>%
  mutate(cancer_history = case_when(is.na(cancer_history) ~ 0, T~cancer_history)) %>%
  mutate(maternal_smoking = case_when(is.na(maternal_smoking) ~ "Unknown", 
                                      maternal_smoking=="smoker" ~ "Current smoker", 
                                      maternal_smoking=="ex-smoker" ~ "Former smoker", 
                                      maternal_smoking=="non-smoker" ~ "Never smoked", 
                                      T~maternal_smoking)) %>% 
  mutate(maternal_simd = case_when(is.na(maternal_simd) ~"Unknown", T~maternal_simd)) %>%
  mutate(maternal_simd = paste0("SIMD ", maternal_simd)) %>%
  mutate(baby_sex = case_when(is.na(baby_sex) ~ "Unknown", T~baby_sex)) %>%

  mutate(ind_sickle_cell = case_when(is.na(ind_sickle_cell) ~ 0, T~ind_sickle_cell)  ) %>%
  mutate(mtx_flag = case_when(mtx_PIS_conception==1 & mtx_PIS_to_12_wks==1 ~1, T~0),
         asm_flag = case_when(asm_conception==1 & asm_to_12wks==1 ~ 1, T~0))


df <- df %>% mutate(asm_anytime= convert_YN(asm_anytime),
                    asm_flag= convert_YN(asm_flag),
                    asm_conception = convert_YN(asm_conception),
                    asm_to_12wks = convert_YN(asm_to_12wks),
                    asm_after_12_wks = convert_YN(asm_after_12_wks),
                    mtx_flag= convert_YN(mtx_flag),
                    mtx_PIS_conception = convert_YN(mtx_PIS_conception),
                    mtx_PIS_to_12_wks = convert_YN(mtx_PIS_to_12_wks),
                    mtx_PIS_after_12wks = convert_YN(mtx_PIS_after_12wks)) 

##obesity
obesity_grp <- df %>% filter(obese==1)

obesity_tab <- obesity_grp %>% group_by(hdfa_preg, year_conception) %>%
  count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n) %>%
  mutate(percent_treated= exposed_hdfa/(exposed_hdfa+unexposed_hdfa)*100 ) %>%
  mutate(indicator="Obesity")

##diabetes
dm_grp <- df %>% filter(ind_preexist_diabetes==1)

dm_tab <- dm_grp %>% group_by(hdfa_preg, year_conception) %>%
  count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n) %>%
  mutate(percent_treated= exposed_hdfa/(exposed_hdfa+unexposed_hdfa)*100 ) %>%
  mutate(indicator="Pre-existing diabetes")



##asm
asm_grp <- df %>% filter(asm_flag =="Yes")

asm_tab <- asm_grp %>% group_by(hdfa_preg, year_conception) %>%
  count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n) %>%
  mutate(percent_treated= exposed_hdfa/(exposed_hdfa+unexposed_hdfa)*100 ) %>%
  mutate(indicator="Taking ASM in pregnancy")

all_grps <- rbind(dm_tab, obesity_tab, asm_tab)

p<- ggplot(all_grps , aes(x=year_conception, y=percent_treated)) +
  geom_line()+
  facet_wrap(vars(indicator), nrow=2)+
  ylim(0,100)+xlab("Year of conception")+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018, 2020,2022))+
  ylab("% pregnancies treated")+  
  #ggtitle("Percentage of pregnancies to women with\n obesity treated with hdFA")+
  theme(axis.title = element_text(size=10), 
        axis.text = element_text(size=10),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.2) )#+
 # theme_bw(base_size = 10)

p

emf(file = paste0(folder_data_path,"outputs/eFigx_trend_indications.emf"),
    width = 5.5, height=4,emfPlus = FALSE, emfPlusFontToPath=TRUE, family = "Arial"
    )
p
dev.off()
