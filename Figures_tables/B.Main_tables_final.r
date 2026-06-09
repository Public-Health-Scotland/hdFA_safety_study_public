#Final main tables####
###libraries and setup####
source("00.setup.r")
library(tidyr)
library(openxlsx)
library(readr)
library(readxl)
library(epiR)
library(reshape2)

##Functions
##new version of tab_fun function to combine the N and % into one column
##table function
tab_fun <- function(df,x, varname){
 
  var <- sym(x)
  df %>%
    group_by(!!var, hdfa_preg) %>% count()%>%
    pivot_wider(names_from = hdfa_preg, values_from=n) %>%
    mutate(var=varname)%>%
    rename(values =!!var) %>%
    ungroup() %>% 
    mutate(exposed_hdfa = replace_na(exposed_hdfa,0), unexposed_hdfa = replace_na(unexposed_hdfa, 0)) %>%
    mutate(total_ex = sum(exposed_hdfa, na.rm=T), total_un = sum(unexposed_hdfa)) %>% 
    mutate(Total = exposed_hdfa+ unexposed_hdfa) %>%
    mutate(`% of Total` = Total/total_cohort*100) %>%
    mutate(`% of Total` = format(round_half_up(`% of Total`,1),nsmall=1))  %>%
    mutate(perc_exposed = exposed_hdfa/total_ex *100,
           perc_unexp= unexposed_hdfa/total_un*100) %>%
    mutate(perc_exposed = str_trim(format(round_half_up(perc_exposed,1),nsmall=1)),
           perc_unexp= str_trim(format(round_half_up(perc_unexp,1),nsmall=1)) )%>%
    mutate(perc_unexp = case_when(perc_unexp %in% c("  0.0"," 0.0","0.0" ) & unexposed_hdfa!=0 ~ "<0.1",
                                  perc_unexp == "100.0" & unexposed_hdfa!=total_un ~ ">99.9",
                                  T~as.character(perc_unexp)),
           perc_exposed = case_when(perc_exposed %in% c("  0.0"," 0.0","0.0" ) & exposed_hdfa!=0 ~ "<0.1",
                                    perc_exposed  == "100.0"  & exposed_hdfa!=total_ex~ ">99.9",
                                    T~as.character(perc_exposed))) %>%
    mutate(`% of Total` = case_when(`% of Total`=="   NA"~ "", 
                                    `% of Total`%in% c(" 0.0", "  0.0") & Total !=0 ~"<0.1",
                                    T~`% of Total`) )%>%
    mutate(exposed_hdfa = formatC(exposed_hdfa, big.mark=","),
           unexposed_hdfa = formatC(unexposed_hdfa, big.mark=","), 
           Total = formatC(Total, big.mark=",")) %>%
    select(-total_ex, -total_un) %>%
    mutate(exposed = paste0(exposed_hdfa, " (", perc_exposed, ")" )) %>%
    mutate(unexposed = paste0(unexposed_hdfa, " (", perc_unexp, ")" )) %>%
    mutate(Total = paste0(Total," (",  `% of Total`, ")" )) %>%
    select(var, values, Total, exposed, unexposed)
  
}
tab_add_heads <- function(tab){
  tab %>% group_by(var) %>%
    slice(1)  %>% 
    ungroup() %>%
    mutate(across(-var, ~ .[NA])) %>%
    bind_rows(tab) %>% 
    arrange(var) %>%
    mutate(values_by_var = case_when(is.na(values)~ var, 
                                     T~ paste0("  ", values))) %>%
    select(values_by_var, Total, exposed,  unexposed)
}

convert_YN <- function(x){case_when(x==1 ~ "Yes", 
                                    x==0 ~ "No", 
                                    is.na(x)~ "No", 
                                    T~ as.character(x))}

###Create  workbook  ####
workbook <- createWorkbook()
## set styles ###

bold.style <- createStyle(textDecoration = "Bold",fgFill = "white")
head.style <- createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white", border = "Bottom")
bottom.style <- createStyle(wrapText = TRUE,  border = "Bottom",fgFill = "white")
##bottom style for Table1 with merged cells - align text to top
bottom.style2 <- createStyle(wrapText = TRUE,  border = "Bottom",fgFill = "white", valign = "top")

centre.style <- createStyle(halign = "center", 
                            fgFill="white", border="top", textDecoration = "bold")
##Create contents page####
addWorksheet(workbook, "Contents")
##add worksheets####
addWorksheet(workbook, "Table1", tabColour = "yellow")

writeFormula(
  workbook,"Table1",
  x = '=HYPERLINK(\"#Contents!A3\",
  "Back to contents")',
  startCol = 7, startRow = 1
)
### Table1 sheet ####
Table1<-  readxl::read_excel("/conf/FolicAcid/data/outputs/Tables_and_Figures.xlsx", 
                             sheet = "T1 TTE", range = "A3:C36")
sheet1_caption <-
  "Table 1: Specification of the Target Trial and its Emulation to estimate the causal effect of high dose folic acid supplementation during pregnancy on the risk of childhood cancer"
writeData(workbook, "Table1", sheet1_caption,
          startRow = 1, startCol = 1)

addStyle(workbook, "Table1", bold.style, rows = 1, cols = 1, gridExpand = TRUE)

writeData(workbook, "Table1", Table1,
          startRow = 3, startCol = 1, headerStyle = head.style)

addStyle(workbook,  "Table1", style = createStyle(fgFill = "white", wrapText = TRUE, valign = "top"), rows = 3:36,
         cols = 1:100, gridExpand = TRUE)
addStyle(workbook, "Table1", bold.style, rows = c(1:3), cols = 1:3, gridExpand = TRUE)
setColWidths(workbook,  "Table1", cols = c(1:3), widths = c(27,52,52))

addStyle(workbook, "Table1", head.style, rows = 3, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 15, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 19, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 20, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 24, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 29, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 33, cols = c(1:3))
addStyle(workbook, "Table1", bottom.style2, rows = 36, cols = c(1:3))

rangeRows = 3:36
rangeCols = 1:3
## left borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table1",
  style = openxlsx::createStyle(
    border = c("left"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = rangeCols[1],
  stack = TRUE,
  gridExpand = TRUE
)

##right borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table1",
  style = openxlsx::createStyle(
    border = c("right"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = tail(rangeCols, 1),
  stack = TRUE,
  gridExpand = TRUE
)
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table1",
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows = rangeRows[1],
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

###add borders for columns
rangeRows = 3:36
rangeCols = 2
## left borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table1",
  style = openxlsx::createStyle(
    border = c("left"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = rangeCols[1],
  stack = TRUE,
  gridExpand = TRUE
)
##right borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table1",
  style = openxlsx::createStyle(
    border = c("right"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = tail(rangeCols, 1),
  stack = TRUE,
  gridExpand = TRUE
)


###Table 2 sheet ####
addWorksheet(workbook, "Table_2", tabColour = "yellow")
##add white background to everything
addStyle(workbook, "Table_2", style = createStyle(fgFill = "white"), rows = 1:200, cols = 1:100, gridExpand = TRUE)

writeFormula(
  workbook, "Table_2",
  x = '=HYPERLINK(\"#Contents!A3\",
  "Back to contents")',
  startCol = 8, startRow = 1
)
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


###  grouped ethnicity 
df <- df %>% mutate(mat_ethnicity_broad_groups = 
                      case_when(grepl("white", maternal_ethnicity_desc, ignore.case=T) |
                                  maternal_ethnicity_desc %in% c("Scottish", "Gypsy/Traveller", "Other British","Polish") |
                                  grepl("Irish", maternal_ethnicity_desc, ignore.case=T) ~ "White", 
                                maternal_ethnicity_desc %in% 
                                  c("African, Scottish African or British African", "African, African Scottish or African British",
                                    "OLD_CODEAfrican")  ~
                                  "African, Scottish African or British African",
                                grepl("Bangladeshi", maternal_ethnicity_desc, ignore.case=T)|
                                  grepl("Asian", maternal_ethnicity_desc, ignore.case=T)|
                                  grepl("Chinese", maternal_ethnicity_desc, ignore.case=T)|
                                  grepl("Indian", maternal_ethnicity_desc, ignore.case=T)|
                                  grepl("Pakistani", maternal_ethnicity_desc, ignore.case=T) ~
                                  "Asian, Scottish Asian or British Asian",
                                maternal_ethnicity_desc %in%
                                  c("Caribbean, Caribbean Scottish or Caribbean British",
                                    "Caribbean or Black", "OLD_CODEAny other black background", 
                                    "Black, Black Scottish or Black British" ) ~"Caribbean or Black",
                                grepl("mixed", maternal_ethnicity_desc, ignore.case=T)  ~"Mixed or multiple ethnic groups",
                                grepl("Arab", maternal_ethnicity_desc, ignore.case=T)|
                                  maternal_ethnicity_desc %in% c("Other ethnic group", "OLD_CODEAny other ethnic background") ~"Other ethnic group",
                                T~ "Unknown/unclassified")) %>%
  
  mutate(mat_ethnicity_broad_groups_fill = 
           case_when(grepl("white", maternal_ethnicity_desc_infill, ignore.case=T) |
                       maternal_ethnicity_desc_infill %in% c("Scottish", "Gypsy/Traveller", "Other British","Polish") |
                       grepl("Irish", maternal_ethnicity_desc_infill, ignore.case=T) ~ "White", 
                     maternal_ethnicity_desc_infill %in% 
                       c("African, Scottish African or British African", "African, African Scottish or African British",
                         "OLD_CODEAfrican")  ~
                       "African, Scottish African or British African",
                     grepl("Bangladeshi", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Asian", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Chinese", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Indian", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Pakistani", maternal_ethnicity_desc_infill, ignore.case=T) ~
                       "Asian, Scottish Asian or British Asian",
                     maternal_ethnicity_desc_infill %in%
                       c("Caribbean, Caribbean Scottish or Caribbean British",
                         "Caribbean or Black", "OLD_CODEAny other black background", 
                         "Black, Black Scottish or Black British" ) ~"Caribbean or Black",
                     grepl("mixed", maternal_ethnicity_desc_infill, ignore.case=T)  ~"Mixed or multiple ethnic groups",
                     grepl("Arab", maternal_ethnicity_desc_infill, ignore.case=T)|
                       maternal_ethnicity_desc_infill %in% c("Other ethnic group", "OLD_CODEAny other ethnic background") ~"Other ethnic group",
                     T~ "Unknown/unclassified"))


df <- df %>% mutate(cancer_history = convert_YN(cancer_history), 
                    any_comorb = convert_YN(any_comorb),
                    ind_ntd= convert_YN(ind_ntd),
                    ind_preexist_diabetes=  convert_YN(ind_preexist_diabetes),
                    ind_coeliac=  convert_YN(ind_coeliac),
                    ind_thalassaemia = convert_YN(ind_thalassaemia),
                    ind_sickle_cell= convert_YN(ind_sickle_cell),
                    obese= convert_YN(obese),
                    asm_anytime= convert_YN(asm_anytime),
                    mtx_anytime= convert_YN(mtx_anytime) ,
                    asm_flag= convert_YN(asm_flag),
                    asm_conception = convert_YN(asm_conception),
                    asm_to_12wks = convert_YN(asm_to_12wks),
                    asm_after_12_wks = convert_YN(asm_after_12_wks),
                    mtx_flag= convert_YN(mtx_flag),
                    mtx_PIS_conception = convert_YN(mtx_PIS_conception),
                    mtx_PIS_to_12_wks = convert_YN(mtx_PIS_to_12_wks),
                    mtx_PIS_after_12wks = convert_YN(mtx_PIS_after_12wks),
                    any_congenital_condition = convert_YN(any_congenital_condition)) 
##exposure numbers####
t_n_exposed <- df %>% group_by(hdfa_preg) %>% count() %>% pivot_wider(names_from = hdfa_preg, values_from = n)
t_n_exposed$var <- "Total"
t_n_exposed$values <- NA
total_cohort <- nrow(df)
t_n_exposed <-t_n_exposed %>%
  ungroup() %>%
  ##row totals for the total exposures
  mutate(perc_exposed = (exposed_hdfa/(exposed_hdfa+ unexposed_hdfa)) *100,
         perc_unexp= ( unexposed_hdfa/(exposed_hdfa+ unexposed_hdfa))*100 ) %>%
  mutate(perc_exposed = case_when(perc_exposed < 1~round_half_up(perc_exposed,2),
                                  perc_exposed >= 1~round_half_up(perc_exposed,1)),
         perc_unexp= case_when(perc_unexp <1 ~ round_half_up(perc_unexp,2), 
                               perc_unexp >=1 ~ round_half_up(perc_unexp,1) )) %>% 
  mutate(Total = exposed_hdfa+ unexposed_hdfa) %>% 
  mutate(`% of Total` = Total/total_cohort*100) %>%
  mutate(`% of Total` = format(round_half_up(`% of Total`,1),nsmall=1)) %>%
  mutate(exposed = paste0(exposed_hdfa, " (", perc_exposed, ")" )) %>%
  mutate(unexposed = paste0(unexposed_hdfa, " (", perc_unexp, ")" )) %>%
  mutate(Total = paste0(Total," (",  `% of Total`, ")" )) %>%
  select(var, values, Total, exposed, unexposed) %>%
  tab_add_heads() 

t_n_exposed$values_by_var <- c(NA, "Total")


##Maternal factors####
###year conception#
t_yr <- tab_fun(df, "year_conception", "Year of conception")%>%
  tab_add_heads()

###Mother age##
df<- df %>% mutate(age_group = factor(age_group, c("<20" , "20-24", "25-29", "30-34", "35-39", "40+" )))
t_age <- tab_fun(df, "age_group", "Maternal age at conception (years)")%>%
  tab_add_heads()
###Maternal simd##
t_simd <- tab_fun(df, "maternal_simd", "Maternal deprivation")%>%
  tab_add_heads()
###maternal ethnicity#
ethnic_group_order <- c("African, Scottish African or British African", 
                        "Asian, Scottish Asian or British Asian",
                        "Caribbean or Black", "Mixed or multiple ethnic groups",
                        "White",
                        "Other ethnic group",
                        "Unknown/unclassified")
df <- df %>% 
  mutate(mat_ethnicity_broad_groups_fill = factor(mat_ethnicity_broad_groups_fill,ethnic_group_order ))


t_ethnicity_broad_filled <- tab_fun(df, "mat_ethnicity_broad_groups_fill", "Maternal ethnicity")%>%
  tab_add_heads()
###mother BMI##
t_bmi <- tab_fun(df, "bmi_group", "Maternal BMI at antenatal booking")%>%
  tab_add_heads()

###mother cancer history
t_cancer_history <-tab_fun(df, "cancer_history", "Maternal cancer history")%>%
  tab_add_heads()
###Mother comorbidities and inidcators#
###ASM use###
t_asm1  <-tab_fun(df, "asm_flag", "Maternal ASM use \u1d48")%>%
  tab_add_heads()

###Methotrexate##
t_mtx1 <- tab_fun(df, "mtx_flag", "Maternal methotrexate use")%>%
  tab_add_heads()

###Coeliac##
t_coeliac <- tab_fun(df, "ind_coeliac", "Maternal coeliac disease")%>%
  tab_add_heads()

###obesity
df<- df %>% mutate(obese = case_when(bmi_group=="Obese"~ "Yes", 
                                     bmi_group=="Unknown"~ "Unknown",T~"No" )) %>%
  mutate(obese = factor(obese, c( "No", "Yes","Unknown")))
t_obese  <- tab_fun(df, "obese", "Maternal obesity at antenatal booking")%>%
  tab_add_heads()

###Thalessaemia major##
t_thaless  <- tab_fun(df, "ind_thalassaemia", "Maternal thalassaemia major")%>%
  tab_add_heads()

###pre-existing diabetes
t_diabetes <-tab_fun(df, "ind_preexist_diabetes", "Maternal pre-pregnancy diabetes")%>%
  tab_add_heads()

###SIckle cell
t_sickle <-tab_fun(df, "ind_sickle_cell", "Maternal sickle cell anaemia") %>%
  tab_add_heads()

##Family history NTD
t_ntd <-tab_fun(df, "ind_ntd", "Family history of NTD \u1d9c") %>%
  tab_add_heads()
##any indicator
df <-df %>% mutate(any_indicator = case_when( obese=="Yes" | ind_ntd=="Yes" | ind_coeliac=="Yes" |
                                                ind_sickle_cell=="Yes" |ind_thalassaemia=="Yes" |
                                                ind_preexist_diabetes=="Yes" |mtx_flag=="Yes"| asm_flag=="Yes" ~"Yes" ,
                                              T~"No"))
t_any_ind <-tab_fun(df, "any_indicator", "Any indication for hdFA \u1d47") %>%
  tab_add_heads()

##smoking###
t_smoke <-tab_fun(df, "maternal_smoking", "Maternal smoking at antenatal booking")%>%
  tab_add_heads()
###Comorbidity#
####ANY#
t_comorb <-tab_fun(df, "any_comorb", "Maternal comorbidity (excluding indication for hdFA or history of cancer)")%>%
  tab_add_heads()

total_cohort <- (t_n_exposed$exposed_hdfa[2] + t_n_exposed$unexposed_hdfa[2])

##build table####
blank_row <- t_n_exposed %>% slice(1)
indications <- blank_row %>% mutate(values_by_var="Indications for hdFA \u1d47")
t2_baseline <- rbind(t_n_exposed,blank_row ,
                     t_yr,  blank_row, t_age,  blank_row,
                     t_simd,  blank_row, t_ethnicity_broad_filled,  blank_row,
                     blank_row, t_ntd,  blank_row, t_diabetes,  blank_row,t_obese, blank_row ,
                     t_asm1,  blank_row, t_mtx1,  blank_row,
                     t_coeliac,  blank_row,t_sickle,  blank_row,t_thaless,   blank_row,
                     t_any_ind,  blank_row,
                     t_cancer_history,  blank_row,
                     t_comorb,   blank_row,
                     t_smoke) %>% rename(Characteristics = values_by_var)

##Add references to footnotes - use UNICODE superscript
t2_baseline <-  t2_baseline  %>% 
  mutate(Characteristics = case_when(Characteristics=="  SIMD 1 (most deprived)" ~ "  SIMD 1 (most deprived)\u1d43",##superscript a
                                     T~Characteristics))
names(t2_baseline) <- c("Characteristics", "N (%)", "N (%)", "N (%)")
##table headers

writeData(workbook, "Table_2","All singleton live births", startRow = 3, startCol = 2)
writeData(workbook, "Table_2","Treated with high dose folic acid", startRow = 3, startCol = 3)

writeData(workbook, "Table_2","Not treated with high dose folic acid", startRow = 3, startCol = 4)

addStyle(workbook, sheet = "Table_2", centre.style,
         rows = 3, cols = 2:4, gridExpand = TRUE)
##add bold styling 
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "Maternal", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "year of", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "sex", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "indicat", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "total", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "cancer", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains", rule = "birthweight", style = bold.style)
conditionalFormatting(workbook, "Table_2", cols = 1, rows = 1:200, 
                      type = "contains",rule = "family", style = bold.style)


writeData(workbook, "Table_2",
          "Table 2: Baseline characteristics of included pregnancies, stratified by treatment group",
          startCol = 1, startRow = 1)

##footnotes
writeData(workbook, "Table_2",
          "\u1d43 SIMD = Scottish Index of Multiple Deprivation",
          startRow = 101, startCol = 1)
writeData(workbook, "Table_2",
          "\u1d47 hdFA = high dose folic acid",
          startRow = 102, startCol = 1)
writeData(workbook, "Table_2",
          "\u1d9c NTD =  neural tube defect",
          startRow = 103, startCol = 1)
writeData(workbook, "Table_2",
          "\u1d48 ASM = antiseizure medication",
          startRow = 104, startCol = 1)



##make caption bold
addStyle(workbook, "Table_2", bold.style, rows = c(1,4), cols = 1:4, gridExpand = TRUE)
##write table with header styling

for (row in 1:nrow(t2_baseline)){
  sheetRow <- data.frame(lapply(t2_baseline[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook,"Table_2", x = sheetRow, startRow = row+3, colNames = TRUE,  
              headerStyle = head.style)
  } else {
    writeData(workbook, "Table_2", x = sheetRow, startRow = row+4, colNames = FALSE)
  }
}

writeData(workbook, "Table_2","Indications for hdFA \u1d47", startRow = 48, startCol = 1)
setColWidths(workbook, "Table_2", cols = c(1:4), widths = c(65,34,34,34))

addStyle(workbook, "Table_2", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "0.0", ), rows = 5:100, cols = c(2,3,4), gridExpand = TRUE)
addStyle(workbook, "Table_2", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "0.0",border="Bottom" ),
         rows = nrow(t2_baseline)+4, cols = c(2,3,4), gridExpand = TRUE)

##left align fist column (otherwise years are right aligned.)
addStyle(workbook, "Table_2", 
         createStyle(halign="left",fgFill="white", 
         ), rows = 1:nrow(t2_baseline)+4, cols = 1, gridExpand = TRUE)

rangeRows = 3
rangeCols = 1:4
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table_2", 
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows = c(3,5),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

## left internal borders
rangeRows = 3:98
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table_2",
  style = openxlsx::createStyle(
    border = c("left"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = c(2,3,4),
  stack = TRUE,
  gridExpand = TRUE
)
##bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "Table_2",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

### Model result table ####
#
addWorksheet(workbook, "T4_model_results")
addStyle(workbook,  "T4_model_results", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)

title<- "Table 4: Modelled cumulative risk of childhood cancer by specified age points, and overall relative risk in included singleton live births treated, compared to not treated, with high dose folic acid"

writeData(workbook,  "T4_model_results",
          title,
          startRow = 1, startCol = 1)
setColWidths(workbook,"T4_model_results", cols = c(1:7), widths = c(32,32,32,32, 32, 32, 32))
addStyle(workbook,  "T4_model_results",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

##Singlton LBs
n_lb <- df %>% group_by(hdfa_preg) %>% count() %>% ungroup() 
n_lb <- n_lb %>% pivot_wider(names_from = hdfa_preg, values_from = n) %>% mutate(col1 = "N live births")

df <- df %>%  mutate(age_yr_at_diag = interval(date_end_pregnancy, incidence_date) /years(1)) %>% 
  mutate(incid_0_1 = case_when(cancer_outcome==1 & age_yr_at_diag <1 ~1, T~0), 
         incid_1_4 = case_when(cancer_outcome==1 & age_yr_at_diag>=1 & cancer_outcome==1 & age_yr_at_diag <5 ~1, T~0), 
         incid_5_9 = case_when(cancer_outcome==1 & age_yr_at_diag >=5 & age_yr_at_diag <10 ~1, T~0), 
         incid_10_14 = case_when(cancer_outcome==1 & age_yr_at_diag >=10 & age_yr_at_diag <15 ~1, T~0) ) 
##cuminc 
n_event_1 <- df %>% filter(incid_0_1==1) %>% group_by(hdfa_preg) %>% 
  count() %>% mutate(age = "1 year")
n_event_5 <- df %>% filter(incid_0_1==1|incid_1_4==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "5 year")
n_event_10 <- df %>% filter(incid_0_1==1|incid_1_4==1|incid_5_9==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "10 year")
n_event_end <-  df %>% filter(incid_0_1==1|incid_1_4==1 | incid_5_9==1|incid_10_14==1 )%>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "End of follow up")

events <- rbind(n_event_1, n_event_5, n_event_10, n_event_end)
events <- events %>% pivot_wider(names_from = hdfa_preg, values_from = n) 
names(events) <- c("Age", "exposed_events", "unexposed_events")


##cumulative risks
##load main model results
results <- readRDS(paste0(folder_data_path, "outputs/predictions_fitwts_spline.rds"))
wideres_unwt <- readRDS(paste0(folder_data_path, "outputs/predict_surv_unwt_spline.rds"))

##wide res
wideres <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')

# Create summary statistics
wideres <- wideres%>%
  mutate(
    RD = (1-Treated) - (1-Untreated),
    logRatio = log(Treated)/log(Untreated),
    CumIncTrt = 1-Treated, 
    CumIncUntrt = 1- Untreated,
    CIR = (1-Treated)/ (1-Untreated)
  )
wideres$logRatio[1] <- NA
wideres$cHR <- sapply(0:13, FUN=function(x){mean(wideres$logRatio[wideres$time_interval <= x], na.rm=T)})


##load bootstrapped results
bootres<-readRDS("/conf/FolicAcid/data/outputs/main_bootres.rds" )
quibble <- function(x, q = c(0.025,0.5, 0.975), dropNA = TRUE) {
  tibble(x = quantile(x, q, na.rm = dropNA), q = q)
}
##pull out HR and 95% CI for the final timepoint
bootres_cHR_summary <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(cHR = list(quibble(cHR, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(cHR) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`chr_CI2.5%` = `0.025`, `chr_CI97.5%` =  `0.975`,  `chr_50%` =`0.5`) %>%
  mutate(cHR_modelled = wideres$cHR[wideres$time_interval==13]) %>%
  filter(time_interval==13)


## Cumulative risk####
bootres <- bootres %>% mutate(CumIncTrt = 1-Treated, 
                              CumIncUntrt = 1- Untreated)
trt_cuminc <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(CumIncTrt = list(quibble(CumIncTrt , c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(CumIncTrt ) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CumInc CI2.5%` = `0.025`, `CumInc CI97.5%` =  `0.975`,  `CumInc 50%` =`0.5`) %>%
  mutate(randf="Treated")%>% 
  filter(time_interval %in% c(1,5,10,13))

untrt_cuminc <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(CumIncUntrt = list(quibble(CumIncUntrt , c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(CumIncUntrt )%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CumInc CI2.5%` = `0.025`, `CumInc CI97.5%` =  `0.975`,  `CumInc 50%` =`0.5`) %>%
  mutate(randf="Untreated")%>% 
  filter(time_interval %in% c(1,5,10,13))

bs_cuminc <- rbind(trt_cuminc , untrt_cuminc )
result_trt <- wideres %>% filter(time_interval %in% c(1,5,10,13)) %>%
  select(time_interval, CumIncTrt) %>%
  mutate(randf = "Treated") %>% rename(CumInc = CumIncTrt)

result_untrt <- wideres %>% filter(time_interval %in% c(1,5,10,13)) %>%
  select(time_interval, CumIncUntrt) %>%
  mutate(randf = "Untreated") %>% rename(CumInc = CumIncUntrt)

result_cuminc <- rbind(result_untrt, result_trt)
bs_cuminc<-left_join(bs_cuminc, result_cuminc)

bs_cuminc <- bs_cuminc %>% 
  mutate(cumrisk = paste0(round_half_up(`CumInc 50%`*100000,1), " (", 
                           round_half_up(`CumInc CI2.5%`*100000,1)," - " ,
                           round_half_up(`CumInc CI97.5%`*100000,1), ")" )) %>%
  select(time_interval, randf, cumrisk) %>%
  pivot_wider(names_from = randf, values_from = c(cumrisk)) %>%
  mutate(time_interval = paste0(time_interval, " year")) %>%
  mutate(time_interval = case_when(time_interval== "13 year" ~ "End of follow up", 
                                   T~time_interval))%>%
  rename(Age = time_interval)

##risk difference bootstrapped summary
RD_sum <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(RD = list(quibble(RD , c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(RD) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`RD CI2.5%` = `0.025`, `RD CI97.5%` =  `0.975`,  `RD 50%` =`0.5`) %>%
  filter(time_interval %in% c(1,5,10,13)) 

model_RD<- wideres %>% 
  filter(time_interval %in% c(1,5,10,13)) %>% 
  select(time_interval, RD)
  
RD <- left_join(RD_sum, model_RD) %>%
  mutate(RD =  paste0(  round_half_up(`RD 50%`*100000,1), " (", 
                       round_half_up(`RD CI2.5%`*100000,1)," to " ,
                       round_half_up(`RD CI97.5%`*100000,1), ")" )) %>%
  select(time_interval, RD) %>%
  mutate(time_interval = paste0(time_interval, " year")) %>%
  mutate(time_interval = case_when(time_interval== "13 year" ~ "End of follow up", 
                                   T~time_interval)) %>%
  rename(Age = time_interval)

tab <- left_join(events, bs_cuminc)
tab <- left_join(tab, RD)

##manually paste the overall RR and CIs into cell
writeData(workbook,   "T4_model_results",tab,
          startRow = 4, startCol = 1, headerStyle = head.style)
writeData(workbook, "T4_model_results", " ",
          startRow = 3, startCol = 1)

mergeCells(workbook,"T4_model_results", cols = 4:5, rows = 3)
writeData(workbook,   "T4_model_results","Cumulative risk (per 100,000 individuals)", 
          startRow = 3, startCol = 4, headerStyle = head.style)
mergeCells(workbook,"T4_model_results", cols = 7, rows = 3)
writeData(workbook, "T4_model_results", "Relative risk of being diagnosed with cancer (95%CI)",
          startRow = 3, startCol = 7)

RR <- paste0(round_half_up(bootres_cHR_summary$cHR_modelled, 2),
             " (", sprintf("%0.2f",round_half_up(bootres_cHR_summary$`chr_CI2.5%`, 2)), " - ",
             round_half_up(bootres_cHR_summary$`chr_CI97.5%`, 2),")")

mergeCells(workbook,"T4_model_results", cols = 7, rows = 5:8)
writeData(workbook, "T4_model_results", RR,
          startRow = 5, startCol = 7)
addStyle(workbook, "T4_model_results",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=6:7, rows=5:8, gridExpand = TRUE)

##right align cumulative risk
addStyle(workbook, "T4_model_results",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=4:5, rows=5:8, gridExpand = TRUE)

cuminc_header_trt <- paste0("Cumulative cancers diagnosed group treated with high dose folic acid (N = ",
                            scales::comma(n_lb$exposed_hdfa[1]), ") before the stated age")
cuminc_header_untrt <- paste0("Cumulative cancers diagnosed group not treated with high dose folic acid (N = ",
                              scales::comma(n_lb$unexposed_hdfa[1]), ") before the stated age")
#add table headers
mergeCells(workbook,"T4_model_results", cols = 2, rows = 3:4)
writeData(workbook, "T4_model_results", 
          cuminc_header_trt,
          startRow = 3, startCol = 2)
mergeCells(workbook,"T4_model_results", cols = 3, rows = 3:4)
writeData(workbook, "T4_model_results", cuminc_header_untrt,
          startRow = 3, startCol = 3)
writeData(workbook, "T4_model_results", "Treated with high dose folic acid",
          startRow = 4, startCol = 4)
writeData(workbook, "T4_model_results", "Not treated with high dose folic acid",
          startRow = 4, startCol = 5)
writeData(workbook, "T4_model_results", "Risk difference (cancers per 100,000 individuals)",
          startRow = 4, startCol = 6)

addStyle(workbook, "T4_model_results", 
         style = createStyle(fgFill = "white", wrapText = TRUE,halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =6:7, gridExpand = TRUE)

addStyle(workbook, "T4_model_results", 
         style = createStyle(fgFill = "white", wrapText = TRUE, halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =4, gridExpand = TRUE)

addStyle(workbook, "T4_model_results", 
         style = createStyle(fgFill = "white", halign =  "center", wrapText = TRUE, textDecoration = "bold"), 
         rows = 3,   cols =c(2:3, 6,7), gridExpand = TRUE)

writeData(workbook, "T4_model_results", "Abbreviations: CI, confidence intervals ",
          startRow = 10, startCol = 1)
writeData(workbook, "T4_model_results", "Note: as treatement effect was modelled as a single factor with no interaction with time, the risk ratio is the same across all time points.",
          startRow = 11, startCol = 1)
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "T4_model_results", 
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows =3,
  cols = 1:7,
  stack = TRUE,
  gridExpand = TRUE
)
## bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "T4_model_results", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = c(4,9),
  cols = 1:7,
  stack = TRUE,
  gridExpand = TRUE
)

## bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "T4_model_results", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = 3,
  cols = 4:5,
  stack = TRUE,
  gridExpand = TRUE
)

#####save file####
#openXL(workbook)
workbook$ActiveSheet<-as.integer(1)
saveWorkbook(workbook, paste0(folder_data_path, "outputs/Main_tables1_3.xlsx"), overwrite=TRUE)


