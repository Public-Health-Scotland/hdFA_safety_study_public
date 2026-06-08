#Supplementary tables####
####Tables for December meeting
###libraries and setup####
source("00.setup.r")
source("Figures_Tables/i.functions_for_excel.r")
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
  #df<-df_ind
  #x <- "ind_ntd"
  # varname<-"ind_ntd"
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
    mutate(`% of Total`= case_when(`% of Total` %in% c("  0.0"," 0.0","0.0" ) & Total!=0 ~ "<0.1",
                                   `% of Total` == "100.0" & Total!=total_cohort ~ ">99.9",
                                   T~as.character(`% of Total`))) %>%
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
####

###Create  workbook  ####
workbook <- createWorkbook()
# set default font####
modifyBaseFont(workbook, fontSize = 10, fontName = "Arial")
##footnotes need to be 8pt and modified individually

## set styles ###
bold.style <- createStyle(textDecoration = "Bold",fgFill = "white")
head.style <- createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white", border = "Bottom")
bottom.style <- createStyle(wrapText = TRUE,  border = "Bottom",fgFill = "white")
##bottom style for tte with merged cells - align text to top
bottom.style2 <- createStyle(wrapText = TRUE,  border = "Bottom",fgFill = "white", valign = "top")

centre.style <- createStyle(halign = "center", 
                            fgFill="white", border="top", textDecoration = "bold")
##Create contents page####
addWorksheet(workbook, "Contents")

##eTable0 :Definitions####
addWorksheet(workbook, "eTable_1")
addStyle(workbook, "eTable_1", style = createStyle(fgFill = "white", ),
         rows = 1:2, cols = 1:100, gridExpand = TRUE)
addStyle(workbook, "eTable_1", style = createStyle(fgFill = "white",wrapText = TRUE, valign = "top" ),
         rows = 3:200, cols = 1:100, gridExpand = TRUE)
writeData(workbook, "eTable_1", "eTable 1: Definitions and derivation of confounders included in analyses",
          startRow = 1, startCol = 1)
setColWidths(workbook, "eTable_1", cols = c(1:7), widths = c(30,45,27, 60,60))
addStyle(workbook, "eTable_1", bold.style, rows = c(1), cols = 1:7, gridExpand = TRUE)

Covariate <-c("Year of conception","Maternal age at conception (years)",
              "Maternal deprivation","Maternal ethnicity","Maternal obesity at antenatal booking",
              "Maternal smoking at antenatal booking","Family history of NTD",
              "Maternal pre-pregnancy diabetes","Maternal anti-seizure medicine use",
              "Maternal methotrexate use","Maternal coeliac disease",
              "Maternal sickle cell anaemia","Maternal thalassaemia major",
              "Maternal history of cancer","Maternal comorbidity")

Categories <-  c("2010\n2011\n2012\n2013\n2014\n2015\n2016\n2017\n2018\n2019\n2020\n2021\n2022",
                 "<20\n20-24\n25-29\n30-34\n35-39\n40+\nUnknown",
                 "SIMD quintile 1 (most deprived)\nSIMD quintile 2\nSIMD quintile 3\nSIMD quintile 4\nSIMD quintile 5 (least deprived)\nUnknown",
                 "African, Scottish African or British African
 Asian, Scottish Asian or British Asian
 Caribbean or Black\nMixed or multiple ethnic groups
 White\n  Other ethnic group\nUnknown/unclassified",
                 "Yes\nNo\nUnknown", "Current smoker\nFormer smoker\nNever smoked\nUnknown",
                 "Yes\nNo","Yes\nNo","Yes\nNo", "Yes\nNo","Yes\nNo","Yes\nNo",
                 "Yes\nNo","Yes\nNo","Yes\nNo")

Datasource <-c("SLiPBD", "SLiPBD", "SLiPBD","SLiPBD", "SLiPBD", "SLiPBD",
               "SMR01, SMR02, SLiCCD and CARDRISS", "SMR01 and SMR02", "PIS and HEPMA", "PIS and HEPMA", 
               "SMR01 and SMR02", "SMR01 and SMR02", "SMR01 and SMR02",
               "Scottish Cancer Registry (SMR06)", "SMR01 and SMR02")

Lookback <-c("year of conception of index pregnancy as recorded on SLiPBD", 
             "Maternal age at conception of index pregnancy as recorded on SLiPBD", 
             "Based on postcode at antenatal booking, or, if missing, end of pregnancy of index pregnancy as recorded on SLiPBD",
             "Maternal ethnicity as recorded on index record on SLiPBD",
             "BMI at antenatal booking for the index pregnancy, as recorded in SLiPBD ",
             "Smoking status as recorded on antenatal booking record of index pregnancy.",
             "Diagnostic codes included on general (SMR01) and maternity (SMR02) 
records for each woman with date of discharge from five years prior to the
estimated date of conception.\n
Look back of SLiCCD (using data between 2000-2020) and CARDRISS register
(using data between 2021-2023) records to ascertain any previous pregnancy
to the same woman where the baby was reported to have an NTD.",
             "Diagnostic codes included on general (SMR01) and maternity (SMR02) records for
each woman with date of discharge from five years prior to the
estimated date of conception.\n
SMR02 delivery record for the index delivery  to identify maternal pre-pregnancy diabetes",
             "≥1 prescription dispensed ≤12 weeks prior to the estimated date of conception
and ≥1 prescription dispensed between the estimated date of conception and 11+6 weeks gestation",
             "≥1 prescription dispensed ≤12 weeks prior to the estimated date of 
conception and ≥1 prescription dispensed between the estimated date of
conception and 11+6 weeks gestation",
             "Diagnostic codes included on general (SMR01) and maternity (SMR02) records
for each woman with date of discharge from five years prior to the estimated date of conception",
             "diagnostic codes included on general (SMR01) and maternity (SMR02)
records for each woman with date of discharge from five years prior to 
the estimated date of conception.\nSMR02 delivery record for the index delivery 
to identify maternal pre-pregnancy disease",
             "diagnostic codes included on general (SMR01) and maternity (SMR02) records
for each woman with date of discharge from five years prior to the estimated date of conception.\n 
SMR02 delivery record for the index delivery  to identify maternal pre-pregnancy disease",
             "Maternal history of cancer will be defined as any malignancy diagnosed prior to the estimated date of conception\u1d43",
             "Diagnostic codes included on general (SMR01) and maternity (SMR02) records for
each woman with date of discharge from five years prior to the estimated date of conception")

DefinitionDiagnosticcodes <-
  c("NA","NA", "NA", "NA", "BMI ≥30 kg/m2", "NA", "ICD10 codes: Q00-Q01, Q05",
    "ICD10 codes:E10 - E14; O24.0-O24.3\nDiabetes-specific hard coded 
  variable on SMR02 delivery records (Diabetes =1: pre-existing diabetes)",
    "BNF section 4.8.1: Control of epilepsy\nBNF section 4.8.2: Drugs used in status epilepticus\n
VTM names: Sodium valproate; Valproic acid", 
    "BNF code 1001030U0", "ICD10 codes: K90.0", "
ICD10 codes: D57 (exclude D57.3)", "ICD10 codes: D56.1",
    "ICD10 codes: C00-C96 excluding C44 (C97 is not used by the Scottish Cancer Registry))",
    "see eTable 2 for details of included conditions and ICD10 codes")

tab <- as.data.frame(cbind(Covariate, Categories, Datasource,
                           Lookback, DefinitionDiagnosticcodes))
names(tab) <- c("Covariate" , "Categories"  , "Data Source", "Lookback"  ,                
                "Definition/ Diagnostic Codes")

writeData(workbook, "eTable_1",tab ,
          headerStyle = createStyle(wrapText = TRUE,textDecoration = "bold", fgFill = "white"),
          startRow = 3, startCol = 1)

addStyle(workbook, "eTable_1",
         createStyle(halign="left", valign = "top", fgFill="white", wrapText = TRUE),
         rows = 4:18, cols = c(1, 3,4,5), gridExpand = TRUE)

##bottom borders
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_1",
  style = openxlsx::createStyle(border = c("bottom"),
                                borderStyle = c("thin") ),
  rows = c(3:18),
  cols = c(1:5),
  stack = TRUE,
  gridExpand = TRUE
)
writeData(workbook, "eTable_1",
          "See Appendices 3-5 of the protocol for additional information on how maternal conditions, comorbidites and cancer history were derived",
          startRow = 20, startCol = 1)
addStyle(workbook, "eTable_1",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 20, cols = 1, gridExpand = TRUE)

##
##eTable02 :Comorbidty ICD10####
addWorksheet(workbook, "eTable_2")
addStyle(workbook, "eTable_2", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)
setColWidths(workbook, "eTable_2", cols = c(1:7), widths = c(55,55))
addStyle(workbook, "eTable_2", bold.style, rows = c(1), cols = 1:7, gridExpand = TRUE)

Condition <- c("Haematological diseases", "Thyroid disorders", 
               "Demyelinating and neuromuscular diseases",
               "Hypertension","Venous thromboembolism","Asthma","Gastrointestinal diseases",
               "Liver diseases","Skin disorders",
               "Immune mediated joint and connective tissue disorders","Kidney diseases",
               "Disorders of female pelvic organs/genital tract")

ICD10codes <- c(   "D50-D89, O99.0-O99.1, excluding D57.0-57.2, D57.8 and D56.1",
                   "E00-E07, O99.2",  "G35-G37, G70-G73", "I10-I15, O10",
                   "I26, I80-I82", "J45-J46, O99.5", "K25-K28, K50-K52, K80-K86, O99.6",
                   "K71-K76","L10-L45, O99.7", "M05-M06, M08, M30-M35",
                   "N00-N07, N10-N15, N17-N19", "N70-N77, N80-N94, N96-N98" )

tab <- as.data.frame(cbind(Condition, ICD10codes))
names(tab) <- c("Condition", "ICD10 codes")
###table titl.
writeData(workbook, "eTable_2", 
          "eTable 2: Derivation of confounder indicating maternal comorbidity (ICD-10)",
          startRow = 1, startCol = 1)
writeData(workbook, "eTable_2",tab ,
          headerStyle = createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"),
          startRow = 4, startCol = 1)
##bottom borders
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_2",
  style = openxlsx::createStyle(border = c("bottom"),
                                borderStyle = c("thin") ),
  rows = c(4,16),
  cols = c(1:2),
  stack = TRUE,
  gridExpand = TRUE
)
##eTable3 :Timing of exposure####
addWorksheet(workbook, "eTable_3")
addStyle(workbook, "eTable_3", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)
##run file that sets up the data for table 2
source("Figures_tables/iii.Data_clean_for_tables.r")
source("Figures_tables/ii.Exposures_timing.r")

writeData(workbook, "eTable_3", "eTable 3: Additional information on the high dose folic acid received by the treated group",
          startRow = 1, startCol = 1)
mergeCells(workbook, "eTable_3", rows = 3, cols=2:5)
writeData(workbook, "eTable_3"," Number of singleton live births dispensed the stated number of prescriptions during the exposure period\u1d47",
          startRow = 3, startCol = 2)
addStyle(workbook, "eTable_3", 
         style = createStyle(fgFill = "white", wrapText=TRUE), rows = 1:200, cols = 1:7, gridExpand = TRUE)

#
writeData(workbook, "eTable_3","Total quantity of hdFA\u1d9c tablets dispensed during the exposure period\u1d48",
          startRow = 3, startCol = 7)
writeData(workbook, "eTable_3","N",
          startRow = 4, startCol = 2)
writeData(workbook, "eTable_3","%",
          startRow = 4, startCol = 3)


writeData(workbook, "eTable_3",tab_n_prescrib ,
          headerStyle = createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"),
          startRow = 4, startCol = 1)
##headers in merged cells
mergeCells(workbook, "eTable_3", cols = 1, rows = 3:4)
writeData(workbook, "eTable_3", names(tab_n_prescrib)[1],
          startRow = 3, startCol = 1)
mergeCells(workbook, "eTable_3", cols = 6, rows = 3:4)
writeData(workbook, "eTable_3", names(tab_n_prescrib)[6],
          startRow = 3, startCol = 6)

addStyle(workbook, "eTable_3", centre.style, rows = 3, cols = 2:7, gridExpand = TRUE)
addStyle(workbook, "eTable_3", bold.style, rows = c(1), cols = 1:7, gridExpand = TRUE)
addStyle(workbook, "eTable_3", head.style, rows = c(3:4), cols = 1:7, gridExpand = TRUE)


addStyle(workbook, "eTable_3",
         createStyle(halign="right", fgFill="white", textDecoration = "bold", wrapText = TRUE),
         rows = 4, cols = c(2:5,7), gridExpand = TRUE)


setColWidths(workbook, "eTable_3", cols = c(1:7), widths = c(52,27,27, 27, 27, 27, 27))
setRowHeights(workbook, "eTable_3", rows = 3, heights = 42)

##add title and footnotes
writeData(workbook, "eTable_3", "\u1d43 The estimated date of conception is set to 2+0 gestation",
          startRow = 11, startCol = 1)
#footnote b
writeData(workbook, "eTable_3","\u1d47 From 12 weeks before the estimated date of conception (-10+0 gestation) to the end of pregnancy inclusive",
          startRow = 12, startCol = 1)
##footnotec
writeData(workbook, "eTable_3","\u1d9c hdFA is high dose folic acid",
          startRow = 13, startCol = 1)
##footnote d
writeData(workbook, "eTable_3","\u1d48 To singleton live births with known total quantity of hdFA tablets dispensed during the exposure period",
          startRow = 14, startCol = 1)
#footnote e
writeData(workbook, "eTable_3","\u1d49 IQR is interquartile range",
          startRow = 15, startCol = 1)
#style footnotes (size)
addStyle(workbook, "eTable_3",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 11:14, cols = 1, gridExpand = TRUE)

##set 1000s format.
addStyle(workbook,  "eTable_3", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "COMMA", ), rows = 5:9, cols = 2:7, gridExpand = TRUE)

rangeRows = 3:9
rangeCols = 1:7
## left borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_3",
  style = openxlsx::createStyle(
    border = c("left"),
    borderStyle = c("thin")
  ),
  rows = rangeRows,
  cols = c(1,2,6,7,8),
  stack = TRUE,
  gridExpand = TRUE
)


## top borders
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_3",
  style = openxlsx::createStyle(  border = c("top"),
                                  borderStyle = c("thin") ),
  rows = rangeRows[1],
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
##bottom borders
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_3",
  style = openxlsx::createStyle(border = c("bottom"),
                                borderStyle = c("thin") ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

##border under header
rangeRows = 3:4
rangeCols = 1:7

openxlsx::addStyle(
  wb = workbook, sheet = "eTable_3",
  style = openxlsx::createStyle(    border = c("bottom"),
                                    borderStyle = c("thin")  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
##right borders internal
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_3",
  style = openxlsx::createStyle(  border = c("right"),
                                  borderStyle = c("thin") ),
  rows = 3:9,
  cols = 1,
  stack = TRUE,
  gridExpand = TRUE
)
##right borders internal
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_3",
  style = openxlsx::createStyle(  border = c("right"),
                                  borderStyle = c("thin") ),
  rows = 3:9,
  cols = 5,
  stack = TRUE,
  gridExpand = TRUE
)
openxlsx::addStyle(
  wb = workbook,  sheet = "eTable_3",
  style = openxlsx::createStyle(  border = c("right"),
                                  borderStyle = c("thin") ),
  rows = 3:9,
  cols = 6,
  stack = TRUE,
  gridExpand = TRUE
)


###Additional characteristics: ####
addWorksheet(workbook, "eTable_4")
addStyle(workbook, "eTable_4", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:7, gridExpand = TRUE)
addStyle(workbook, "eTable_4",
         style = createStyle(fgFill = "white", numFmt = "COMMA"),
         rows = 5:200, cols = c(2,4,6), gridExpand = TRUE)

source("Figures_tables/iv.additional_char_data_supp_format.r")

writeData(workbook, "eTable_4", "eTable 4: Additional characteristics of included singleton live births, stratified by treatment group",
          startRow = 1, startCol = 1, 
          headerStyle = head.style)

#mergeCells(workbook, "eTable_4", cols = 2:3, rows = 3)
writeData(workbook, "eTable_4","All singleton live births", startRow = 3, startCol = 2)
#mergeCells(workbook, "eTable_4", cols = 4:5, rows = 3)
writeData(workbook, "eTable_4","Treated with high dose folic acid", startRow = 3, startCol = 3)
#mergeCells(workbook, "eTable_4", cols = 6:7, rows = 3)
writeData(workbook, "eTable_4","Not treated with high dose folic acid", startRow = 3, startCol = 4)
addStyle(workbook,  "eTable_4", bold.style, rows = c(1,3,4), cols = 1:7, gridExpand = TRUE)


writeData(workbook, "eTable_4", table_addchar, startRow = 4, startCol = 1,
          headerStyle = head.style)


for (row in 1:nrow( table_addchar)){
  sheetRow <- data.frame(lapply( table_addchar[row,],
                                 function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook, "eTable_4", x = sheetRow, startRow = row+3, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook,  "eTable_4", x = sheetRow, startRow = row+4, colNames = FALSE)
  }
}
##right align
addStyle(workbook, "eTable_4", 
         createStyle(halign="right", fgFill="white", 
                     numFmt = "0.0", ), rows = 5:100, cols = c(3,5,7), gridExpand = TRUE)
##sot 1000s format.
addStyle(workbook,  "eTable_4", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "COMMA" ), rows = 5:100, cols = c(2,4,6), gridExpand = TRUE)


writeData(workbook, "eTable_4",
          "Major congenital condition includes any major structural or chromosomal condition as defined by the EUROCAT network of European congenital condition registers",
          startRow = 31, startCol = 1)

writeData(workbook, "eTable_4",
          "\u1d43 EDC is estimated date of conception",
          startRow = 32, startCol = 1)


setColWidths(workbook, "eTable_4", cols = c(1:7), widths = c(52,34,34,34))
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows =1:30, 
                      type = "contains", rule = "Maternal", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30, 
                      type = "contains", rule = "Total", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30,
                      type = "contains", rule = "Due to", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30, 
                      type = "contains", rule = "exposure", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows =1:30,
                      type = "contains", rule = "study end", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30,
                      type = "contains", rule = "birth", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows =1:30,
                      type = "contains", rule = "Sex", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30, 
                      type = "contains", rule = "Major", style = bold.style)
conditionalFormatting(workbook,  "eTable_4", cols = 1, rows = 1:30, 
                      type = "contains", rule = "Mediator", style = bold.style)
addStyle(workbook, "eTable_4",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 31, cols = 1, gridExpand = TRUE)

addStyle(workbook,  "eTable_4", bold.style, rows = 1, cols = 1:4)
addStyle(workbook, sheet = "eTable_4", centre.style, rows = 3, cols = 2:4, gridExpand = TRUE)
addStyle(workbook, "eTable_4", createStyle(halign="right", border = "Bottom", textDecoration = "bold", fgFill="white"),
         rows = 4, cols = 2:4, gridExpand = TRUE)

#add borders
rangeRows = 3
rangeCols = 1:4
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_4", 
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows = rangeRows[1],
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

## bottom header border
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_4", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = 4,
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
# left internal borders & right border
rangeRows = 3:29

##bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_4",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

###congenital conditions tab####
addWorksheet(workbook,"eTable_5")
addStyle(workbook,"eTable_5", 
         style = createStyle(fgFill = "white",), 
         rows = 1:200, cols = 1:7, gridExpand = TRUE)

writeData(workbook,"eTable_5", 
          "eTable 5: Additional information on major congenital conditions seen in the treated and untreated group",
          startRow = 1, startCol = 1)
addStyle(workbook,"eTable_5", 
         style = createStyle(fgFill = "white",textDecoration = "bold"), 
         rows = 1, cols = 1, gridExpand = TRUE)

setRowHeights(workbook,"eTable_5",rows = c(5:18), 
              heights = c(75,30,30,45,75, 30, 45, 60, 30, 60,
                          30, 120, 45, 75))

source("v.CC_data_processing.r")
names(t_ccs) <- c("Eurocat congenital condition group", "ICD10-BPA codes" ,                  
                  "Exclusions" ,  "N_trt"  , "%_trt" , "N_untrt" , "%_untrt" )

t_ccs <- t_ccs %>% 
  mutate( `%_trt` = sprintf("%0.2f", `%_trt` ), 
          `%_untrt` = sprintf("%0.2f", `%_untrt` ) ) %>%
  mutate( `%_trt` = case_when( as.character(`%_trt`)=="0" & N_trt!=0 ~"<0.01", T~as.character(`%_trt`) )) %>%
  mutate(treated = paste0(N_trt," (", `%_trt`, ")"), 
         untreated = paste0(N_untrt," (" ,`%_untrt`, ")")) %>% 
  select(-c(N_trt,  `%_trt` , N_untrt , `%_untrt`)) 

names(t_ccs) <- c("Eurocat congenital condition group", "ICD10-BPA codes" ,                  
                  "Exclusions" ,  "N (%)"  ,"N (%)" )

t_ccs$`ICD10-BPA codes`[t_ccs$`Eurocat congenital condition group`=="Other conditions"] <-
  "All codes in 'any major condition' not covered by specific condition groups"
#mergeCells(workbook,"eTable_5", rows = 3, cols=4:5)
writeData(workbook, "eTable_5","Singleton live births treated with high dose folic acid",
          headerStyle = createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"),
          startRow = 3, startCol = 4)
#mergeCells(workbook,"eTable_5", rows = 3, cols=6:7)
writeData(workbook, "eTable_5",
          "Singleton live births not treated with high dose folic acid",
          headerStyle = createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"),
          startRow = 3, startCol = 5)
writeData(workbook,"eTable_5", t_ccs,
          headerStyle = createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"),
          startRow = 4, startCol = 1)

writeData(workbook, "eTable_5",
          "Percentages in this table are percentages of singleton live births in that treatment group",
          headerStyle = createStyle(fgFill = "white"),
          startRow = 20, startCol = 1)
writeData(workbook, "eTable_5",
          "An individual may have more than one congenital condition, so the sum of numbers from each Eurocat group will not match the total number with any condition",
          headerStyle = createStyle(fgFill = "white"),
          startRow = 21, startCol = 1)
addStyle(workbook, "eTable_5",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 20:21, cols = 1, gridExpand = TRUE)
##right alighn cols
addStyle(workbook,"eTable_5", 
         createStyle(halign="right",fgFill="white",  textDecoration = "Bold",
         ), rows = 4, cols = c(4,5), gridExpand = TRUE)
addStyle(workbook,"eTable_5", 
         createStyle(halign="right", valign="top",fgFill="white", 
         ), rows = 5:18, cols = c(4,5), gridExpand = TRUE)
#bold# heads
addStyle(workbook,"eTable_5", 
         createStyle(wrapText = TRUE, fgFill="white",halign="center", 
                     textDecoration = "Bold",), rows = 3, cols = 4:5, gridExpand = TRUE)
##wrap text in cols 2 and 3
addStyle(workbook,"eTable_5", 
         createStyle(wrapText = TRUE, fgFill="white",halign="left", valign = "top"),
         rows = 5:18, cols = c(1:3), gridExpand = TRUE)

setColWidths(workbook,"eTable_5", cols = c(1:7),
             widths = c(34,60,60,35,35))

writeFormula(
  workbook, "eTable_5",
  x = '=HYPERLINK(\"#Contents!A3\",
  "Back to contents")',
  startCol = 8, startRow = 1
)

rangeRows = 3:18
rangeCols = 1:5
## left borders
openxlsx::addStyle(
  wb = workbook,
  sheet ="eTable_5",
  style = openxlsx::createStyle(  border = c("left"),
                                  borderStyle = c("thin") ),
  rows = rangeRows,
  cols = rangeCols[1],
  stack = TRUE,
  gridExpand = TRUE
)

##right borders
openxlsx::addStyle(
  wb = workbook,
  sheet ="eTable_5",
  style = openxlsx::createStyle(  border = c("right"),
                                  borderStyle = c("thin") ),
  rows = rangeRows,
  cols = tail(rangeCols, 1),
  stack = TRUE,
  gridExpand = TRUE
)
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet ="eTable_5",
  style = openxlsx::createStyle(  border = c("top"),
                                  borderStyle = c("thin") ),
  rows = rangeRows[1],
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
##bottom borders
openxlsx::addStyle(
  wb = workbook,  sheet ="eTable_5",
  style = openxlsx::createStyle(  border = c("bottom"),
                                  borderStyle = c("thin")  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
##bottom border for header
openxlsx::addStyle(
  wb = workbook,  sheet ="eTable_5",
  style = openxlsx::createStyle(  border = c("bottom"), 
                                  borderStyle = c("thin") ),
  rows = 4,
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)
###add borders for columns

## left borders
openxlsx::addStyle(
  wb = workbook,  sheet ="eTable_5",
  style = openxlsx::createStyle(    border = c("left"),
                                    borderStyle = c("thin")  ),
  rows = rangeRows,
  cols = c(2,3,4,5,6),
  stack = TRUE,
  gridExpand = TRUE
)


##eTable 6 Outcomes (cancer type) tab ####
## Cancer subgroups####
addWorksheet(workbook, "eTable_6")
addStyle(workbook, "eTable_6", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)

writeFormula(
  workbook, "eTable_6",
  x = '=HYPERLINK(\"#Contents!A3\",
  "Back to contents")',
  startCol = 5, startRow = 1
)
writeData(workbook, "eTable_6", "eTable 6: Childhood cancer (incidence rate) in included singleton live births, by treatment group",
          startRow = 1, startCol = 1)
source("Figures_Tables/vi.outcomes_table.r")

addStyle(workbook, "eTable_6",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

for (row in 1:nrow(overall_rates)){
  sheetRow <- data.frame(lapply(overall_rates[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook, "eTable_6", x = sheetRow, startRow = row+2, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook,  "eTable_6", x = sheetRow, startRow = row+3, colNames = FALSE)
  }
}
##1000s format
for (row in 1:nrow(stats_0_1)){
  sheetRow <- data.frame(lapply(stats_0_1[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  # if (row == 1) {
  ##    writeData(workbook, "eTable_6", x = sheetRow, startRow = row+9, colNames = TRUE,  headerStyle = head.style)
  #  } else {
  writeData(workbook,  "eTable_6", x = sheetRow, startRow = row+10, colNames = FALSE)
  # }
}

writeData(workbook,  "eTable_6","Cancer risk by age group",
          startRow = 10, startCol = 1)

for (row in 1:nrow(stats_1_4)){
  sheetRow <- data.frame(lapply(stats_1_4[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_6", x = sheetRow, startRow = row+16, colNames = FALSE)
}


for (row in 1:nrow(stats_5_9)){
  sheetRow <- data.frame(lapply(stats_5_9[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_6", x = sheetRow, startRow = row+22, colNames = FALSE)
}


for (row in 1:nrow(stats_10_14)){
  sheetRow <- data.frame(lapply(stats_10_14[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_6", x = sheetRow, startRow = row+28, colNames = FALSE)
  
}
writeData(workbook,  "eTable_6","",
          startRow = 3, startCol = 1, headerStyle = head.style)

addStyle(workbook, "eTable_6", 
         createStyle(wrapText = TRUE, halign="center",fgFill="white",textDecoration = "bold", border="bottom"),
         cols=c(2,3), rows=3)

addStyle(workbook, "eTable_6", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "COMMA" ), rows = 4:34, cols = c(2,3), gridExpand = TRUE)

writeData(workbook,  "eTable_6",
          "Abbreviations: PYAR, person years at risk; IQR, interquartile range; CI, confidence intervals ",
          startRow = 36, startCol = 1)

addStyle(workbook, "eTable_6",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 36, cols = 1, gridExpand = TRUE)

setColWidths(workbook,"eTable_6", cols = c(1:7), widths = c(68,20,20))
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_6",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = c(2,34),
  cols = 1:3,
  stack = TRUE,
  gridExpand = TRUE
)


## outcomes###
##etable7 ####
addWorksheet(workbook, "eTable_7")
addStyle(workbook, "eTable_7", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)
setColWidths(workbook,"eTable_7", cols = c(1:7), widths = c(68,30,30))
##white background fill

writeData(workbook,   "eTable_7", 
          "eTable 7: Additional information on the childhood cancers seen in the treated and untreated group",
          startRow = 1, startCol = 1)
addStyle(workbook, "eTable_7", 
         style = createStyle(fgFill = "white", wrapText=TRUE),
         rows = 1:200, cols = 1:7, gridExpand = TRUE)

##cancer groups####
can_grp <- df %>% filter(cancer_outcome==1) %>%
  mutate(cancer_group = str_pad(cancer_group,2,side = "left", pad = "0")) %>%
  group_by(hdfa_preg, cancer_group, cancer_group_desc) %>% count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n) %>%
  mutate(exposed_hdfa = replace_na(exposed_hdfa,0)) %>% arrange(cancer_group) %>%
  ungroup() %>%
  mutate(total_exp_can = sum(exposed_hdfa), total_unexp_can = sum(unexposed_hdfa)) %>% 
  mutate(perc_exp_cancers = str_trim(format(round_half_up(exposed_hdfa/total_exp_can*100, 1))), 
         perc_unexp_cancers = str_trim(format(round_half_up(unexposed_hdfa/total_unexp_can *100, 1)))) %>%
  select(cancer_group_desc,exposed_hdfa, perc_exp_cancers, unexposed_hdfa, perc_unexp_cancers) %>%
  mutate(exposed = paste0(exposed_hdfa, " (", perc_exp_cancers , ")"), 
         unexposed =paste0(unexposed_hdfa, " (", perc_unexp_cancers , ")") ) %>%
  select(cancer_group_desc,exposed, unexposed)
names(can_grp) <- c("Type of childhood cancer", "N (%)", "N (%)")

writeData(workbook,  "eTable_7","Singleton live births treated with high dose folic acid",
          startRow = 3, startCol = 2, headerStyle =createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"))
writeData(workbook,  "eTable_7","Singleton live births not treated with high dose folic acid",
          startRow = 3, startCol = 3, headerStyle =createStyle(wrapText = TRUE, textDecoration = "Bold",fgFill = "white"))
addStyle(workbook, "eTable_7", centre.style, rows = 3, cols = 2:3, gridExpand = TRUE)
addStyle(workbook, "eTable_7", bold.style, rows = c(1), cols = 1:4, gridExpand = TRUE)
#addStyle(workbook, "eTable_7", head.style, rows = 4, cols = 1:3, gridExpand = TRUE)
addStyle(workbook, "eTable_7",
         createStyle(wrapText = TRUE, halign="left",fgFill="white",
                     textDecoration = "bold"),
         cols=1, rows=3:4, gridExpand=TRUE)
addStyle(workbook, "eTable_7",
         createStyle(wrapText = TRUE, halign="center",fgFill="white",
                     textDecoration = "bold"),
         cols=2:3, rows=3, gridExpand=TRUE)
addStyle(workbook, "eTable_7",
         createStyle(wrapText = TRUE, textDecoration = "bold",halign="right",fgFill="white", border="bottom"),
         cols=2:3, rows=4, gridExpand=TRUE)
addStyle(workbook, "eTable_7",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=2:3, rows=5:16, gridExpand=TRUE)

writeData(workbook,  "eTable_7",can_grp,
          startRow = 4, startCol = 1, headerStyle = head.style)

setColWidths(workbook,"eTable_6", cols = c(1:3), widths = c(60,30,30))


openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_7",
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows = 3,
  cols = 1:3,
  stack = TRUE,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_7",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = c(4,16),
  cols = 1:3,
  stack = TRUE,
  gridExpand = TRUE
)
## Sensitivity any indicator baseline characteristics####
#eTable 8 sheet ####
addWorksheet(workbook, "eTable_8")
##add white background to everything
addStyle(workbook, "eTable_8", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)

##run file that sets up the data for table 
source("Figures_tables/vii.sensitivity_1_tables_data_processing.r")

###Formatting table 

writeData(workbook,"eTable_8","All singleton live births", startRow = 3, startCol = 2)
writeData(workbook, "eTable_8","Treated with high dose folic acid", startRow = 3, startCol = 3)
writeData(workbook, "eTable_8","Not treated with high dose folic acid", startRow = 3, startCol = 4)

addStyle(workbook, sheet ="eTable_8", centre.style,
         rows = 3, cols = 2:4, gridExpand = TRUE)
addStyle(workbook, sheet ="eTable_8", style = createStyle(fgFill = "white", indent=2), 
         rows= c(9:21), cols=1)

##add bold styling 
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "Maternal", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "year of", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "sex", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "indicat", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "total", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "cancer", style = bold.style)
conditionalFormatting(workbook,"eTable_8", cols = 1, rows = 1:200, 
                      type = "contains", rule = "birthweight", style = bold.style)
conditionalFormatting(workbook, "eTable_8", cols = 1, rows = 1:200, 
                      type = "contains",rule = "family", style = bold.style)


writeData(workbook, "eTable_8",
          "eTable 8: Baseline characteristics of included singleton live births, by treatment group. Sensitivity analysis limited to children of women with any indication for hdFA",
          startCol = 1, startRow = 1)

writeData(workbook, "eTable_8",
          "\u1d43 SIMD = Scottish Index of Multiple Deprivation",
          startRow = 96, startCol = 1)
writeData(workbook, "eTable_8",
          "\u1d47 hdFA = high dose folic acid",
          startRow = 97, startCol = 1)
writeData(workbook, "eTable_8",
          "\u1d9c NTD =  neural tube defect",
          startRow = 98, startCol = 1)
writeData(workbook, "eTable_8",
          "\u1d48 ASM = antiseizure medication",
          startRow = 99, startCol = 1)

addStyle(workbook, "eTable_8",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 96:99, cols = 1, gridExpand = TRUE)
##make caption bold
addStyle(workbook, "eTable_8", bold.style, rows = c(1,4), cols = 1:7, gridExpand = TRUE)

for (row in 1:nrow(t2_baseline)){
  sheetRow <- data.frame(lapply(t2_baseline[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook,"eTable_8", x = sheetRow, startRow = row+3, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook, "eTable_8", x = sheetRow, startRow = row+4, colNames = FALSE)
  }
}

writeData(workbook, "eTable_8","Indications for hdFA", startRow = 48, startCol = 1)
setColWidths(workbook, "eTable_8", cols = c(1:7), widths = c(65,32,32, 32))

addStyle(workbook, "eTable_8", 
         createStyle(halign="right",fgFill="white" ), rows = 5:100, cols = c(2,3,4), gridExpand = TRUE)
addStyle(workbook, "eTable_8", 
         createStyle(halign="right",fgFill="white", textDecoration = "bold" ),
         rows = 4, cols = c(2,3,4), gridExpand = TRUE)
addStyle(workbook,"eTable_8", 
         style = createStyle(halign="right", fgFill = "white",  border = "bottom"), 
         rows = nrow(t2_baseline)+4, cols = c(2,3,4), gridExpand = TRUE)


##left align fist column (otherwise years are right aligned.)
addStyle(workbook, "eTable_8", 
         createStyle(halign="left",fgFill="white", 
         ), rows = 1:nrow(t2_baseline)+4, cols = 1, gridExpand = TRUE)

##italicise indicators
addStyle(workbook, "eTable_8", 
         createStyle(halign="left",fgFill="white", textDecoration = "italic",
         ), rows = 49:80, cols = 1, gridExpand = TRUE)
#italicise indicators and bold headers
addStyle(workbook, "eTable_8", 
         createStyle(halign="left",fgFill="white", textDecoration = c("italic", "bold"),
         ), rows = c(49,53,57,62,66,70,74,78), cols = 1, gridExpand = TRUE)

rangeRows = 3
rangeCols = 1:4
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_8", 
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
rangeRows = 3:94

##bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_8", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

## Sensitivity any indicator outcome rates ####

addWorksheet(workbook, "eTable_9")

addStyle(workbook, "eTable_9", style = createStyle(fgFill = "white"), rows = 1:200, cols = 1:100, gridExpand = TRUE)
addStyle(workbook, "eTable_9",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

writeData(workbook, "eTable_9", 
          "eTable 9: Childhood cancer (incidence rate) in included singleton live births, by treatment group. Sensitivity analysis limited to children of women with any indication for hdFA",
          startRow = 1, startCol = 1)

source("Figures_tables/viii.Sensitivity_outcomes.r")

for (row in 1:nrow(overall_rates)){
  sheetRow <- data.frame(lapply(overall_rates[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook, "eTable_9", x = sheetRow, startRow = row+2, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook, "eTable_9", x = sheetRow, startRow = row+3, colNames = FALSE)
  }
}
##1000s format
for (row in 1:nrow(stats_0_1)){
  sheetRow <- data.frame(lapply(stats_0_1[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  # if (row == 1) {
  ##    writeData(workbook, "eTable_7", x = sheetRow, startRow = row+9, colNames = TRUE,  headerStyle = head.style)
  #  } else {
  writeData(workbook,  "eTable_9", x = sheetRow, startRow = row+10, colNames = FALSE)
  # }
}

writeData(workbook,  "eTable_9","Cancer risk by age group",
          startRow = 10, startCol = 1)

for (row in 1:nrow(stats_1_4)){
  sheetRow <- data.frame(lapply(stats_1_4[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_9", x = sheetRow, startRow = row+16, colNames = FALSE)
  
}

for (row in 1:nrow(stats_5_9)){
  sheetRow <- data.frame(lapply(stats_5_9[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_9", x = sheetRow, startRow = row+22, colNames = FALSE)
  
}

for (row in 1:nrow(stats_10_14)){
  sheetRow <- data.frame(lapply(stats_10_14[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_9", x = sheetRow, startRow = row+28, colNames = FALSE)
  
}
writeData(workbook, "eTable_9","",
          startRow = 3, startCol = 1, headerStyle = head.style)

addStyle(workbook,"eTable_9", 
         createStyle(wrapText = TRUE, halign="center",fgFill="white",textDecoration = "bold", border="bottom"),
         cols=c(2,3), rows=3)

addStyle(workbook, "eTable_9", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "COMMA" ), rows = 4:34, cols = c(2,3), gridExpand = TRUE)


writeData(workbook,   "eTable_9",
          "Abbreviations: PYAR, person years at risk; IQR, interquartile range; CI, confidence intervals ",
          startRow = 36, startCol = 1)
addStyle(workbook, "eTable_9",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 36, cols = 1, gridExpand = TRUE)

setColWidths(workbook,"eTable_9", cols = c(1:7), widths = c(68,20,20))
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_9",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = c(2,34),
  cols = 1:3,
  stack = TRUE,
  gridExpand = TRUE
)

## eTable10 Sensitivity any indicator model results ####
addWorksheet(workbook, "eTable_10")
quibble <- function(x, q = c(0.025,0.5, 0.975), dropNA = TRUE) {
  tibble(x = quantile(x, q, na.rm = dropNA), q = q)
}
addStyle(workbook,  "eTable_10", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)
addStyle(workbook,  "eTable_10",  style = bold.style, 
         rows = 1, cols = 1, gridExpand = TRUE)
setRowHeights(workbook,  "eTable_10",rows=3, heights=27)

writeData(workbook,  "eTable_10",
          "eTable 10: Cumulative risk and risk difference of childhood cancer by specified age points. Relative risk of childhood cancer in those treated with hdFA, compared to not treated.  Sensitivity analysis limited to children of women with any indication for hdFA.",
          startRow = 1, startCol = 1)

setColWidths(workbook, "eTable_10", cols = c(1:7), widths = c(32,32,32,32, 32, 32, 32))
addStyle(workbook,   "eTable_10",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

##Singlton LBs
n_lb <- df_ind %>% group_by(hdfa_preg) %>% count() %>% ungroup() 
n_lb <- n_lb %>% pivot_wider(names_from = hdfa_preg, values_from = n) %>% mutate(col1 = "N live births")

df_ind <- df_ind %>%  mutate(age_yr_at_diag = interval(date_end_pregnancy, incidence_date) /years(1)) %>% 
  mutate(incid_0_1 = case_when(cancer_outcome==1 & age_yr_at_diag <1 ~1, T~0), 
         incid_1_4 = case_when(cancer_outcome==1 & age_yr_at_diag>=1 & cancer_outcome==1 & age_yr_at_diag <5 ~1, T~0), 
         incid_5_9 = case_when(cancer_outcome==1 & age_yr_at_diag >=5 & age_yr_at_diag <10 ~1, T~0), 
         incid_10_14 = case_when(cancer_outcome==1 & age_yr_at_diag >=10 & age_yr_at_diag <15 ~1, T~0) ) 
##cuminc 
n_event_1 <- df_ind  %>% filter(incid_0_1==1) %>% group_by(hdfa_preg) %>% 
  count() %>% mutate(age = "1 year")
n_event_5 <- df_ind  %>% filter(incid_0_1==1|incid_1_4==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "5 year")
n_event_10 <- df_ind %>% filter(incid_0_1==1|incid_1_4==1|incid_5_9==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "10 year")
n_event_end <- df_ind  %>% filter(incid_0_1==1|incid_1_4==1 | incid_5_9==1|incid_10_14==1 )%>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "End of follow up")

events <- rbind(n_event_1, n_event_5, n_event_10, n_event_end)
events <- events %>% pivot_wider(names_from = hdfa_preg, values_from = n) 
names(events) <- c("Age", "exposed_events", "unexposed_events")


##cumulative risks
results <- readRDS(paste0(folder_data_path, "outputs/sens_any_predictions_fitwts_l.rds"))
results_unwt <- readRDS(paste0(folder_data_path, "outputs/sesn_any_predictions_unwt.rds"))

##wide res
wideres <- dcast(results, time_interval ~ randf, value.var = 'mean_survival')

#head(wideres)

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
bootres <- readRDS( paste0(folder_data_path, "outputs/sens_any_bootres1_500.rds"))

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
  mutate(cumrisk = paste0( 
    sprintf("%0.1f", round_half_up(`CumInc 50%`*100000,1)), " (", 
    sprintf("%0.1f" , round_half_up(`CumInc CI2.5%`*100000,1))," - " ,
    sprintf("%0.1f" ,round_half_up(`CumInc CI97.5%`*100000,1)), ")" )) %>%
  select(time_interval, randf, cumrisk) %>%
  pivot_wider(names_from = randf, values_from = c(cumrisk)) %>%
  mutate(time_interval = paste0(time_interval, " year")) %>%
  mutate(time_interval = case_when(time_interval== "13 year" ~ "End of follow up", 
                                   T~time_interval))%>%
  rename(Age = time_interval)

##risk diffeence bootstrappedsummary
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
  mutate(RD =  paste0( #round_half_up(RD*100000,1), " (", 
    sprintf("%0.1f" ,round_half_up(`RD 50%`*100000,1)), " (", 
    sprintf("%0.1f" , round_half_up(`RD CI2.5%`*100000,1))," to " ,
    sprintf("%0.1f" , round_half_up(`RD CI97.5%`*100000,1)), ")" )) %>%
  select(time_interval, RD) %>%
  mutate(time_interval = paste0(time_interval, " year")) %>%
  mutate(time_interval = case_when(time_interval== "13 year" ~ "End of follow up", 
                                   T~time_interval)) %>%
  rename(Age = time_interval)

tab <- left_join(events, bs_cuminc)
tab <- left_join(tab, RD)

##manually paste the overall RR and CIs into cell
writeData(workbook,    "eTable_10",tab,
          startRow = 4, startCol = 1, headerStyle = head.style)
writeData(workbook,  "eTable_10", " ",
          startRow = 3, startCol = 1)

mergeCells(workbook, "eTable_10", cols = 4:5, rows = 3)
writeData(workbook,    "eTable_10","Cumulative risk (per 100,000 individuals)", 
          startRow = 3, startCol = 4, headerStyle = head.style)
mergeCells(workbook, "eTable_10", cols = 7, rows = 3)
writeData(workbook,  "eTable_10", "Relative risk of being diagnosed with cancer (95% CI)",
          startRow = 4, startCol = 7)

RR <- paste0(round_half_up(bootres_cHR_summary$cHR_modelled, 2),
             " (", sprintf("%0.2f",round_half_up(bootres_cHR_summary$`chr_CI2.5%`, 2)), " - ",
             round_half_up(bootres_cHR_summary$`chr_CI97.5%`, 2),")")

mergeCells(workbook, "eTable_10", cols = 7, rows = 5:8)
writeData(workbook,  "eTable_10", RR,
          startRow = 5, startCol = 7)
addStyle(workbook,  "eTable_10",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=6, rows=5:8, gridExpand = TRUE)
addStyle(workbook,  "eTable_10",
         createStyle(wrapText = TRUE,valign = "center", halign="right",fgFill="white"),
         cols=7, rows=5:8, gridExpand = TRUE)
##right align cumulative risk
addStyle(workbook,  "eTable_10",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=4:5, rows=5:8, gridExpand = TRUE)

cuminc_header_trt <- paste0("Cumulative cancers diagnosed group treated with high dose folic acid (N = ",
                            scales::comma(n_lb$exposed_hdfa[1]), ") before the stated age")
cuminc_header_untrt <- paste0("Cumulative cancers diagnosed group not treated with high dose folic acid (N = ",
                              scales::comma(n_lb$unexposed_hdfa[1]), ") before the stated age")
#ad table headers
mergeCells(workbook, "eTable_10", cols = 2, rows = 3:4)
writeData(workbook,  "eTable_10", 
          cuminc_header_trt,
          startRow = 3, startCol = 2)
mergeCells(workbook, "eTable_10", cols = 3, rows = 3:4)
writeData(workbook,  "eTable_10", cuminc_header_untrt,
          startRow = 3, startCol = 3)
writeData(workbook,  "eTable_10", "Treated with high dose folic acid",
          startRow = 4, startCol = 4)
writeData(workbook,  "eTable_10", "Not treated with high dose folic acid",
          startRow = 4, startCol = 5)
writeData(workbook,  "eTable_10", "Risk difference (cancers per 100,000 individuals)",
          startRow = 4, startCol = 6)

addStyle(workbook,  "eTable_10", 
         style = createStyle(fgFill = "white", wrapText = TRUE,halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =6:7, gridExpand = TRUE)

addStyle(workbook,  "eTable_10", 
         style = createStyle(fgFill = "white", wrapText = TRUE, halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =4, gridExpand = TRUE)

addStyle(workbook,  "eTable_10", 
         style = createStyle(fgFill = "white", halign =  "center", wrapText = TRUE, textDecoration = "bold"), 
         rows = 3:4,   cols =c(2:3, 6,7), gridExpand = TRUE)

writeData(workbook,  "eTable_10", "Abbreviations: CI, confidence intervals ",
          startRow = 10, startCol = 1)
writeData(workbook,  "eTable_10", "Note: Maximum age at end of follow-up was 13 years. Relative risk estimated using inverse probability of treatment weighting and pooled logistic regression. Because the model assumed a constant treatment effect over time, the estimated relative risk applies across all follow-up periods.",
          startRow = 11, startCol = 1)
addStyle(workbook,  "eTable_10",
         createStyle(wrapText = TRUE, halign="left",fgFill="white"),
         cols=1, rows=c(10,11), gridExpand = TRUE, )

## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet =  "eTable_10", 
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
  sheet =  "eTable_10", 
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
  sheet =  "eTable_10", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = 3,
  cols = 4:5,
  stack = TRUE,
  gridExpand = TRUE
)

##eTable 11 Sensitivity ASM baseline characteristics####
addWorksheet(workbook, "eTable_11")

##add white background to everything
addStyle(workbook, "eTable_11", style = createStyle(fgFill = "white"), rows = 1:200, cols = 1:100, gridExpand = TRUE)
#add link back to contents

##run file that sets up the data for table 2
source("Figures_tables/ix.sens_ASM_table_demog.r")



writeData(workbook,"eTable_11","All singleton live births", startRow = 3, startCol = 2)
#mergeCells(workbook,"eTable_11", cols = 4:5, rows = 3)
writeData(workbook, "eTable_11","Treated with high dose folic acid", startRow = 3, startCol = 3)
#mergeCells(workbook, "eTable_11", cols = 6:7, rows = 3)
writeData(workbook, "eTable_11","Not treated with high dose folic acid", startRow = 3, startCol = 4)

addStyle(workbook, sheet ="eTable_11", centre.style,
         rows = 3, cols = 2:4, gridExpand = TRUE)
addStyle(workbook,"eTable_11", 
         createStyle(wrapText = TRUE, halign="right",fgFill="white",
                     textDecoration = "bold", border="bottom"),
         cols=c(2:4), rows=4)
##add bold styling 
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "Maternal", style = bold.style)
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "year of", style = bold.style)
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "sex", style = bold.style)
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "indicat", style = bold.style)
conditionalFormatting(workbook,"eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "total", style = bold.style)
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "cancer", style = bold.style)
conditionalFormatting(workbook,"eTable_11", cols = 1, rows = 1:200, 
                      type = "contains", rule = "birthweight", style = bold.style)
conditionalFormatting(workbook, "eTable_11", cols = 1, rows = 1:200, 
                      type = "contains",rule = "family", style = bold.style)

writeData(workbook, "eTable_11",
          "eTable 11: Baseline characteristics of included singleton live births, by treatment group. Sensitivity analysis limited to children of women receiving ASM in pregnancy",
          startCol = 1, startRow = 1)
writeData(workbook, "eTable_11",
          "\u1d43 SIMD = Scottish Index of Multiple Deprivation",
          startRow = 90, startCol = 1)
writeData(workbook, "eTable_11",
          "\u1d47 hdFA = high dose folic acid",
          startRow = 91, startCol = 1)
writeData(workbook, "eTable_11",
          "\u1d9c NTD =  neural tube defect",
          startRow = 92, startCol = 1)
writeData(workbook, "eTable_11",
          "\u1d48 ASM = antiseizure medication",
          startRow = 93, startCol = 1)

addStyle(workbook, "eTable_11",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 90:93, cols = 1, gridExpand = TRUE)

##make caption bold
addStyle(workbook, "eTable_11", bold.style, rows = c(1,4), cols = 1:4, gridExpand = TRUE)
addStyle(workbook, "eTable_11",
         createStyle(wrapText = TRUE, halign="right",fgFill="white",
                     textDecoration = "bold", border="bottom"), 
         rows = 4, cols = 2:4)
##write table with header styling

for (row in 1:nrow(t2_baseline_asm)){
  sheetRow <- data.frame(lapply(t2_baseline_asm[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook,"eTable_11", x = sheetRow, startRow = row+3, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook, "eTable_11", x = sheetRow, startRow = row+4, colNames = FALSE)
  }
}

writeData(workbook, "eTable_11","Indications for hdFA", startRow = 47, startCol = 1)
setColWidths(workbook, "eTable_11", cols = c(1:4), widths = c(65,33,33,33))
##1000s format
addStyle(workbook, "eTable_11", 
         createStyle(halign="right",fgFill="white"), 
         rows = 5:100, cols = c(2,3,4), gridExpand = TRUE)
addStyle(workbook,"eTable_11", 
         style = createStyle(halign="right",fgFill = "white",  border = "bottom"), 
         rows = nrow(t2_baseline_asm)+4, cols = c(2,3,4), gridExpand = TRUE)


##left align fist column (otherwise years are right aligned.)
addStyle(workbook, "eTable_11", 
         createStyle(halign="left",fgFill="white", 
         ), rows = 1:nrow(t2_baseline_asm)+4, cols = 1, gridExpand = TRUE)
##italics for indicators
addStyle(workbook, "eTable_11", 
         createStyle(halign="left",fgFill="white", textDecoration = "italic"
         ), rows = 48:74, cols = 1, gridExpand = TRUE)
##bold italics for header for indicators
addStyle(workbook, "eTable_11", 
         createStyle(halign="left",fgFill="white", textDecoration = "italic"
         ), rows = c(48,52,56,61,65,69,73), cols = 1, gridExpand = TRUE)

rangeRows = 3
rangeCols = 1:4
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_11", 
  style = openxlsx::createStyle(
    border = c("top"),
    borderStyle = c("thin")
  ),
  rows = c(3,5),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

## left  borders
rangeRows = 3:88

##bottom borders
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_11", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = tail(rangeRows, 1),
  cols = rangeCols,
  stack = TRUE,
  gridExpand = TRUE
)

## Sensitivity ASM  outcome rates ####
addWorksheet(workbook, "eTable_12")

writeData(workbook, "eTable_12", 
          "eTable 12: Childhood cancer (incidence rate) in included singleton live births, by treatment group. Sensitivity analysis limited to children of women receiving ASM in pregnancy",
          startRow = 1, startCol = 1)

source("Figures_tables/x.Sens_ASM_outcomes.r")

addStyle(workbook, "eTable_12", style = createStyle(fgFill = "white"), rows = 1:200, cols = 1:100, gridExpand = TRUE)
addStyle(workbook, "eTable_12",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

for (row in 1:nrow(overall_rates)){
  sheetRow <- data.frame(lapply(overall_rates[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  if (row == 1) {
    writeData(workbook, "eTable_12", x = sheetRow, startRow = row+2, colNames = TRUE,  headerStyle = head.style)
  } else {
    writeData(workbook, "eTable_12", x = sheetRow, startRow = row+3, colNames = FALSE)
  }
}
##1000s format

for (row in 1:nrow(stats_0_1)){
  sheetRow <- data.frame(lapply(stats_0_1[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  # if (row == 1) {
  ##    writeData(workbook, "eTable_7", x = sheetRow, startRow = row+9, colNames = TRUE,  headerStyle = head.style)
  #  } else {
  writeData(workbook,  "eTable_12", x = sheetRow, startRow = row+10, colNames = FALSE)
  # }
}

writeData(workbook,  "eTable_12","Cancer risk by age group",
          startRow = 10, startCol = 1)


for (row in 1:nrow(stats_1_4)){
  sheetRow <- data.frame(lapply(stats_1_4[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_12", x = sheetRow, startRow = row+16, colNames = FALSE)
  
}

for (row in 1:nrow(stats_5_9)){
  sheetRow <- data.frame(lapply(stats_5_9[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_12", x = sheetRow, startRow = row+22, colNames = FALSE)
  
}


for (row in 1:nrow(stats_10_14)){
  sheetRow <- data.frame(lapply(stats_10_14[row,],
                                function(x){type.convert(as.character(x))}),
                         check.names = FALSE, stringsAsFactors = FALSE)
  
  writeData(workbook,  "eTable_12", x = sheetRow, startRow = row+28, colNames = FALSE)
  
}
writeData(workbook, "eTable_12","",
          startRow = 3, startCol = 1, headerStyle = head.style)

addStyle(workbook,"eTable_12", 
         createStyle(wrapText = TRUE, halign="center",fgFill="white",textDecoration = "bold", border="bottom"),
         cols=c(2,3), rows=3)


addStyle(workbook, "eTable_12", 
         createStyle(halign="right",fgFill="white", 
                     numFmt = "COMMA" ), rows = 4:34, cols = c(2,3), gridExpand = TRUE)


writeData(workbook,  "eTable_12",
          "Abbreviations: PYAR, person years at risk; IQR, interquartile range; CI, confidence intervals ",
          startRow = 36, startCol = 1)
addStyle(workbook, "eTable_12",
         createStyle(halign="left", valign = "top", fgFill="white",
                     fontSize = 8, wrapText=FALSE),
         rows = 36, cols = 1, gridExpand = TRUE)

setColWidths(workbook,"eTable_12", cols = c(1:7), widths = c(68,20,20))
openxlsx::addStyle(
  wb = workbook,
  sheet = "eTable_12",
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = c(2,34),
  cols = 1:3,
  stack = TRUE,
  gridExpand = TRUE
)


## Sensitivity ASM model results ####

addWorksheet(workbook, "eTable_13")

addStyle(workbook,  "eTable_13", style = createStyle(fgFill = "white"),
         rows = 1:200, cols = 1:100, gridExpand = TRUE)

setRowHeights(workbook,  "eTable_13",rows=3, heights=27)

writeData(workbook,  "eTable_13",
          "eTable 13: Childhood cancer (relative risk) in included singleton live births, by treatment group. Sensitivity analysis limited to children of women receiving ASM in pregnancy",
          startRow = 1, startCol = 1)


setColWidths(workbook, "eTable_13", cols = c(1:7), widths = c(32,32,32,32, 32, 32, 32))
addStyle(workbook,   "eTable_13",  style = bold.style, rows = 1, cols = 1, gridExpand = TRUE)

##Singlton LBs
n_lb <- df_asm %>% group_by(hdfa_preg) %>% count() %>% ungroup() 
n_lb <- n_lb %>% pivot_wider(names_from = hdfa_preg, values_from = n) %>% mutate(col1 = "N live births")

df_asm <- df_asm %>%  mutate(age_yr_at_diag = interval(date_end_pregnancy, incidence_date) /years(1)) %>% 
  mutate(incid_0_1 = case_when(cancer_outcome==1 & age_yr_at_diag <1 ~1, T~0), 
         incid_1_4 = case_when(cancer_outcome==1 & age_yr_at_diag>=1 & cancer_outcome==1 & age_yr_at_diag <5 ~1, T~0), 
         incid_5_9 = case_when(cancer_outcome==1 & age_yr_at_diag >=5 & age_yr_at_diag <10 ~1, T~0), 
         incid_10_14 = case_when(cancer_outcome==1 & age_yr_at_diag >=10 & age_yr_at_diag <15 ~1, T~0) ) 
##cuminc 
##no events in first year
n_event_1 <- df_asm %>%  group_by(hdfa_preg, incid_0_1) %>% 
  count() %>% mutate(age = "1 year") %>%
  select(-n) %>% rename(n = incid_0_1)


n_event_5 <- df_asm  %>% filter(incid_0_1==1|incid_1_4==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "5 year")
n_event_10 <- df_asm %>% filter(incid_0_1==1|incid_1_4==1|incid_5_9==1) %>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "10 year")
n_event_end <- df_asm %>% filter(incid_0_1==1|incid_1_4==1 | incid_5_9==1|incid_10_14==1 )%>% group_by(hdfa_preg)%>% 
  count() %>% mutate(age = "End of follow up")

events <- rbind(n_event_1, n_event_5, n_event_10, n_event_end)
events <- events %>% pivot_wider(names_from = hdfa_preg, values_from = n) 
names(events) <- c("Age", "exposed_events", "unexposed_events")

##cumulative risks
##load main model results
results <- readRDS(paste0(folder_data_path, "outputs/sens_ASM_predictions_fitwts_spline.rds"))
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
bootres <- readRDS( paste0(folder_data_path, "outputs/sens_ASM_bootres1_500.rds"))

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
  mutate(cumrisk = paste0(
    sprintf("%0.1f", round_half_up(`CumInc 50%`*100000,1)), " (", 
    sprintf("%0.1f", round_half_up(`CumInc CI2.5%`*100000,1))," - " ,
    sprintf("%0.1f", round_half_up(`CumInc CI97.5%`*100000,1)), ")" )) %>%
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
  mutate(RD =  paste0(     sprintf("%0.1f", round_half_up(`RD 50%`*100000,1)), " (", 
    sprintf("%0.1f", round_half_up(`RD CI2.5%`*100000,1))," to " ,
    sprintf("%0.1f", round_half_up(`RD CI97.5%`*100000,1)), ")" )) %>%
  select(time_interval, RD) %>%
  mutate(time_interval = paste0(time_interval, " year")) %>%
  mutate(time_interval = case_when(time_interval== "13 year" ~ "End of follow up", 
                                   T~time_interval)) %>%
  rename(Age = time_interval)

tab <- left_join(events, bs_cuminc)
tab <- left_join(tab, RD)

##manually paste the overall RR and CIs into cell
writeData(workbook, "eTable_13",tab,
          startRow = 4, startCol = 1, headerStyle = head.style)
writeData(workbook,  "eTable_13", " ",
          startRow = 3, startCol = 1)

mergeCells(workbook, "eTable_13", cols = 4:5, rows = 3)
writeData(workbook, "eTable_13","Cumulative risk (per 100,000 individuals)", 
          startRow = 3, startCol = 4, headerStyle = head.style)
mergeCells(workbook, "eTable_13", cols = 7, rows = 3)
writeData(workbook,  "eTable_13", "Relative risk of being diagnosed with cancer (95% CI)",
          startRow = 4, startCol = 7)

RR <- paste0(round_half_up(bootres_cHR_summary$cHR_modelled, 2),
             " (", sprintf("%0.2f",round_half_up(bootres_cHR_summary$`chr_CI2.5%`, 2)), " - ",
             round_half_up(bootres_cHR_summary$`chr_CI97.5%`, 2),")")

mergeCells(workbook, "eTable_13", cols = 7, rows = 5:8)
writeData(workbook,  "eTable_13", RR,
          startRow = 5, startCol = 7)
addStyle(workbook,  "eTable_13",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=6, rows=5:8, gridExpand = TRUE)
addStyle(workbook,  "eTable_13",
         createStyle(wrapText = TRUE,valign="center", halign="right",fgFill="white"),
         cols=7, rows=5:8, gridExpand = TRUE)
##right align cumulative risk
addStyle(workbook,  "eTable_13",
         createStyle(wrapText = TRUE, halign="right",fgFill="white"),
         cols=4:5, rows=5:8, gridExpand = TRUE)

cuminc_header_trt <- paste0("Cumulative cancers diagnosed group treated with high dose folic acid (N = ",
                            scales::comma(n_lb$exposed_hdfa[1]), ") before the stated age")
cuminc_header_untrt <- paste0("Cumulative cancers diagnosed group not treated with high dose folic acid (N = ",
                              scales::comma(n_lb$unexposed_hdfa[1]), ") before the stated age")
#ad table headers
mergeCells(workbook, "eTable_13", cols = 2, rows = 3:4)
writeData(workbook,  "eTable_13", 
          cuminc_header_trt,
          startRow = 3, startCol = 2)
mergeCells(workbook, "eTable_13", cols = 3, rows = 3:4)
writeData(workbook,  "eTable_13", cuminc_header_untrt,
          startRow = 3, startCol = 3)
writeData(workbook,  "eTable_13", "Treated with high dose folic acid",
          startRow = 4, startCol = 4)
writeData(workbook,  "eTable_13", "Not treated with high dose folic acid",
          startRow = 4, startCol = 5)
writeData(workbook,  "eTable_13", "Risk difference (cancers per 100,000 individuals)",
          startRow = 4, startCol = 6)

addStyle(workbook,  "eTable_13", 
         style = createStyle(fgFill = "white", wrapText = TRUE,halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =6:7, gridExpand = TRUE)

addStyle(workbook,  "eTable_13", 
         style = createStyle(fgFill = "white", wrapText = TRUE, halign =  "center", textDecoration = "bold"), 
         rows = 3,   cols =4, gridExpand = TRUE)

addStyle(workbook,  "eTable_13", 
         style = createStyle(fgFill = "white", halign =  "center", wrapText = TRUE, textDecoration = "bold"), 
         rows = 3:4,   cols =c(2:3, 6,7), gridExpand = TRUE)

writeData(workbook,  "eTable_13", "Abbreviations: CI, confidence intervals ",
          startRow = 10, startCol = 1)
writeData(workbook,  "eTable_13", "Maximum age at end of follow-up was 13 years. Relative risk estimated using inverse probability of treatment weighting and pooled logistic regression. Because the model assumed a constant treatment effect over time, the estimated relative risk applies across all follow-up periods.",
          startRow = 11, startCol = 1)

addStyle(workbook,  "eTable_13",
         createStyle(wrapText = TRUE, halign="left",fgFill="white"),
         cols=1, rows=c(10,11), gridExpand = TRUE, )
## top borders
openxlsx::addStyle(
  wb = workbook,
  sheet =  "eTable_13", 
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
  sheet =  "eTable_13", 
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
  sheet =  "eTable_13", 
  style = openxlsx::createStyle(
    border = c("bottom"),
    borderStyle = c("thin")
  ),
  rows = 3,
  cols = 4:5,
  stack = TRUE,
  gridExpand = TRUE
)

##Fill in contents page####
addStyle(workbook, "Contents", style = createStyle(fgFill = "white"), rows = 1:200, cols = 1:100, gridExpand = TRUE)

writeData(workbook, "Contents","Contents page",
          startRow = 1, startCol = 1)
## Internal Hyperlin
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_1!A3\", "eTable 1: Definitions and derivation of confounders included in analyses")',
  startCol = 2, startRow = 4
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_2!A3\", "eTable 2: Derivation of confounder indicating maternal comorbidity (ICD-10)")',
  startCol = 2, startRow = 5
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_3!A3\", "eTable 3: Additional information on the high dose folic acid received by the treated group")',
  startCol = 2, startRow = 6
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_4!A3\", "eTable 4: Additional characteristics of included singleton live births, stratified by treatment group")',
  startCol = 2, startRow = 7
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_5!A3\", "eTable 5: Additional information on major congenital conditions seen in the treated and untreated group")',
  startCol = 2, startRow = 8
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_6!A3\", "eTable 6: Childhood cancer in included singleton live births, by treatment group")',
  startCol = 2, startRow = 9
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_7!A3\", "eTable 7: Additional information on the childhood cancers seen in the treated and untreated group")',
  startCol = 2, startRow = 10
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_8!A3\", "eTable 8: Baseline characteristics of pregnancies included in sensitivity analysis limited to any indication for hdFA, stratified by treatment group")',
  startCol = 2, startRow = 11
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_9!A3\", "eTable 9: Childhood cancer in included singleton live births, by treatment group. Sensitivity analysis limited to any indication")',
  startCol = 2, startRow = 12
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_10!A3\", "eTable 10: Relative risk of childhood cancer by specified age points in singleton live births with and indication for hdFA prescribing: treated, compared to not treated")',
  startCol = 2, startRow = 13
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_11!A3\", "eTable 11: Baseline characteristics of pregnancies included in sensitivity analysis limited to taking ASM, stratified by treatment grou")',
  startCol = 2, startRow = 14
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_12!A3\", "eTable 12: Childhood cancer in included singleton live births, by treatment group. Sensitivity analysis limited to ASM use")',
  startCol = 2, startRow = 15
)
writeFormula(
  workbook, "Contents",
  x = '=HYPERLINK(\"#eTable_13!A3\", "eTable 13: Relative risk of childhood cancer by specified age points in singleton live births to women taking ASM in pregnancy: treated, compared to not treated")',
  startCol = 2, startRow = 16
)
#####save file####

#openXL(workbook)
workbook$ActiveSheet<-as.integer(1)
saveWorkbook(workbook, paste0(folder_data_path, "outputs/supplementary_tables.xlsx"), overwrite=TRUE)

