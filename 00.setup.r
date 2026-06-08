#Setup ####
## this script loads packages, esablishes file paths and conncts to SMRA

library(renv)
library(odbc)
library(dplyr)
library(janitor)
library(hablar)
library(lubridate)
library(keyring)
library(phsmethods)
library(arrow)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)

#renv::init()
keyring::keyring_unlock(keyring = "DATABASE",
                        password = source("~/database_keyring.R")[["value"]])

SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], #
                            pwd = keyring::key_get("SMRA", Sys.info()[["user"]], keyring = "DATABASE"))

denodo_connection <- dbConnect(odbc(),
                                dsn = "DVPROD",
                               bigint = "integer",
                               uid = Sys.info()[["user"]],
                                pwd = keyring::key_get("DVPROD", Sys.info()[["user"]], keyring = "DATABASE"))

##Filepaths
folder_data_path <- "/conf/FolicAcid/data/"


###Define the code lists for relevant comorbidities####
Haematological <-	c(paste0("D",50:55),"D560", "D562", "D563", "D564", "D568","D569",
                    paste0("D",58:89), "O990", "O991")


haem_excl_anaemia<-	c(paste0("D",65:89),  "O991")

Thyroid <-	c(paste0("E0",0:7), "O992")
Demyelinating_NMD <-	c("G35", "G36", "G37", paste0("G",70:73))
Hypertension<-	c(paste0("I", 10:15), "O10")
VTE <- 	c("I26", "I80", "I81", "I82")
Asthma <-	c("J45", "J46", "O995")
GI_diseases <-	c(paste0("K", 25:28), paste0("K", 50:52), paste0("K", 80:86), "O996")
Liver <-	c(paste0("K", 71:76))
Skin <-	c(paste0("L", 10:45), "O997")
Immune_mediated_joint_connective_tissue<- 	c("M05", "M06", "M08", paste0("M", 30:35))
Kidney_diseases <-	c(paste0("N0",c(0:7)), paste0("N",c(10:15)), "N17", "N18", "N19")
Disorders_pelvic_genital_tract <-	c(paste0("N", 70:77), paste0("N",80:94), paste0("N", 96:98))

###Define code lists for NTD, sickle cell, thalassemia, coeliac, pre-pregnancy diabetes####
NTD <- c("Q00", "Q01", "Q05")
Coeliac <- "K90"
Sickle_cell <- "D57"
Diabetes<- c("E10", "E11", "E12", "E13", "E14", "O240","O241","O242","O243")
Thalassaemia <-	"D561"



###ethnicity labels 
#labels for unagegated ethnicity codes
ethnicity_labels <- function(ethnicity_code) {case_when(ethnicity_code == "1A" ~ "Scottish",
                                                         ethnicity_code == "1B" ~ "Other British",
                                                         ethnicity_code == "1C" ~ "Irish",
                                                         ethnicity_code == "1D" ~ "OLD_CODE Any other White Background" ,
                                                         ethnicity_code == "1E" ~ "English",
                                                         ethnicity_code == "1F" ~ "Welsh",
                                                         ethnicity_code == "1G" ~ "Northern Irish",
                                                         ethnicity_code == "1H" ~ "British",
                                                         ethnicity_code == "1J" ~ "Irish",
                                                         ethnicity_code == "1K" ~ "Gypsy/Traveller",
                                                         ethnicity_code == "1L" ~ "Polish",
                                                         ethnicity_code == "1Z" ~ "Other white ethnic group",
                                                        ethnicity_code == "E EUROPE EXC POLAND" ~ "Other white ethnic group",
                                                        ethnicity_code == "2A" ~ "Any mixed or multiple ethnic groups",
                                                         ethnicity_code== "3A" ~ "OLD_CODEIndian",
                                                         ethnicity_code== "3B" ~ "OLD_CODEPakistani",
                                                         ethnicity_code== "3C" ~ "OLD_CODEBangladeshi",
                                                         ethnicity_code== "3D" ~ "OLD_CODEChinese",
                                                         ethnicity_code=="3E" ~ "OLD_CODEAny other Asian background",
                                                         ethnicity_code=="3F" ~ "Pakistani, Pakistani Scottish or Pakistani British",
                                                         ethnicity_code=="3G" ~ "Indian, Indian Scottish or Indian British",
                                                         ethnicity_code=="3H" ~ "Bangladeshi, Bangladeshi Scottish or Bangladeshi British",
                                                         ethnicity_code=="3J" ~ "Chinese, Chinese Scottish or Chinese British",
                                                         ethnicity_code=="3Z" ~ "Other Asian, Scottish Asian or British Asian",
                                                         ethnicity_code=="4A" ~ "OLD_CODECaribbean",
                                                         ethnicity_code=="4B" ~ "OLD_CODEAfrican",
                                                         ethnicity_code=="4C" ~ "OLD_CODEAny other black background",
                                                         ethnicity_code=="4D" ~ "African, African Scottish or African British",
                                                         ethnicity_code=="4E" ~ "Caribbean, Caribbean Scottish or Caribbean British",
                                                         ethnicity_code=="4F" ~ "Black, Black Scottish or Black British",
                                                         ethnicity_code=="4X" ~ "African, African Scottish or African British",
                                                         ethnicity_code=="4Y" ~ "Other African",
                                                         ethnicity_code=="4Z" ~ "OLD_CODEOther African, Caribbean or Black",
                                                         ethnicity_code=="5A" ~ "OLD_CODEAny other ethnic background",
                                                         ethnicity_code=="5B" ~ "OLD_CODEArab",
                                                         ethnicity_code=="5C" ~ "Caribbean, Caribbean Scottish or Caribbean British",
                                                         ethnicity_code=="5D" ~ "Black, Black Scottish or Black British",
                                                         ethnicity_code=="5X" ~"Caribbean or Black",
                                                         ethnicity_code=="5Y" ~ "Other Caribbean or Black",
                                                         ethnicity_code=="5Z" ~ "Other ethnic group",
                                                         ethnicity_code=="6A" ~ "Arab, Arab Scottish or Arab British",
                                                         ethnicity_code=="6Z" ~ "Other ethnic group",
                                                         ethnicity_code=="98" ~"Prefer not to say",
                                                         ethnicity_code=="99" ~"Not known",
                                                        
                                                   T~"Not known")
}
                                                     

