# 
# summary scores for SCQ
#
#reference: https://www.carepatron.com/templates/social-communication-questionnaire
#https://www.sralab.org/rehabilitation-measures/social-communication-questionnaire.

library(data.table) 
library(tidyverse)
library(dplyr) 


selfreport_all <- read.csv("RareCNVSelfreportSca_DATA_2025-01-31_2103.csv")


#selfreport2<-selfreport_all %>%
#  group_by(record_id) %>%
#  summarize_all(~ max(as.character(.), na.rm = TRUE)) %>%
#  ungroup

#selfreport_admin<-selfreport_all[which(selfreport_all$redcap_event_name=="administration_arm_1"),]
#cleanup 
#remove if missing rarecnv_id - no way of linking 
selfreport_admin<-selfreport_all[which(selfreport_all$redcap_event_name=="administration_arm_1" & selfreport_all$rarecnv_id!=""),]

#proband self reports
selfreport<-selfreport_all[which(selfreport_all$redcap_event_name=="subject_selfreport_arm_1"),]
selfreport<-selfreport[selfreport$record_id %in% selfreport_admin$record_id,]

#collateral self reports
selfreport_col<-selfreport_all[which(selfreport_all$redcap_event_name=="collateral_selfrep_arm_1"),]
selfreport_col<-selfreport_col[which(selfreport_col$record_id %in% selfreport_admin$record_id),]

#for rows_path to work need NA and not empty strings
selfreport_admin[selfreport_admin==""]<-NA
selfreport_col2<-rows_patch(selfreport_admin, selfreport_col, by = "record_id",unmatched = "ignore") #update only missing values

#quality check- keep complete surveys
selfreport_col2<-selfreport_col2[which(selfreport_col2$social_communication_questionnaire_scq_complete==2),]

#scoring

# limit to scq columns to simplify scoring 
scq <- selfreport_col2[, c('rarecnv_id', "scales_age",grep('scq', names(selfreport_col2), value=TRUE))] %>%
  select(-social_communication_questionnaire_scq_complete)

# convert data to integer  (unk, M gets converted to NA's)
scq[,-which(names(scq) == 'rarecnv_id')] <-  lapply(scq[,-which(names(scq) == 'rarecnv_id')], as.integer)

# reverse coding columns: 2, 9, 19-40
reverse_code_cols<-c("scq_2","scq_9",colnames(scq)[grepl("^scq_(19|2[0-9]|3[0-9]|40)$",colnames(scq))])
scq[, reverse_code_cols] <- 1 - scq[, reverse_code_cols]

#scq3 %>%
#  mutate(across(reverse_code_cols, ~  . - 1))


# Total score
scq <- scq %>%
  mutate(scq_total = ifelse(scq$scq_1 == 1, rowSums(scq[,-c(1, 2)], na.rm = TRUE), ifelse(scq$scq_1 == 0, rowSums(scq[,-c(1:8)], na.rm = TRUE), NA)))

# rows with more than 3 missing values in SCQ items not valid
#num_missing <- rowSums(is.na(scq3))
#none meet criteria
#scq3$scq_smry[num_missing > 3] <- NA 


scq$corepsych_asd_scq<-NA
scq$corepsych_asd_scq[scq$scq_total>=15]<-"No ASD"
scq$corepsych_asd_scq[scq$scq_total<15]<-"Subclinical ASD"

