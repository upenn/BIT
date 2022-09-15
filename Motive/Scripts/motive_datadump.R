
#demographics based on study enroll

#libraries
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
# function for datediff in months
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# Study Enroll
{
  
  studyAll<-read.csv("/data/secure/bit_dwh/oracle/bbl_study_all.csv", comment.char="#")
  names(studyAll)<-tolower(names(studyAll))
  #pull out motive [dw changed grmpy in all comments-> motive] enrolled subjects
  studyEnroll<-studyAll[which(studyAll$protocol=='829502 - MOTIVE'),]
  studyEnroll$bblid = as.numeric(studyEnroll$bblid)
  
  studyEnroll <- studyEnroll%>%
    filter( bblid>1000, bblid != 18026)
    #DW - 18026 used to be incorrectly listed as enrolled in motive, now fixed
  
  
  studyEnroll$formatted_doenroll<- as.Date(as.character(studyEnroll$doenroll), format = "%d-%b-%y")
  studyEnroll<- studyEnroll%>%
    group_by(bblid)%>%
    arrange(formatted_doenroll, .by_group = TRUE)
  
  
  studyEnroll$system_Datediff<- difftime(Sys.Date(),studyEnroll$formatted_doenroll, units = "days")
  
  
  studyEnroll<-within(studyEnroll, mydiff <- ave(system_Datediff, bblid, FUN=function(x) x - x[1]))
  
  #DW see this for ave www.r-bloggers.com/2013/09/ave-and-the-function-in-r/  , it performs a function within levels/instances of a given variable/factor, in this case bblid
  
  studyEnroll<-studyEnroll %>%
    mutate(timep = ifelse(abs(mydiff)>365,2,ifelse(abs(mydiff)==0,1,3)))
  
  #MS - Timepoints are coded but the timepoints generated in oracle are used for any calculations.
   
  #DW mutate adds columns: dplyr.tidyverse.org/reference/mutate.html, here it is adding the calculated timep variable, note tp=3 is anything neither 0 nor >365d
  
  
  studyEnroll2<-studyEnroll%>%
    select(bblid, timepoints, formatted_doenroll)
    
  #DW THIS IS JUST 2 VARIABLE STUDYENROLL TO USE FOR OTHER DATATYPES BELOW
  
  
  ostudy<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/studyEnroll.csv")
  write.csv(studyEnroll,ostudy,na="",row.names=F)
  #write.csv(studyEnroll,"C:/Users/Mrugank/Desktop/oracle dump/Oracle_dump_pmacs_10_08/studyenroll_motive.csv", row.names = FALSE)
  }


# {
#   demo<-read.csv("/data/secure/bbldm/subjectdemosall_v.csv") 
#   names(demo)<-tolower(names(demo))
#   demo2<-demo[demo$bblid %in% studyEnroll$bblid,]
#   demo2<-demo2[which(demo2$protocol=='829502 - MOTIVE'),]
#   
#   demographics_motive <- left_join(studyEnroll2,demo2, by = "bblid")
# 
#   odemo<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/demographics.csv")
#   write.csv(demographics_motive,odemo,na="",row.names=F)
#   
#   
# }

{
demo<-read.csv("/data/secure/bit_dwh/oracle/subjectvisitsall.csv")
names(demo)<-tolower(names(demo))
#all demographics for bblids matching those that got ENROLLED (not scanned, since some enrolled people never scanned) for MOTIVE
demo2<-demo[demo$bblid %in% studyEnroll$bblid,]

#motive specific demo collected
demo3<-demo2[which(demo2$protocol=='829502 - MOTIVE'),]

# Rank takes care of multiples. Filtered to # MS latest DOVISIT #DW NO? LATEST?
# Run the whole pipe together
#DW NOTE THAT IF TAKING MOST RECENT DEMOGRAPHICS, THIS WILL CHANGE OVER TIME
#DW - i think dovisit is normally same as doenroll, i think demographics recollected or updated every time enrollment done, but not every scan visit

#MS - Previously, the following code was run on demo2 which was giving the latest collected demographics across all protocols. This has now been changed to
# give demographics collected for motive first and if missing, take the closest to doenroll

demo3<-demo3%>%
  mutate(new_dovisit = lubridate::dmy(dovisit))
  # group_by(bblid, new_dovist)%>%
  # mutate(rank = rank(desc(new_dovisit)))%>%
  # filter(rank == 1)  

# MS - this becomes redundant and commenting it out for now

## BBLID that are in study enroll but not marked as Motive
#NOT SURE THIS DISTINCTION IS WORKING RIGHT,  SEE 20082 THEY HAVE A MOTIVE DEMOGRAPHICS BUT ITS PULLING LONGLUCEST ANYWAY... 
#MS - this should work correctly

nodemo<-demo2[!demo2$bblid %in% demo3$bblid,]

## Descending Rank filters to the latest DOVISIT  #YES I THINK TRUE!
nodemo<-nodemo %>%
  mutate(new_dovisit = lubridate::dmy(dovisit))%>%
  group_by(bblid)%>%
  mutate(rank = rank(desc(new_dovisit)))%>%
  filter(rank == 1)

#combine
demofinal<-bind_rows(demo3,nodemo)
names(demofinal)<-tolower(names(demofinal))
#merge with study enroll

# The datediff gives you the demographics collected closest to the doenroll

demographics_motive <- left_join(studyEnroll2,demofinal, by = "bblid")%>%
  mutate(dateDiff = abs(formatted_doenroll - new_dovisit)) %>%
  group_by(bblid, formatted_doenroll) %>%
  filter(dateDiff == min(dateDiff))



odemo<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/demographics.csv")
write.csv(demographics_motive,odemo,na="",row.names=F)
}
# # Remove PHI
# 
# removePHI<-c("PROTOCOL.y","SID.x","SID.y","LNAME","FNAME","SUFFIX","PERM_ADDRESS","PERM_CITY","PERM_STATE","PERM_ZIP","LOCAL_ADDRESS","LOCAL_CITY","LOCAL_STATE","LOCAL_ZIP","HPHONE","WPHONE","CPHONE","EMAIL")
# 
# #removePHI<-c("PROTOCOL.y","PERM_ADDRESS","PERM_CITY","PERM_STATE","PERM_ZIP","LOCAL_ADDRESS","LOCAL_CITY","LOCAL_STATE","LOCAL_ZIP","HPHONE","WPHONE","CPHONE","EMAIL")
# 
# demographics_midline<- demographics_midline%>%
#   select(-removePHI)
# 
# names(demographics_midline)<-tolower(names(demographics_midline))


#imglook

{
  img<-read.csv("/data/secure/bit_dwh/oracle/imglook.csv")
  names(img)<-tolower(names(img))
  # all BBLID from study enroll
  img<-img[img$bblid %in% studyEnroll$bblid,]
  
  #grmpy specific img collected
  img_mot<-img[which(img$protocol=='829502 - MOTIVE'),]
  #pull out IS5
  img_mot<-subset(img_mot, !grepl("^IS5", scanstat))#135
  img_mot$scanstat<-factor(img_mot$scanstat)
  #calculate scanage
  imgfinal<-merge(img_mot,demo2[,c("bblid","dobirth")],by="bblid")
  imgfinal$formatted_doscan<- as.Date(as.character(imgfinal$doscan), format = "%d-%b-%y")
  
  imgfinal$ScanAgeMonths<-mapply(mondf,as.Date(as.character(imgfinal$dobirth),"%d-%b-%y"),as.Date(as.character(imgfinal$doscan),"%d-%b-%y"))
  
  
  imgfinal<- imgfinal%>%
    group_by(bblid)%>%
    arrange(formatted_doscan, .by_group = TRUE)
  
  
  imgfinal$system_Datediff<- difftime(Sys.Date(),imgfinal$formatted_doscan, units = "days")
  
  
  imgfinal<-within(imgfinal, mydiff <- ave(system_Datediff, bblid, FUN=function(x) x - x[1]))
  
  imgfinal<-imgfinal %>%
    mutate(scanpoint = ifelse(abs(mydiff)>365,2,ifelse(abs(mydiff)<365,1,3)))
  
  
  img_motive<-imgfinal[,c("bblid","scanid","doscan","protocol","scanstat","head_coil","drug_test","pregnancy_test","ScanAgeMonths","comments", "scanpoint")]
  
  oimg<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/imglook.csv")
  write.csv(img_motive,oimg,na="",row.names=F)
  
  
  
}

#merge<- left_join(demographics_motive,img_motive, by = c("bblid" = "bblid","timepoints" = "scanpoint"))


# Diagnosis
{
  dx<-paste0("/data/secure/bit_repo/derivatives/diagnosis/diagnosis_wsmryvars_20220523.csv")
  diagnosis<-read.csv(dx)
  #diagnosis<- read.csv("/data/secure/lab/derivatives/diagnosis/diagnosis_wsmryvars_20211122.csv", as.is=TRUE)
  names(diagnosis)<-tolower(names(diagnosis))
  diagnosis <- diagnosis%>%
    filter( bblid>1000, bblid != 18026)
  
  diagnosis$bblid_dodiagnosis <- paste(diagnosis$bblid,"_", diagnosis$dodiagnosis)  
  
  diagnosis$formatted_dodiagnosis<- as.Date(as.character(diagnosis$dodiagnosis), format = "%d-%b-%y")
  
  diagnosis1<- diagnosis%>%
    group_by(bblid)%>%
    mutate(rank = rank(formatted_dodiagnosis))%>%
    arrange(formatted_dodiagnosis, .by_group = TRUE)
}

# Diagnosis code for where you will have a repeated diagnosis for tp 2 and 9 if no corresponding diagnosis exists.

{
  merge_diag <- left_join(studyEnroll2, diagnosis1, by = "bblid")%>%
  mutate(dateDiff = abs(formatted_doenroll - formatted_dodiagnosis)) %>%
  group_by(bblid, formatted_doenroll) %>%
  filter(dateDiff == min(dateDiff))

  
  merge_diag$diag_datediff <-abs(mapply(mondf,merge_diag$formatted_doenroll,merge_diag$formatted_dodiagnosis))

  odx<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/diagnosis.csv")
  write.csv(merge_diag,odx,na="",row.names=F)
}

# Diagnosis code for where tp2 will have a diagnosis done within 180 days of doenroll and tp9 will be NA

{
  enroll_tp1<-studyEnroll2%>%
    filter(timepoints == 1)
  
  enroll_tp2<-studyEnroll2%>%
    filter(timepoints == 2)
  
  enroll_tp9<-studyEnroll2%>%
    filter(timepoints == 9)
  
  merge_diag_tp1 <- left_join(enroll_tp1, diagnosis1, by = "bblid")%>%
    mutate(dateDiff = abs(formatted_doenroll - formatted_dodiagnosis)) %>%
    group_by(bblid, formatted_doenroll) %>%
    filter(dateDiff == min(dateDiff)| is.na(dateDiff))
  
  
  diagnosis2<- diagnosis1[!diagnosis1$bblid_dodiagnosis %in% merge_diag_tp1$bblid_dodiagnosis,]
  
  merge_diag_tp2 <- left_join(enroll_tp2, diagnosis2, by = "bblid")%>%
    mutate(dateDiff = abs(formatted_doenroll - formatted_dodiagnosis)) %>%
    group_by(bblid, formatted_doenroll) %>%
    filter(dateDiff >= 0 & dateDiff<= 180 | is.na(dateDiff))
  
  merge_diag_tp2_1 <- left_join(enroll_tp2, merge_diag_tp2) 
  
  
  all_diagnosis<- bind_rows(merge_diag_tp1,merge_diag_tp2_1, enroll_tp9)
  odx<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/diagnosis_updated_tp.csv")
  write.csv(all_diagnosis,odx,na="",row.names=F)
  
}






# Medications
{
  medicine<- read.csv("/data/secure/bit_dwh/oracle/medicineall_v.csv", comment.char="#")
  names(medicine)<-tolower(names(medicine))
  medicine<-medicine[which(medicine$protocol=='829502 - MOTIVE'),]
  medicine <- medicine%>%
    filter( bblid>1000, bblid != 18026)
  medicine$bblid<-as.numeric(medicine$bblid)
  
  omed<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/medications.csv")
  write.csv(medicine,omed,na="",row.names=F)
}

#merge_diag_med<-left_join(merge_diag,medicine, by = "bblid")

#sips
{
  sips<-read.csv("/data/secure/bit_dwh/oracle/sips.csv", comment.char="#")
  names(sips)<-tolower(names(sips))
  sips<-sips[sips$bblid %in% studyEnroll$bblid,]
  sips$type2<-sips$type
  sips$type2[sips$type2=="AP"|sips$type2=="IP"|sips$type2=="FP"|sips$type2=="Intake Proband"|sips$type2=="P"|sips$type2=="MP"]<-"Proband"
  sips$type2[sips$type2=="FC"|sips$type2=="GO2MI_2"|sips$type2=="GO2YPI_2"|sips$type2=="IC"|sips$type2=="MI"|sips$type2=="YPI"]<-"Collateral"
  sips$type2[sips$type2=="FPC"|sips$type2=="IPC"]<-"Combined"
  sips$type2<-factor(sips$type2)
  #matching on closest study enroll date
  sips2<-merge(sips,studyEnroll[,c("bblid","doenroll")])
  sips2$sipsenrolldiff<-abs(mapply(mondf,as.Date(as.character(sips2$dosips),"%d-%b-%y"),as.Date(as.character(sips2$doenroll),"%d-%b-%y")))
  sips2 <- as.data.table(sips2)
  sips3<-as.data.frame(sips2[sips2[, .I[which.min(sipsenrolldiff)], by=bblid]$V1])
  
  os1<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/sips_all.csv")
  write.csv(sips3,os1,na="",row.names=F)
  
}


# common scales

axis<- read.csv("/data/secure/bit_dwh/axis/axis_common_selfreport.csv")
axis<-axis%>%
  filter(bblid>1000)
names(axis)[1] <- "redcapid"


# ari
{
  
  
  
  axis_ari <- axis %>%
    select(redcapid,	admin_timestamp,	protocol,	protocol_number,	
           bblid,	libi_id,	scales_id,	scales_administration,	date,
           assessor,	location, admin_sex,
           ari_1, ari_2,	ari_3,	ari_4,	ari_5,	ari_6,	ari_7,	ari_comments,ari_proband_complete
    ) %>%
    #filter(!is.na(ari_1))%>%
    filter(protocol == "motive")
  
  
  
  
  
  
  #oari<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/ari_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(nda_ari,oari,na="",row.names=F)
  
}




#panas

{
  
  axis_panas <- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           #protocol,
           # admin_timestamp,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date,
           # assessor,	location, admin_sex,
           panas_1, panas_2, panas_3, panas_4, panas_5, panas_6, panas_7, panas_8, panas_9, panas_10, positive_and_negative_affect_schedule_child_panas_complete
           
    )
    #filter(!is.na(panas_1))%>%
    

  
  
 
  #opanas<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/panas_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(nda_panas,opanas,na="",row.names=F)
  
  
}




# bisbas

{
  axis_bisbas <- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location, admin_sex,
           bisbas_1,	bisbas_2,	bisbas_3,	bisbas_4,	bisbas_5,	bisbas_6,
           bisbas_7,	bisbas_8,	bisbas_9,	bisbas_10,	bisbas_11,	bisbas_12,
           bisbas_13,	bisbas_14,	bisbas_15,	bisbas_16,	bisbas_17,	bisbas_18,
           bisbas_19,	bisbas_20,	bisbas_21,	bisbas_22,	bisbas_23,	bisbas_24,	bisbas_comments,bisbas_complete
    ) 
    #filter(!is.na(bisbas_1))%>%
    
    
  # obisbas<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/bisbas_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(nda_bisbas,obisbas,na="",row.names=F)
  
}

#AES

{
  axis_aes<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location
           ,aes_1
           ,aes_2
           ,aes_3
           ,aes_4
           ,aes_5
           ,aes_6
           ,aes_7
           ,aes_8
           ,aes_9
           ,aes_10
           ,aes_11
           ,aes_12
           ,aes_13
           ,aes_14
           ,aes_15
           ,aes_16
           ,aes_17
           ,aes_18
           ,apathy_evaluation_scale_aes_complete)
    #filter(!is.na(aes_1))%>%
    
 
  
  #oaes<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/aes_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_aes,oaes,na="",row.names=F)
  
  
}

#GRIT
{
  axis_grit<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           grit_1,
           grit_2,
           grit_3,
           grit_4,
           grit_5,
           grit_6,
           grit_7,
           grit_8,
           grit_9,
           grit_10,
           grit_11,
           grit_12,
           grit_13,
           grit_14,
           grit_15___0,
           grit_15___1,
           grit_15___2,
           grit_15___3,
           grit_comments,
           grit_complete
           
    )
    #filter(!is.na(grit_1))%>%
    
   
  
  #ogrit<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/grit_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_grit,ogrit,na="",row.names=F)
  
}

#prime_sr_d_wolf

{
  axis_prime_wolf<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           prime_sr_d_wolf_1,
           prime_sr_d_wolf_1_distress,
           prime_sr_d_wolf_2,
           prime_2_distress,
           prime_sr_d_wolf_3,
           prime_sr_d_wolf_3_distress,
           prime_sr_d_wolf_4,
           prime_sr_d_wolf_4_distress,
           prime_sr_d_wolf_5,
           prime_sr_d_wolf_5_distress,
           prime_sr_d_wolf_6,
           prime_sr_d_wolf_6_distress,
           prime_sr_d_wolf_7,
           prime_sr_d_wolf_7_distress,
           prime_sr_d_wolf_8,
           prime_sr_d_wolf_8_distress,
           prime_sr_d_wolf_9,
           prime_sr_d_wolf_9_distress,
           prime_sr_d_wolf_10,
           prime_sr_d_wolf_10_distress,
           prime_sr_d_wolf_11,
           prime_sr_d_wolf_11_distress,
           prime_sr_d_wolf_12,
           prime_sr_d_wolf_12_distress
           ,prime_screen_srdwolf_complete

           )
    #filter(!is.na(prime_sr_d_wolf_1))
  
  #oprime<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/prime_sr_d_wolf_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_prime_wolf,oprime,na="",row.names=F)

}

#GCOS_CP v1

{
  axis_gcos_cp_v1<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           gcos_cp_1a,
           gcos_cp_1b,
           gcos_cp_1c,
           gcos_cp_2a,
           gcos_cp_2b,
           gcos_cp_2c,
           gcos_cp_3a,
           gcos_cp_3b,
           gcos_cp_3c,
           gcos_cp_4a,
           gcos_cp_4b,
           gcos_cp_4c,
           gcos_cp_5a,
           gcos_cp_5b,
           gcos_cp_5c,
           gcos_cp_6a,
           gcos_cp_6b,
           gcos_cp_6c
           ,gcoscp_complete

    )
    

 # ogcos_cp_v1<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/gcos_cp_v1_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_gcos_cp_v1,ogcos_cp_v1,na="",row.names=F)
}


#GCOS_CP_v2
{
  axis_gcos_cp_v2<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           gcos_cp_v2_1a,
           gcos_cp_v2_1b,
           gcos_cp_v2_1c,
           gcos_cp_v2_2a,
           gcos_cp_v2_2b,
           gcos_cp_v2_2c,
           gcos_cp_v2_3a,
           gcos_cp_v2_3b,
           gcos_cp_v2_3c,
           gcos_cp_v2_4a,
           gcos_cp_v2_4b,
           gcos_cp_v2_4c,
           gcos_cp_v2_5a,
           gcos_cp_v2_5b,
           gcos_cp_v2_5c,
           gcos_cp_v2_6a,
           gcos_cp_v2_6b,
           gcos_cp_v2_6c,
           gcos_cp_v2_7a,
           gcos_cp_v2_7b,
           gcos_cp_v2_7c,
           gcos_cp_v2_8a,
           gcos_cp_v2_8b,
           gcos_cp_v2_8c
           ,general_comprehensive_operating_system_clinical_po_complete
           
           
    )
    #filter(!is.na(gcos_cp_v2_1a))%>%
    

  
  #ogcos_cp_v2<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/gcos_cp_v2_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_gcos_cp_v2,ogcos_cp_v2,na="",row.names=F)
}

#WOLF_Trait 

{
  axis_wolf_trait<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
  
  wolf_trait_1,
  wolf_trait_2,
  wolf_trait_3,
  wolf_trait_4,
  wolf_trait_5,
  wolf_trait_6,
  wolf_trait_7,
  wolf_trait_8,
  wolf_trait_9
  ,wolf_trait_complete
  
    )
    #filter(!is.na(wolf_trait_1))%>%
    
   
  
  #owolf_trait<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/wolf_trait_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_wolf_trait,owolf_trait,na="",row.names=F)
}

#WOLD_IM_EM

{
  axis_wolf_im_em<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
  
  wolf_im_em_1,
  wolf_im_em_2,
  wolf_im_em_3,
  wolf_im_em_4,
  wolf_im_em_5,
  wolf_im_em_6,
  wolf_im_em_7,
  wolf_im_em_8,
  wolf_im_em_9,
  wolf_im_em_10,
  wolf_im_em_11,
  wolf_im_em_12,
  wolf_im_em_13,
  wolf_im_em_14
  ,wolf_imem_short_complete
  
    )
    #filter(!is.na(wolf_im_em_1))%>%
    
  
  
  #owolf_im_em<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/wolf_im_em_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(axis_wolf_im_em,owolf_im_em,na="",row.names=F)
  
}


#SWLS

{
  axis_swls<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
  swls_1,
  swls_2,
  swls_3,
  swls_4,
  swls_5,
  swls_6,
  swls_7,
  swls_8,
  swls_9,
  swls_10,
  swls_11,
  swls_12,
  swls_13,
  swls_14,
  swls_15,
  swls_16,
  swls_17,
  swls_18
  ,satisfaction_with_life_scale_swls_complete
    )
    
  
  
  #oswls<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/swls_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(axis_swls,oswls,na="",row.names=F)
}

#RSAS

{
  axis_rsas<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
  soc_anhed_1,
  soc_anhed_2,
  soc_anhed_3,
  soc_anhed_4,
  soc_anhed_5,
  soc_anhed_6,
  soc_anhed_7,
  soc_anhed_8,
  soc_anhed_9,
  soc_anhed_10,
  soc_anhed_11,
  soc_anhed_12,
  soc_anhed_13,
  soc_anhed_14,
  soc_anhed_15,
  soc_anhed_comments
  ,revised_social_anhedonia_scale_rsas_complete
  
    )
    
 
  
  #orsas<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/rsas_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_rsas,orsas,na="",row.names=F)
}

#RPAS

{
  axis_rpas<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           phys_anhed_1,
           phys_anhed_2,
           phys_anhed_3,
           phys_anhed_4,
           phys_anhed_5,
           phys_anhed_6,
           phys_anhed_7,
           phys_anhed_8,
           phys_anhed_9,
           phys_anhed_10,
           phys_anhed_11,
           phys_anhed_12,
           phys_anhed_13,
           phys_anhed_14,
           phys_anhed_15,
           phys_anhed_comments
           ,revised_physical_anhedonia_scale_rpas_complete
           )
    

  
  #orpas<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/rpas_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_rpas,orpas,na="",row.names=F)
}

#eswan_social

{
  axis_eswan_social<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,	admin_timestamp,	protocol,	protocol_number,
           bblid,	libi_id,	scales_id,	scales_administration,	date
           ,assessor,	location,
           eswan_social_01,
           eswan_social_02,
           eswan_social_03,
           eswan_social_04a,
           eswan_social_04b,
           eswan_social_05
           ,eswan_social_anxiety_proband_complete
           
           
           
    )
   

  
  #oeswansocial<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/eswan_social_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_eswan_social,oeswansocial,na="",row.names=F)
}

#eswan_dmdd

{
  axis_eswan_dmdd<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           eswan_dmdd_01a,
           eswan_dmdd_01b,
           eswan_dmdd_01c,
           eswan_dmdd_02a,
           eswan_dmdd_02b,
           eswan_dmdd_02c,
           eswan_dmdd_03a,
           eswan_dmdd_03b,
           eswan_dmdd_03c,
           eswan_dmdd_04a,
           eswan_dmdd_04b,
           eswan_dmdd_04c,
           eswan_dmdd_05a,
           eswan_dmdd_05b,
           eswan_dmdd_05c,
           eswan_dmdd_06a,
           eswan_dmdd_06b,
           eswan_dmdd_06c,
           eswan_dmdd_07a,
           eswan_dmdd_07b,
           eswan_dmdd_07c,
           eswan_dmdd_08a,
           eswan_dmdd_08b,
           eswan_dmdd_08c,
           eswan_dmdd_09a,
           eswan_dmdd_09b,
           eswan_dmdd_09c,
           eswan_dmdd_10a,
           eswan_dmdd_10b,
           eswan_dmdd_10c
           ,eswan_dmdd_proband_complete
           )
    
 
  
  #oeswandmdd<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/eswan_dmdd_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_eswan_dmdd,oeswandmdd,na="",row.names=F)
}

#eswan_dmdd

{
  axis_eswan_adhd<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           eswan_adhd_01,
           eswan_adhd_02,
           eswan_adhd_03,
           eswan_adhd_04,
           eswan_adhd_05,
           eswan_adhd_06,
           eswan_adhd_07,
           eswan_adhd_08,
           eswan_adhd_09,
           eswan_adhd_10,
           eswan_adhd_11,
           eswan_adhd_12,
           eswan_adhd_13,
           eswan_adhd_14,
           eswan_adhd_15,
           eswan_adhd_16,
           eswan_adhd_17,
           eswan_adhd_18,
           eswan_adhd_comments
           ,eswan_adhd_proband_complete
          
    )
  
  
  
  #oeswanadhd<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/eswan_adhd_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(axis_eswan_adhd,oeswanadhd,na="",row.names=F)
}

#motive_psq

{
  axis_motive_psq<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           motive_psq_inscan_1,
           motive_psq_inscan_2,
           motive_psq_inscan_3,
           motive_psq_inscan_4,
           motive_psq_inscan_5,
           motive_psq_inscan_6,
           motive_psq_inscan_7,
           motive_psq_inscan_8,
           motive_psq_inscan_9,
           motive_psq_inscan_10,
           motive_psq_inscan_11,
           motive_psq_inscan_12,
           motive_psq_inscan_13,
           motive_psq_inscan_14,
           motive_psq_inscan_15,
           motive_psq_inscan_16,
           motive_psq_afterscan_1,
           motive_psq_afterscan_2,
           motive_psq_afterscan_3,
           motive_psq_afterscan_4,
           motive_psq_afterscan_5,
           motive_psq_afterscan_6
           ,motive_psq_complete
         
    )

  
 # omotivepsq<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/motive_psq_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_motive_psq,omotivepsq,na="",row.names=F)
}

#mapssr

{
  axis_mapssr<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           mapssr_1,
           mapssr_2,
           mapssr_3,
           mapssr_4,
           mapssr_5,
           mapssr_6,
           mapssr_7,
           mapssr_8,
           mapssr_9,
           mapssr_10,
           mapssr_11,
           mapssr_12,
           mapssr_13,
           mapssr_14,
           mapssr_15
           ,mapssr_complete
       
    )
    
     
  
  #omapssr<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/mapssr_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_mapssr,omapssr,na="",row.names=F)
}

#lotr

{
  axis_lotr<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           lotr_1,
           lotr_2,
           lotr_3,
           lotr_4,
           lotr_5,
           lotr_6,
           lotr_7,
           lotr_8,
           lotr_9,
           lotr_10
           ,life_orientation_test_revised_lotr_complete
)
    
   
  
  #olotr<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/lotr_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_lotr,olotr,na="",row.names=F)
}

#imi_sr

{
  axis_imi_sr<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           imi_sr_interest_1,
           imi_sr_value_5,
           imi_sr_interest_4,
           imi_sr_value_4,
           imi_sr_choice_1,
           imi_sr_choice_2,
           imi_sr_interest_2,
           imi_sr_choice_5,
           imi_sr_value_1,
           imi_sr_interest_5,
           imi_sr_choice_4,
           imi_sr_value_3,
           imi_sr_choice_3,
           imi_sr_value_2,
           imi_sr_interest_3
           ,intrinsic_motivation_inventory_for_schizophrenia_r_complete
          
    )
   
  
  #oimisr<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/imi_sr_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_imi_sr,oimisr,na="",row.names=F)
}

#contesiq

{
  axis_contesiq<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           contesiq_1,
           contesiq_2,
           contesiq_3,
           contesiq_4,
           contesiq_5,
           contesiq_6,
           contesiq_7,
           contesiq_8,
           contesiq_9
           ,conte_social_interest_questionnaire_complete
           
           
    )
    
   
  
 # ocontesiq<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/contesiq_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_contesiq,ocontesiq,na="",row.names=F)
}

#bdi1a

{
  axis_bdi1a<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           bdi_1,
           bdi_2,
           bdi_3,
           bdi_4,
           bdi_5,
           bdi_6,
           bdi_7,
           bdi_8,
           bdi_9,
           bdi_10,
           bdi_11,
           bdi_12,
           bdi_13,
           bdi_14,
           bdi_15,
           bdi_16,
           bdi_17,
           bdi_18,
           bdi_19,
           bdi_19a,
           bdi_20,
           bdi_21,
           bdi_comments
           ,bdi1a_complete
           
           
           
    )
    
  #obdi1a<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/bdi1a_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(axis_bdi1a,obdi1a,na="",row.names=F)
}

#asrm

{
  axis_asrm<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           asrm_1,
           asrm_2,
           asrm_3,
           asrm_4,
           asrm_5
           ,altman_selfrating_mania_scale_asrm_complete
           
           
    )
    
     
  
 # oasrm<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/asrm_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_asrm,oasrm,na="",row.names=F)
}

#substance

{
  axis_substance<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,	
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           substance_practice_age,
           substance_practice_fam___1,
           substance_practice_fam___2,
           substance_practice_fam___3,
           substance_practice_fam___4,
           substance_practice_fam___5,
           substance_practice_fam___6,
           substance_tob_010,
           substance_tob_020___1,
           substance_tob_020___2,
           substance_tob_020___3,
           substance_tob_030,
           substance_tob_040,
           substance_tob_060,
           substance_tob_070,
           substance_tob_120,
           substance_tob_080,
           substance_tob_090,
           substance_tob_130,
           substance_tob_100,
           substance_tob_110,
           substance_tob_140,
           substance_alc_010,
           substance_alc_020,
           substance_alc_030,
           substance_alc_050,
           substance_alc_060,
           substance_alc_100,
           substance_alc_110,
           substance_alc_120,
           substance_alc_130,
           substance_alc_140,
           substance_alc_150,
           substance_alc_160,
           substance_mar_010,
           substance_mar_020,
           substance_mar_040,
           substance_mar_050,
           substance_mar_100___1,
           substance_mar_100___2,
           substance_mar_100___3,
           substance_mar_100___4,
           substance_mar_100___5,
           substance_mar_100___6,
           substance_alcexp_yn01,
           substance_alcexp_yn02,
           substance_alcexp_yn03,
           substance_alcexp_yn04,
           substance_alcexp_yn05,
           substance_alcexp_yn06,
           substance_alcexp_yn07,
           substance_alcexp_yn08,
           substance_alcexp_yn09,
           substance_alcexp_yn10,
           substance_alcexp_yn11,
           substance_alcexp_yn12,
           substance_marexp_yn01,
           substance_marexp_yn02,
           substance_marexp_yn03,
           substance_marexp_yn04,
           substance_marexp_yn05,
           substance_marexp_yn06,
           substance_marexp_yn07,
           substance_marexp_yn08,
           substance_marexp_yn09,
           substance_marexp_yn10,
           substance_marexp_yn11,
           substance_marexp_yn12,
           substance_othr_010,
           substance_othr_011,
           substance_othr_012,
           substance_othr_020,
           substance_othr_021,
           substance_othr_022,
           substance_othr_030,
           substance_othr_040,
           substance_othr_050,
           substance_othr_060,
           substance_othr_070,
           substance_othr_071,
           substance_othr_072,
           substance_othr_080,
           substance_othr_090,
           substance_othr_100,
           substance_othr_101,
           substance_othr_102,
           substance_othr_110,
           substance_othr_111,
           substance_othr_112,
           substance_othr_120,
           substance_othr_130,
           substance_othr_140,
           substance_othr_150,
           substance_othr_160,
           substance_othr_170,
           drugs_club___1,
           drugs_club___2,
           drugs_club___3,
           drugs_club___4,
           drugs_club___5,
           drugs_club___0,
           drugs_stimulants___1,
           drugs_stimulants___2,
           drugs_stimulants___3,
           drugs_stimulants___4,
           drugs_stimulants___5,
           drugs_stimulants___0,
           drugs_depressants___1,
           drugs_depressants___2,
           drugs_depressants___3,
           drugs_depressants___4,
           drugs_depressants___5,
           drugs_depressants___0,
           drugs_hallucinogens___1,
           drugs_hallucinogens___2,
           drugs_hallucinogens___3,
           drugs_hallucinogens___4,
           drugs_hallucinogens___5,
           drugs_hallucinogens___6,
           drugs_hallucinogens___7,
           drugs_hallucinogens___8,
           drugs_hallucinogens___9,
           drugs_hallucinogens___10,
           drugs_hallucinogens___0,
           drugs_opiates___1,
           drugs_opiates___2,
           drugs_opiates___3,
           drugs_opiates___4,
           drugs_opiates___5,
           drugs_opiates___0
           ,substance_complete
           
           
    )
    
    
  
  #osubstance<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/substance_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_substance,osubstance,na="",row.names=F)
}

#stai_s

{
  axis_stai_s<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           stai_q_01,
           stai_q_02,
           stai_q_03,
           stai_q_04,
           stai_q_05,
           stai_q_06,
           stai_q_07,
           stai_q_08,
           stai_q_09,
           stai_q_10,
           stai_q_11,
           stai_q_12,
           stai_q_13,
           stai_q_14,
           stai_q_15,
           stai_q_16,
           stai_q_17,
           stai_q_18,
           stai_q_19,
           stai_q_20
           ,stai_state_complete
           
           
    )
    
 
  
  #ostais<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/stai_s_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_stai_s,ostais,na="",row.names=F)
}

#stai_t

{
  axis_stai_t<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           stai_q_21,
           stai_q_22,
           stai_q_23,
           stai_q_24,
           stai_q_25,
           stai_q_26,
           stai_q_27,
           stai_q_28,
           stai_q_29,
           stai_q_30,
           stai_q_31,
           stai_q_32,
           stai_q_33,
           stai_q_34,
           stai_q_35,
           stai_q_36,
           stai_q_37,
           stai_q_38,
           stai_q_39,
           stai_q_40
           ,stai_trait_complete
           
           
           
    )
    
    
  
  #ostait<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/stai_t_", format(Sys.Date(), "%Y%m%d"),".csv")
  #write.csv(axis_stai_t,ostait,na="",row.names=F)
}

#tlx

{
  axis_tlx<- axis %>%
    filter(protocol == "motive")%>%
    select(redcapid,
           # admin_timestamp,	protocol,	protocol_number,
           # bblid,	libi_id,	scales_id,	scales_administration,	date
           # ,assessor,	location,
           tlx_mental,
           tlx_physical,
           tlx_temporal,
           tlx_performance,
           tlx_effort,
           tlx_frustration
           ,nasa_task_load_index_tlx_complete
           
           
           
           
    )
   
      
  
  #otlx<-paste0("/data/secure/lab/pi_repo/dan_wolf/motive/selfreport_scales/tlx_", format(Sys.Date(), "%Y%m%d"),".csv")
 # write.csv(axis_tlx,otlx,na="",row.names=F)
}


df_list <- list(axis_ari,axis_aes,axis_asrm,axis_bdi1a,axis_bisbas,axis_contesiq,
                axis_eswan_adhd,axis_eswan_dmdd,axis_eswan_social,axis_gcos_cp_v1,
                axis_gcos_cp_v2,axis_grit,axis_imi_sr,axis_lotr,axis_mapssr,axis_motive_psq,
                axis_panas,axis_prime_wolf,axis_rpas,axis_rsas,axis_stai_s,axis_stai_t,axis_substance,axis_swls,
                axis_tlx,axis_wolf_im_em,axis_wolf_trait)
              
selfreport_merge<-Reduce(function(d1, d2) merge(d1, d2, by = "redcapid", all.x = TRUE, all.y = TRUE), 
       df_list)


omerge<-paste0("/data/secure/bit_repo/pi_repo/dan_wolf/motive/selfreportscales_motive.csv")
 write.csv(selfreport_merge,omerge,na="",row.names=F)


 
source("/data/secure/bit_repo/scripts/common_interview_motive.R")