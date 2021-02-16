## drake plan to clean and process FTICR-MS data for 2017 SBB
## Kaizad F. Patel
## Feb 16, 2021


# source relevant functions/packages --------------------------------------
source("code/0-packages.R")
source("code/1-fticrrr_processing_functions.R")


# clean input data --------------------------------------------------------
# get the input corekey in the correct format for the plan/functions
clean_corekey = function(fticr_corekey){
  fticr_corekey  %>% 
    rename(FTICR_name = `Malak.s.ID`) %>% 
    filter(is.na(notes)) %>% 
    dplyr::select(FTICR_name, PoreWater_ID, site, depth, Suction_kPa) %>% 
    force()
}

prep_fticr_file = function(report_raw){
  report_raw %>% 
    rename(Mass = peak) %>% 
    filter(C>0) %>% 
    #mutate(C13 = recode(isotopic, "FALSE" = "0", "TRUE" = "1")) %>% 
    mutate(isotopic = if_else(isotopic == FALSE, 0, 1),
           Class = NA) %>% 
    rename(C13 = isotopic,
           El_comp = ec) %>% 
    dplyr::select(-c(formula, kendrick_m, kendrick_defect, dbe, dbe_chnos, nso))
}


# plan --------------------------------------------------------------------
# this is in drake, so it will re-run only if targets need to be updated

sbb_fticr_processing_plan = 
  drake_plan(
    fticr_corekey = read.csv("data/vlb_sbb_2017_subset/sample_key.csv", na.strings = ""),
    corekey = clean_corekey(fticr_corekey),
    report_raw = read_csv("data/vlb_sbb_2017_subset/fticr_reports/fticr_data.csv"),
    report = prep_fticr_file(report_raw), 
    fticr_meta = make_fticr_meta(report)$meta2,
    
    data_columns = report %>% dplyr::select(-c(C13:P, Class)),
    fticr_data_all = make_fticr_data(report, data_columns, corekey, site, depth, Suction_kPa),
    
    fticr_meta_save = fticr_meta %>% write.csv("data/vlb_sbb_2017_subset/fticr_meta.csv"),
    fticr_data_by_trt = fticr_data_all$data_long_trt %>% write.csv("data/vlb_sbb_2017_subset/fticr_processed_by_treatment.csv", row.names = FALSE),
    fticr_data_reps = fticr_data_all$data_long_key_repfiltered %>% dplyr::select(-CoreID) %>% write.csv("data/vlb_sbb_2017_subset/fticr_processed_reps.csv", row.names = FALSE),
    
  )

make(sbb_fticr_processing_plan)
