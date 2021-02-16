## drake plan to clean and process FTICR-MS data for 3Soils
## Kaizad F. Patel
## Feb 16, 2021


# source relevant functions/packages --------------------------------------
source("code/0-packages.R")
source("code/1-fticrrr_processing_functions.R")


# clean input data --------------------------------------------------------
# get the input corekey in the correct format for the plan/functions

clean_corekey = function(fticr_corekey){
  fticr_corekey  %>% 
    filter(Sample_Type %in% c("as", "sample")) %>% 
    rename(FTICR_name = 1) %>%
    dplyr::select(FTICR_name, Site, CoreNo, Treatment, Suction) %>% 
    mutate(Treatment = recode(Treatment, "Field Moisture Incubation" = "field moist", "Drought Incubation" = "drought",
                              "Saturation Incubation" = "flood", "Time Zero Saturation" = "time zero")) %>% 
    force()
}


# processing plan ---------------------------------------------------------
# this is in drake, so it will re-run only if targets need to be updated

threesoils_fticr_processing_plan = 
  drake_plan(
    fticr_corekey = read_excel("data/threesoils/fticr_reports/FTICR_INPUT_SOILPORE_meta.xlsx"),
    corekey = clean_corekey(fticr_corekey),
    report = read_csv("data/threesoils/fticr_reports/FTICR_INPUT_SOILPORE.csv.zip"),
    fticr_meta = make_fticr_meta(report)$meta2,
    
    data_columns = report %>% dplyr::select(Mass, starts_with("21T_")),
    fticr_data_all = make_fticr_data(report, data_columns, corekey, Site, Treatment, Suction),
    
    fticr_meta_save = fticr_meta %>% write.csv("data/threesoils/fticr_meta.csv"),
    fticr_data_by_trt = fticr_data_all$data_long_trt %>% write.csv("data/threesoils/fticr_processed_by_treatment.csv", row.names = FALSE),
    fticr_data_reps = fticr_data_all$data_long_key_repfiltered %>% dplyr::select(-CoreID) %>% write.csv("data/threesoils/fticr_processed_reps.csv", row.names = FALSE),
  
  )

make(threesoils_fticr_processing_plan)
