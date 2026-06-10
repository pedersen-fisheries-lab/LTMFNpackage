## This code should be run only if these codes need to be updated with extended code values. Also
## make sure to update help files for any data file that has been changed.Runn


## Lists of valid code words to include in internal data ####

#Should match up with lists of valid code words found in the most recent version of the validation
#tab of the Excel sheet
capture_methods <- c("fyke", "angling", "castNet", "other")

capture_outcomes <- c(
  "released",
  "tagged",
  "euthenizedCapture",
  "euthenizedSurgery",
  "euthenizedRecovery",
  "mortalityCapture",
  "mortalitySurgery",
  "mortalityRecovery")

condition_codes <- c("vigorous", "normal", "weak")

deploy_types <- c("GA", "GR", "RT", "SE", "other")

equip_types <- c("VR2Tx", "VR2W", "tag", "RTtag", "VR100", "AMchl", "AMdo",  "AMsal", "GPS", "other")

equipment_actions <- c(
  "On", "Off", "deployed", "retrieved", "moved", "notLocated", "notRetrieved", "located",
  "dataDownload", "checked", "configure", "recharge",
  "anchorMid", "anchorEnd",
  "repairSent", "repairReturn",
  "damaged", "destroyed",
  "other")

fyke_actions <- c("set", "retrieved", "checked", "moved")

otn_tagging_metadata_headers <- c(
  "ANIMAL_ID", "TAG_TYPE", "TAG_MANUFACTURER", "TAG_MODEL", "TAG_SERIAL_NUMBER",
  "TAG_ID_CODE", "TAG_CODE_SPACE", "TAG_IMPLANT_TYPE", "TAG_IMPLANT_METHOD",
  "TAG_ACTIVATION_DATE", "EST_TAG_LIFE", "TAGGER", "TAG_OWNER_PI",
  "TAG_OWNER_ORGANIZATION", "COMMON_NAME_E", "SCIENTIFIC_NAME",
  "CAPTURE_LOCATION", "CAPTURE_LATITUDE", "CAPTURE_LONGITUDE",
  "WILD_OR_HATCHERY", "STOCK", "LENGTH", "WEIGHT", "LENGTH_TYPE",
  "LENGTH2", "LENGTH2_TYPE", "LIFE_STAGE", "AGE", "AGE_UNITS",
  "SEX", "DNA_SAMPLE_TAKEN", "TREATMENT_TYPE", "RELEASE_GROUP",
  "RELEASE_LOCATION", "RELEASE_LATITUDE", "RELEASE_LONGITUDE",
  "UTC_RELEASE_DATE_TIME", "HARVEST_DATE", "CAPTURE_DEPTH", "TEMPERATURE_CHANGE",
  "HOLDING_TEMPERATURE", "PREOP_HOLD_PERIOD", "POSTOP_HOLD_PERIOD",
  "SURGERY_LOCATION", "DATE_OF_SURGERY", "SURGERY_LATITUDE", "SURGERY_LONGITUDE",
  "SEDATIVE", "SEDATIVE_CONCENTRATION", "ANAESTHETIC", "BUFFER",
  "ANAESTHETIC_CONCENTRATION", "BUFFER_CONCENTRATION_IN_ANAESTHETIC",
  "ANAESTHETIC_CONCENTRATION_IN_RECIRCULATION", "BUFFER_CONCENTRATION_IN_RECIRCULATION",
  "DISSOLVED_OXYGEN", "COMMENTS"
)

otn_instrument_deployment_headers <- c(
  "OTN_ARRAY", "STATION_NO", "DEPLOY_DATE_TIME", "DEPLOY_LAT",
  "DEPLOY_LONG", "BOTTOM_DEPTH", "RISER_LENGTH", "INSTRUMENT_DEPTH",
  "INS_MODEL_NO", "INS_SERIAL_NO", "CODE_SET", "TRANSMITTER", "TRANSMIT_MODEL",
  "AR_MODEL_NO", "AR_SERIAL_NO", "DEPLOYED_BY", "RECOVERED", "RECOVER_DATE_TIME",
  "RECOVER_LAT", "RECOVER_LONG", "DATA_DOWNLOADED", "DOWNLOAD_DATE_TIME",
  "FILENAME", "COMMENTS"
)

rt_types <- c("boat", "fixed_st", "fixed_lt")

sex_options <- c("MM", "IM", "MF", "IF", "unk")

sites <- c(
  "JEA", "JWE", "JWA", "JWH", "JGR", "JMA", "JRU", "JOY",
  "LEA", "LWE","LSW","base","lab", "other"
  )

species_codes <- c(
  "COAR", "COCL", "ESLU",
  "MOHU", "MOSA", "OSMO", "PEFL",
  "SAFO", "SANA", "SAVI", "TITI",
  "other")

tag_models <- c("V6", "V7", "V8", "V9", "V13", "V16")


## Write all valid codes to the data folder ####
usethis::use_data(
  capture_methods, capture_outcomes, condition_codes,
  deploy_types, equip_types, equipment_actions, fyke_actions,
  otn_tagging_metadata_headers, rt_types, sex_options, sites, species_codes, tag_models,
  overwrite = TRUE
  )
