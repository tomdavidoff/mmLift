# getDescriptions.R
# R to get file descriptions from BCA data 2023 assessed, 2024 published
# Tom Davidoff
# 05/30/25

library(data.table)

df <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("ACTUAL_USE_DESCRIPTION","FOLIO_ID","JURISDICTION_CODE","JURISDICTION","LAND_WIDTH","LAND_SIZE","LAND_DEPTH","NEIGHBOURHOOD","REGIONAL_DISTRICT","TENURE_DESCRIPTION"))

df <- df[REGIONAL_DISTRICT=="Metro Vancouver"]
df <- df[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite")]

fwrite(df,"data/derived/descriptions.csv")
