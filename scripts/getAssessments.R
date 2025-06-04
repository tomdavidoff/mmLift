# getAssessments.R
# R to get assessments as of 2023 mid-year
# Tom Davidoff
# 05/30/25

library(data.table)

df <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_gnrl_property_values_20240331_REVD24.csv",select=c("FOLIO_ID","GEN_NET_IMPROVEMENT_VALUE","GEN_NET_LAND_VALUE"))
fwrite(df,"data/derived/assessments.csv")
