# getSales.R
# grab transaction data for metro Vancouver
# Tom Davidoff
# 05/30/25

library(data.table)

df <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_TYPE_DESCRIPTION","CONVEYANCE_PRICE"))
df[,CONVEYANCE_DATE := as.Date(substring(CONVEYANCE_DATE,1,8),format="%Y%m%d")]
print(table(df[,CONVEYANCE_TYPE_DESCRIPTION]))
df <- df[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"]
MINDATE <- as.Date("2023-01-01")
MAXDATE <- as.Date("2024-12-31")
df <- df[CONVEYANCE_DATE >= MINDATE & CONVEYANCE_DATE <= MAXDATE]
print(summary(df))
fwrite(df,"data/derived/sales.csv",row.names=FALSE)

