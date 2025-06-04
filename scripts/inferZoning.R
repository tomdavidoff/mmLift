# inferZoning.R
# infer zoning from what gets built in minimal window around September, 2023
# Tom Davidoff
# 06/02/25

library(data.table)
d0 <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/REVD18_and_inventory_extracts_CSV_files/residentialInventory.csv",select=c("jurisdiction","roll_number","zoning"))
d0[,roll_start:=substring(roll_number,1,(nchar(roll_number)-1))] # 3 in Vancouver but this should be same street address
# only keep if last digit in roll end = 0, no matter string length
d0 <- d0[substring(roll_number,nchar(roll_number),nchar(roll_number))=="0",]


df <- fread("data/derived/descriptions.csv",select=c("JURISDICTION_CODE","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION"))
print(table(nchar(df[,ROLL_NUMBER])))
print(summary(df[JURISDICTION_CODE==200,nchar(ROLL_NUMBER)]))

di <- fread("data/derived/inventory2024.csv",select=c("Jurisdiction","Roll_Number","MB_Year_Built"))


d1 <- merge(df,di,by.x=c("JURISDICTION_CODE","ROLL_NUMBER"),by.y=c("Jurisdiction","Roll_Number"),all.x=TRUE)
d1[,roll_start:=substring(ROLL_NUMBER,1,(nchar(ROLL_NUMBER)-1))] # 3 in Vancouver but this should be same street address
d1 <- unique(d1[,.(JURISDICTION_CODE,roll_start,ACTUAL_USE_DESCRIPTION,MB_Year_Built)])

dz <- merge(d0,d1,by.x=c("jurisdiction","roll_start"),by.y=c("JURISDICTION_CODE","roll_start"),all.x=TRUE)
dz[,singleFamily := ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling","Residential Dwelling with Suite")]
print(table(dz[,jurisdiction]))

dzo <- dz[,.(singleShare=weighted.mean(singleFamily,na.rm=TRUE,weight=MB_Year_Built)),by=c("jurisdiction","zoning")]
print(summary(dzo))

fwrite(dzo,"data/derived/inferZoning.csv",row.names=FALSE)

