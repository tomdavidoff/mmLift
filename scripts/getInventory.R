# getInventory.R
# R code to get 2024 inventory
# Tom Davidoff
# 05/30/25

library(data.table)
dirr <- "~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202401/"
files <- list.files(dirr)
onlyVancouver <- 0 
if (onlyVancouver) {
	goodAreas <- c("08","09","10","11","14","15")
	goodFiles <- paste0("~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202401/20240101_A",goodAreas,"_Residential_Inventory_Extract.txt")
} else {
	goodFiles <- paste0(dirr,files)
}
print(goodFiles)
outfile <- data.table()
for (k in goodFiles) {
	print(k)
	df <- fread(file=k,select=c("Jurisdiction","Roll_Number","MB_Year_Built","MB_Effective_Year","Zoning","MB_Manual_Class"),colClasses=c("Roll_Number"="character"))
	print(summary(df))
	outfile <- rbind(outfile,df)
}

fwrite(outfile,"data/derived/inventory2024.csv")



