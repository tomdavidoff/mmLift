# getInventory.R
# R code to get 2025 inventory
# Tom Davidoff
# 05/30/25

library(data.table)
dirr <- "~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202501"
files <- list.files(dirr)
goodAreas <- c("08","09","10","11","14","15")
goodFiles <- paste0("~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202501/20250101_A",goodAreas,"_Residential_Inventory_Extract.txt")
print(goodFiles)
outfile <- data.table()
for (k in goodFiles) {
	print(k)
	df <- fread(file=k,select=c("Jurisdiction","Roll_Number","MB_Year_Built","MB_Effective_Year","Zoning"),colClasses=c("Roll_Number"="character"))
	print(summary(df))
	outfile <- rbind(outfile,df)
}

fwrite(outfile,"data/derived/inventory2025.csv")



