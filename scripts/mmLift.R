# mmLift.R
# R to model effect of upzoning on relative price of land and improvements
# Tom Davidoff

library(data.table)
library(ggplot2)
library(fixest)
library(geosphere)

ds <- fread("data/derived/sales.csv")
dd <- fread("data/derived/descriptions.csv")
da <- fread("data/derived/assessments.csv")
dc <- fread("data/derived/folioCentroids.csv")

df <- merge(ds,dd,by="FOLIO_ID")
df <- merge(df,da,by="FOLIO_ID")
df <- merge(df,dc,by="FOLIO_ID")

# find distance to transit
dt <- fread("data/derived/todList.csv")
print(summary(dt))
df[,maxFSR:=0]
print(table(dt[,get("Type of Transit Station")]))
for (i in 1:nrow(dt)) {
	df[,disti:=distGeo(cbind(lon,lat),c(dt[i,Longitude],dt[i,Latitude]))]
	if (dt[i,`Type of Transit Station`]=="passenger rail station") {
		df[,maxi:=ifelse(disti<200,5,ifelse(disti<400,4,ifelse(disti<800,3,0)))]
	}
	if (dt[i,`Type of Transit Station`]=="bus exchange") {
		df[,maxi:=ifelse(disti<200,4,ifelse(disti<400,3,0))]
	}
	df[,maxFSR:=pmax(maxFSR,maxi,na.rm=TRUE)]
}	
print(summary(df[,maxFSR]))

print(df[1:20])
print(summary(df))
df[,structureShare:=(GEN_NET_IMPROVEMENT_VALUE/(GEN_NET_IMPROVEMENT_VALUE+GEN_NET_LAND_VALUE))]
print(summary(df[,structureShare]))
df[,teardown:=structureShare<0.05] # per merge natural value
df[,assessedValue:=(GEN_NET_IMPROVEMENT_VALUE+GEN_NET_LAND_VALUE)]
tearDown <- data.table(yearmon=date(),coef=numeric())

# conjecture about data structure: regular lots have land and depth
df[,hasDepth:=(!is.na(LAND_DEPTH) & !is.na(LAND_WIDTH))]
print(summary(feols(structureShare ~ hasDepth | paste0(JURISDICTION,NEIGHBOURHOOD),data=df)))
print(summary(feols(teardown ~ hasDepth | paste0(JURISDICTION,NEIGHBOURHOOD),data=df)))

df[,window3:=CONVEYANCE_DATE >= as.Date("2023-08-01") & CONVEYANCE_DATE <= as.Date("2024-02-01") & yearmon(CONVEYANCE_DATE)!= yearmon("2023-11-01")]

for (m in sort(unique(df[,yearmon(CONVEYANCE_DATE)]))) {
	print(m)
	reg <- feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+teardown| JURISDICTION ^ NEIGHBOURHOOD,data=df[yearmon(CONVEYANCE_DATE)==m & hasDepth==1])
	tearDown <- rbind(tearDown,data.table(yearmon=m,coef=coef(reg)["teardownTRUE"]))
}
print(tearDown)
df[is.na(LAND_SIZE),LAND_SIZE:=LAND_WIDTH*LAND_DEPTH]
df[,smallLot:=LAND_WIDTH<30 | LAND_DEPTH<100]

# find tear-downs?
di <- fread("data/derived/inventory2025.csv")
print(nrow(df))
df <- merge(df,di,by.x=c("JURISDICTION_CODE","ROLL_NUMBER"),by.y=c("Jurisdiction","Roll_Number"),all.x=FALSE)
print(nrow(df))
print(quantile(df[MB_Year_Built==2024,structureShare],probs=seq(.1,.95,.05)))
df[,newBuild:=MB_Year_Built==2024]
print(df[,mean(hasDepth),by=newBuild])
df <- df[substring(Zoning,1,1)=="R" & !is.na(Zoning)]
print(table(df[,JURISDICTION]))
print(table(df[maxFSR>0,JURISDICTION]))


df[,post:=CONVEYANCE_DATE >= as.Date("2023-11-01")]
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*i(maxFSR)*teardown|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*i(maxFSR)|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*(maxFSR>3)|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*(maxFSR>0)|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*(maxFSR>0)|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*structureShare|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[maxFSR>2 & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[maxFSR>2 & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*structureShare|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[maxFSR>2 & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[maxFSR>2])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD) + yearmon(CONVEYANCE_DATE),data=df[maxFSR>2])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[hasDepth==1 & smallLot==0 & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[hasDepth==1 & smallLot==0 & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[hasDepth==1 & smallLot==0])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[hasDepth==0 | smallLot==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown*smallLot|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df[hasDepth==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*teardown*LAND_SIZE|paste0(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE)),data=df)))

