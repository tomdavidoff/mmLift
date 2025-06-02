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
df[,nearest:="none"]
df[,mindist:=1000000]
print(table(dt[,get("Type of Transit Station")]))
for (i in 1:nrow(dt)) {
	df[,disti:=distGeo(cbind(lon,lat),c(dt[i,Longitude],dt[i,Latitude]))]
	if (dt[i,`Type of Transit Station`]=="passenger rail station") {
		df[,maxi:=ifelse(disti<200,5,ifelse(disti<400,4,ifelse(disti<800,3,0)))]
	}
	if (dt[i,`Type of Transit Station`]=="bus exchange") {
		df[,maxi:=ifelse(disti<200,4,ifelse(disti<400,3,0))]
	}
	df[,nearest:=ifelse(maxi>maxFSR,dt[i,`Station Name`],nearest)]
	df[,mindist:=ifelse(maxi>maxFSR ,disti,mindist)]
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
print(names(df))
df <- merge(df,di,by.x=c("JURISDICTION_CODE","ROLL_NUMBER"),by.y=c("Jurisdiction","Roll_Number"),all.x=FALSE)
print(nrow(df))
print(quantile(df[singleFamily==1 & MB_Year_Built==2024,structureShare],probs=seq(.1,.95,.05)))
df[,newBuild:=MB_Year_Built==2024]
print(df[,mean(hasDepth),by=newBuild])
print(table(df[,singleFamily]))
print(table(df[,singleFamily]))
print(table(df[,JURISDICTION]))
print(table(df[maxFSR>0,JURISDICTION]))


df[,post:=1*(CONVEYANCE_DATE >= as.Date("2023-11-23"))]
df[,donut:=(CONVEYANCE_DATE > as.Date("2023-11-01") & CONVEYANCE_DATE < as.Date("2024-01-01"))]
print(table(df[,.(singleFamily,maxFSR)]))

# how many df
df[,hasmax:=max(maxFSR)>0,by=.(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE))]
df[,transit:=1*(maxFSR>0)]
print(summary(df[hasmax>0,.N,by=.(JURISDICTION,NEIGHBOURHOOD,yearmon(CONVEYANCE_DATE))]))

df[,singleFamily:=1*singleFamily]
# first just plain month coefficient

getTimeCoefs <- function(dset) {
	# run a regression with month coefficients
	r <- feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+i(yearmon(CONVEYANCE_DATE))|paste0(JURISDICTION,NEIGHBOURHOOD),data=dset)
	# extract only the month coefficients, not log assessed value, and then do a line plot
	coefs <- r$coefficients[grep("yearmon",names(r$coefficients))]
	coefs <- coefs[order(names(coefs))]
	coefs <- data.table(coef=coefs,month=names(coefs))
	coefs[,month:=substring(gsub("yearmon\\(CONVEYANCE_DATE\\)::","",month),1,7)]
	coefs[,month:=as.numeric(month)]
	return(coefs)
}
r1 <- getTimeCoefs(df[singleFamily==1])
r2 <- getTimeCoefs(df[singleFamily==0])
r1[,type:="singleFamily"]
r2[,type:="condo"]
coefs <- rbind(r1,r2)
ggplot(data=coefs,aes(x=month,y=coef,color=type)) + geom_line() + 
	geom_point() + 
	labs(title="Month Coefficients for Single Family and Condo Sales",x="Month",y="Coefficient") +
	scale_color_manual(values=c("singleFamily"="blue","condo"="red")) + # vertical line at Nov 23
	geom_vline(xintercept=2023+(11+23/31)/12, linetype="dashed", color="black") 
ggsave("text/monthCoefficients.png",width=10,height=6)

r1 <- getTimeCoefs(df[singleFamily==1 & transit==1])
r2 <- getTimeCoefs(df[singleFamily==0 & transit==1])
r1[,type:="singleFamily"]
r2[,type:="condo"]
coefs <- rbind(r1,r2)
ggplot(data=coefs,aes(x=month,y=coef,color=type)) + geom_line() + 
	geom_point() + 
	labs(title="month coefficients for single family and condo sales transit only",x="month",y="coefficient") +
	scale_color_manual(values=c("singleFamily"="blue","condo"="red")) + # vertical line at nov 23
	geom_vline(xintercept=2023+(11+23/31)/12, linetype="dashed", color="black") 
ggsave("text/monthCoefficientsTransit.png",width=10,height=6)

r1 <- getTimeCoefs(df[singleFamily==1 & teardown==1])
r2 <- getTimeCoefs(df[singleFamily==1 & structureShare>.3])
r1[,type:="teardown"]
r2[,type:="snazzy"]
coefs <- rbind(r1,r2)
ggplot(data=coefs,aes(x=month,y=coef,color=type)) + geom_line() + 
	geom_point() + 
	labs(title="month coefficients for single family and condo sales transit only",x="month",y="coefficient") +
	scale_color_manual(values=c("teardown"="blue","not"="red")) + # vertical line at nov 23
	geom_vline(xintercept=2023+(11+23/31)/12, linetype="dashed", color="black") 
ggsave("text/monthCoefficientsTeardown.png",width=10,height=6)

r1 <- getTimeCoefs(df[singleFamily==1 & teardown==1 & transit==1])
r2 <- getTimeCoefs(df[singleFamily==1 & structureShare>.3 & transit==1])
r1[,type:="teardown"]
r2[,type:="snazzy"]
coefs <- rbind(r1,r2)
ggplot(data=coefs,aes(x=month,y=coef,color=type)) + geom_line() + 
	geom_point() + 
	labs(title="month coefficients for single family and condo sales transit only",x="month",y="coefficient") +
	scale_color_manual(values=c("teardown"="blue","not"="red")) + # vertical line at nov 23
	geom_vline(xintercept=2023+(11+23/31)/12, linetype="dashed", color="black") 
ggsave("text/monthCoefficientsTeardownTransit.png",width=10,height=6)

df[,rZone:=substring(Zoning,1,1)=="R" & !is.na(Zoning)]

r1 <- getTimeCoefs(df[singleFamily==1 & teardown==1 & rZone==1])
r2 <- getTimeCoefs(df[singleFamily==1 & structureShare>.3 & rZone==1])
r1[,type:="teardown"]
r2[,type:="snazzy"]
coefs <- rbind(r1,r2)
ggplot(data=coefs,aes(x=month,y=coef,color=type)) + geom_line() + 
	geom_point() + 
	labs(title="month coefficients for single family and condo sales transit only",x="month",y="coefficient") +
	scale_color_manual(values=c("teardown"="blue","snazzy"="red")) + # vertical line at nov 23
	geom_vline(xintercept=2023+(11+23/31)/12, linetype="dashed", color="black") 
ggsave("text/monthCoefficientsTeardownR.png",width=10,height=6)



# feols reg log price on transit times single indicator times post
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+transit*post*singleFamily|paste0(JURISDICTION,NEIGHBOURHOOD),data=df[donut==0])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+post*singleFamily|nearest,data=df[transit==1 & donut==0])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+post*structureShare|nearest,data=df[transit==1 & singleFamily==1 & donut==0])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+post*teardown|nearest,data=df[transit==1 & singleFamily==1 & donut==0])))
q("no")

# limit to single family zones
df <- df[substring(Zoning,1,1)=="R" & !is.na(Zoning)]
# month by month transit coefficient
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+transit*i(yearmon(CONVEYANCE_DATE))|paste0(JURISDICTION,NEIGHBOURHOOD),data=df[singleFamily==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+singleFamily*i(yearmon(CONVEYANCE_DATE))|paste0(JURISDICTION,NEIGHBOURHOOD),data=df)))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+teardown*i(yearmon(CONVEYANCE_DATE))|paste0(JURISDICTION,NEIGHBOURHOOD),data=df[singleFamily==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue)+log(LAND_SIZE)*i(yearmon(CONVEYANCE_DATE))|paste0(JURISDICTION,NEIGHBOURHOOD),data=df[singleFamily==1 & hasDepth==1])))

q("no")
print(df[maxFSR>4,.(FOLIO_ID,mindist,nearest,lat,lon)])
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*i(maxFSR)*teardown|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1 & singleFamily==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*(maxFSR)*singleFamily|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(CONVEYANCE_PRICE ~ assessedValue + post*transit|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*transit|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ log(assessedValue) + post*transit*singleFamily|NEIGHBOURHOOD+yearmon(CONVEYANCE_DATE),data=df[JURISDICTION=="City of Vancouver" & window3==1])))
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

