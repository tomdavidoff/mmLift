# getGeo.R
# R to get postal code 
# Tom Davidoff
# 05/30/25

library(data.table)
library(RSQLite)
library(sf)

# connect to file foo.db
df <- dbConnect(SQLite(), dbname = "~/OneDrive - UBC/Documents/data/bca/bca_folios.gpkg.gpkg")
# read the following table with sf
# WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV
# and print the first 10 rows
# get the data from the database, select only two columns, folioid and geom

dg <- sf::st_read(df, layer = "WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV", query = "SELECT folioid, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV"
print(dg[1:10, ])

# convert to standard lat_lon
# set the crs to 3005
# and then transform to WGS84 (4326)
# assuming the original CRS is 3005

dg <- st_as_sf(dg, crs = 3005) # assuming x and y are the coordinate columns
dg <- st_transform(dg, crs = 4326) # assuming the original CRS is 3005

q("no")

dg <- dbGetQuery(df, "SELECT * FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV")
print(dg)
# disconnect from db
dbDisconnect(df)

#print(names(df))
#print(df)

# the following line is the CRS of the data
# 3        NAD83 / BC Albers   3005         EPSG                     3005
print(summary(dg))
g <- st_as_sf(dg, crs = 3005) # assuming x and y are the coordinate columns


(dg)

sf_data <- st_read(df)
# convert to WGS84
print(summary(sf_data))
sf_data_wgs84 <- st_transform(sf_data, crs = 4326)
print(summary(sf_data_wgs84))
# write to a new file

