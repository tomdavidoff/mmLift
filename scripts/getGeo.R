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

dg <- sf::st_read(df, query = "SELECT FOLIO_ID, STREET_NUMBER, STREET_NAME, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV")
print(dg[1:10, ])

# convert to standard lat_lon
# set the crs to 3005
# and then transform to WGS84 (4326)
# assuming the original CRS is 3005

dg <- st_set_crs(dg, 3005) # set the CRS to 3005
dg <- st_transform(dg, crs = 4326) # assuming the original CRS is 3005
print(dg[1:20, ])
# get centroids of geometry and create a data.table with the folioid and the centroid coordinates of the geom variable
centroids <- st_centroid(st_make_valid(dg))
centroids_dt <- data.table(
  FOLIO_ID = centroids$FOLIO_ID,
  lon = st_coordinates(centroids)[, 1],
  lat = st_coordinates(centroids)[, 2],
  street_number = centroids$STREET_NUMBER,
  street_name = centroids$STREET_NAME
)
print(centroids_dt[1:10, ])

fwrite(centroids_dt, "data/derived/folioCentroids.csv")
q("no")
