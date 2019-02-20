library(sf)
library(tidyverse)


# Import shapefiles as an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

head(permit_df)

head(project_df)


# Indentity permitNew_df with project_df to assign project numbers to permits and return just the permits that intersect polygons

permitProject_df <- st_intersection(permit_df, project_df)






