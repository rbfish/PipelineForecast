library(sf)
library(tidyverse)


# Import shapefiles into an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

head(permit_df)

head(project_df)

