library(sf)
library(tidyverse)


# Import shapefiles as an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

head(permit_df)

head(project_df)

# Generate a subset of permit_df containing just permits for new construction.

permitNew_df <- filter(permit_df, CP_IMP_TYP == 'NEW')

# Indentity permitNew_df with project_df to assign project nubers to permits

permitNewProject_df <- st_intersection(permitNew_df, project_df)





