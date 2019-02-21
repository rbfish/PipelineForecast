library(sf)
library(tidyverse)


# Import shapefiles as an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

head(permit_df)

head(project_df)


# Indentity permit_df with project_df to assign project numbers to permits and return just the permits that intersect polygons
# Confirm 10,400 observations

permitProject_df <- st_intersection(permit_df, project_df)

# Testing tidyverse code to count permits per project number

test1 <- permitProject_df %>% filter(CP_IMP_TYP == "NEW") %>% count(PROJ_NUMBE, CP_USE_TYP)






