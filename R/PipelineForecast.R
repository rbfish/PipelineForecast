library(arcgisbinding)
library(sf)
library(tidyverse)

# Check connection to ArcGIS Pro
arc.check_product()

# Import data sets from geodatabase into R using ArcGIS Bridge

permit_df <- arc.open(path = 'C:/RStats/PipelineForecast/Data/Pipeline.gdb/ResidentialBuildingPermit')

project_df <- arc.open(path = 'C:/RStats/PipelineForecast/Data/Pipeline.gdb/ResidentialDevelopmentActivity')

# Verify data has been correctly loaded into R

str(permit_df)

str(project_df)
