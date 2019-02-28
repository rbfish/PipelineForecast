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

# Take a peek at the output file

glimpse(permitProject_df)

# Testing tidyverse code to count monthly permits per project number

Jan2019 <- permitProject_df %>% 
               filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2019-01-01" & CP_ISSUE_D <= "2019-01-31") %>%
                     distinct(BP_NBR, .keep_all = TRUE) %>%
                       count(PROJ_NUMBE, CP_USE_TYP)
                         
Dec2018 <- permitProject_df %>% 
               filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-12-01" & CP_ISSUE_D <= "2018-12-31") %>%
                     distinct(BP_NBR, .keep_all = TRUE) %>%
                       count(PROJ_NUMBE, CP_USE_TYP)

Nov2018 <- permitProject_df %>% 
              filter(CP_IMP_TYP == "NEW") %>%
                 filter(CP_ISSUE_D >= "2018-11-01" & CP_ISSUE_D <= "2018-11-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Oct2018 <- permitProject_df %>% 
              filter(CP_IMP_TYP == "NEW") %>%
                filter(CP_ISSUE_D >= "2018-10-01" & CP_ISSUE_D <= "2018-10-31") %>%
                  distinct(BP_NBR, .keep_all = TRUE) %>%
                    count(PROJ_NUMBE, CP_USE_TYP)

Sept2018 <- permitProject_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-09-01" & CP_ISSUE_D <= "2018-09-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Aug2018 <- permitProject_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-08-01" & CP_ISSUE_D <= "2018-08-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)






