library(sf)
library(tidyverse)


# Import shapefiles as an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

head(permit_df)

head(project_df)


# Indentity permit_df with project_df to assign project numbers to permits and return just the permits that intersect polygons
# Confirm 11,691 observations

permitProject_df <- st_intersection(permit_df, project_df)

# Subset variables of intrest

permitProjectSubset_df <- permitProject_df %>%
        select(BP_NBR, CP_IMP_TYP, CP_ISSUE_D, CP_USE_TYP, PROJ_NUMBE)

# Take a peek at the output file

glimpse(permitProjectSubset_df)

# Gather project_df into tidy data

tidyProject_df <- project_df %>%
    gather(key = "TYPE", value = "ApprovedUnits", APVD_SFD:APVD_APT) %>%
      gather(key = "TYPE", value = "IssuedUnits", ISSD_SFD:ISSD_APT) %>%
        gather(key = "TYPE", value = "PipelineUnits", PIPE_SFD:PIPE_APT)


# Testing tidyverse code to count monthly permits per project number per month

Feb2019 <- permitProjectSubset_df %>% 
               filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2019-02-01" & CP_ISSUE_D <= "2019-02-28") %>%
                     distinct(BP_NBR, .keep_all = TRUE) %>%
                       count(PROJ_NUMBE, CP_USE_TYP)

Jan2019 <- permitProjectSubset_df %>% 
               filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2019-01-01" & CP_ISSUE_D <= "2019-01-31") %>%
                      distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)
                         
Dec2018 <- permitProjectSubset_df %>% 
               filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-12-01" & CP_ISSUE_D <= "2018-12-31") %>%
                     distinct(BP_NBR, .keep_all = TRUE) %>%
                       count(PROJ_NUMBE, CP_USE_TYP)

Nov2018 <- permitProjectSubset_df %>% 
              filter(CP_IMP_TYP == "NEW") %>%
                 filter(CP_ISSUE_D >= "2018-11-01" & CP_ISSUE_D <= "2018-11-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Oct2018 <- permitProjectSubset_df %>% 
              filter(CP_IMP_TYP == "NEW") %>%
                filter(CP_ISSUE_D >= "2018-10-01" & CP_ISSUE_D <= "2018-10-31") %>%
                  distinct(BP_NBR, .keep_all = TRUE) %>%
                    count(PROJ_NUMBE, CP_USE_TYP)

Sept2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-09-01" & CP_ISSUE_D <= "2018-09-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Aug2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-08-01" & CP_ISSUE_D <= "2018-08-30") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)






