library(sf)
library(tidyverse)
library(lubridate)


# Import shapefiles as an R data frame

permit_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_df <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

glimpse(permit_df)

glimpse(project_df)


# Indentity permit_df with project_df to assign project numbers to permits and return just the permits that intersect polygons
# Confirm 11,691 observations

permitProject_df <- st_intersection(permit_df, project_df)

# Subset variables of intrest

permitProjectSubset_df <- permitProject_df %>%
        select(BP_NBR, CP_IMP_TYP, CP_ISSUE_D, CP_USE_TYP, PROJ_NUMBE)

# Take a glimpse at the output file

glimpse(permitProjectSubset_df)

#Plot monthly issued permits by structure type

permitPlot <- permitProjectSubset_df %>% 
    filter(CP_IMP_TYP == "NEW") %>%
      filter(CP_ISSUE_D >= "2010-01-01") %>%
        mutate(month = floor_date(CP_ISSUE_D, unit = "month")) %>%
           group_by(month, CP_USE_TYP) %>%
             tally()

  ggplot(permitPlot, aes(x = month, y = n, fill = CP_USE_TYP, color = CP_USE_TYP)) +
  geom_point()

# Gather project_df into tidy data

tidyProject_df <- project_df %>%
    select(PROJ_NUMBE, STATUS, APVD_SFD:PIPE_APT) %>%
      gather(key = "TYPE", value = "UNIT_COUNT", APVD_SFD:PIPE_APT) %>%
        separate(TYPE, into = c("UNIT_STATUS","UNIT_TYPE"))
     

# Count monthly permits issued per project number per month

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
                  filter(CP_ISSUE_D >= "2018-08-01" & CP_ISSUE_D <= "2018-08-31") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Jul2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                  filter(CP_ISSUE_D >= "2018-07-01" & CP_ISSUE_D <= "2018-07-31") %>%
                    distinct(BP_NBR, .keep_all = TRUE) %>%
                      count(PROJ_NUMBE, CP_USE_TYP)

Jun2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-06-01" & CP_ISSUE_D <= "2018-06-30") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

May2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-05-01" & CP_ISSUE_D <= "2018-05-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

April2018 <- permitProjectSubset_df %>% 
                  filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-04-01" & CP_ISSUE_D <= "2018-04-30") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

March2018 <- permitProjectSubset_df %>% 
                  filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-03-01" & CP_ISSUE_D <= "2018-03-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Feb2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-02-01" & CP_ISSUE_D <= "2018-02-28") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Jan2018 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2018-01-01" & CP_ISSUE_D <= "2018-01-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Dec2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-12-01" & CP_ISSUE_D <= "2017-12-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Nov2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-11-01" & CP_ISSUE_D <= "2017-11-30") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Oct2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-10-01" & CP_ISSUE_D <= "2017-10-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Sept2017 <- permitProjectSubset_df %>% 
                  filter(CP_IMP_TYP == "NEW") %>%
                      filter(CP_ISSUE_D >= "2017-09-01" & CP_ISSUE_D <= "2017-09-30") %>%
                          distinct(BP_NBR, .keep_all = TRUE) %>%
                            count(PROJ_NUMBE, CP_USE_TYP)

Aug2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-08-01" & CP_ISSUE_D <= "2017-08-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Jul2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-07-01" & CP_ISSUE_D <= "2017-07-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Jun2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-06-01" & CP_ISSUE_D <= "2017-06-30") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

May2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-05-01" & CP_ISSUE_D <= "2017-05-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

April2017 <- permitProjectSubset_df %>% 
                  filter(CP_IMP_TYP == "NEW") %>%
                      filter(CP_ISSUE_D >= "2017-04-01" & CP_ISSUE_D <= "2017-04-30") %>%
                          distinct(BP_NBR, .keep_all = TRUE) %>%
                            count(PROJ_NUMBE, CP_USE_TYP)

March2017 <- permitProjectSubset_df %>% 
                  filter(CP_IMP_TYP == "NEW") %>%
                      filter(CP_ISSUE_D >= "2017-03-01" & CP_ISSUE_D <= "2017-03-31") %>%
                          distinct(BP_NBR, .keep_all = TRUE) %>%
                            count(PROJ_NUMBE, CP_USE_TYP)

Feb2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-02-01" & CP_ISSUE_D <= "2017-02-28") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)

Jan2017 <- permitProjectSubset_df %>% 
                filter(CP_IMP_TYP == "NEW") %>%
                    filter(CP_ISSUE_D >= "2017-01-01" & CP_ISSUE_D <= "2017-01-31") %>%
                        distinct(BP_NBR, .keep_all = TRUE) %>%
                          count(PROJ_NUMBE, CP_USE_TYP)






