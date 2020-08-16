library(sf)
library(tidyverse)
library(lubridate)


# Import shapefiles as an R data frame

permit_sf <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialBuildingPermit.shp')

project_sf <- read_sf('C:/RStats/PipelineForecast/Data/ResidentialDevelopmentActivity.shp')

# Verify data has been correctly loaded into R

glimpse(permit_sf)
st_crs(permit_sf)

glimpse(project_sf)
st_crs(project_sf)


# Intersect permit_sf with project_sf to assign project numbers to permits and return just the permits that intersect polygons

permitProject_sf <- st_intersection(permit_sf, project_sf)

                                    
# Remove geometry from layers

permit_df <- st_set_geometry(permit_sf, NULL)
project_df <- st_set_geometry(project_sf, NULL)
permitProject_df <- st_set_geometry(permitProject_sf, NULL)


# Subset variables of intrest from data frame

permitProjectSubset_df <- permitProject_df %>%
  select(BP_NBR, CP_IMP_TYP, CP_ISSUE_D, CP_USE_TYP, PROJ_NUMBE)

# Take a glimpse of the output file

glimpse(permitProjectSubset_df)


# Filter/aggrigate monthly issued permits by structure type

permitPlot <- permit_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2009-01-01") %>%
  mutate(month = floor_date(CP_ISSUE_D, unit = "month")) %>%
  group_by(month, CP_USE_TYP) %>%
  tally()

# Plot the data by strucutre type

ggplot(permitPlot, aes(x = month, y = n, fill = CP_USE_TYP, color = CP_USE_TYP)) +
  geom_point() + geom_line() +
  facet_wrap(~ CP_USE_TYP, ncol = 4)
  

# Gather project_df into tidy data

tidyProject_df <- project_df %>%
  select(PROJ_NUMBE, STATUS, APVD_SFD:PIPE_APT) %>%
  gather(key = "TYPE", value = "UNIT_COUNT", APVD_SFD:PIPE_APT) %>%
  separate(TYPE, into = c("UNIT_STATUS","UNIT_TYPE"))
     

# Count monthly permits issued per project number per month back to January 2017

Apr2019 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2019-04-01" & CP_ISSUE_D <= "2019-04-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Mar2019 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2019-03-01" & CP_ISSUE_D <= "2019-03-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Feb2019 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2019-02-01" & CP_ISSUE_D <= "2019-02-28") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jan2019 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2019-01-01" & CP_ISSUE_D <= "2019-01-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")
                         
Dec2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-12-01" & CP_ISSUE_D <= "2018-12-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Nov2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-11-01" & CP_ISSUE_D <= "2018-11-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Oct2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-10-01" & CP_ISSUE_D <= "2018-10-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Sept2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-09-01" & CP_ISSUE_D <= "2018-09-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Aug2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-08-01" & CP_ISSUE_D <= "2018-08-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jul2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-07-01" & CP_ISSUE_D <= "2018-07-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jun2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-06-01" & CP_ISSUE_D <= "2018-06-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

May2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-05-01" & CP_ISSUE_D <= "2018-05-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

April2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-04-01" & CP_ISSUE_D <= "2018-04-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

March2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-03-01" & CP_ISSUE_D <= "2018-03-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Feb2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-02-01" & CP_ISSUE_D <= "2018-02-28") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jan2018 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2018-01-01" & CP_ISSUE_D <= "2018-01-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Dec2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-12-01" & CP_ISSUE_D <= "2017-12-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Nov2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-11-01" & CP_ISSUE_D <= "2017-11-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Oct2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-10-01" & CP_ISSUE_D <= "2017-10-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Sept2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-09-01" & CP_ISSUE_D <= "2017-09-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Aug2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-08-01" & CP_ISSUE_D <= "2017-08-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jul2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-07-01" & CP_ISSUE_D <= "2017-07-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jun2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-06-01" & CP_ISSUE_D <= "2017-06-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

May2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-05-01" & CP_ISSUE_D <= "2017-05-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

April2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-04-01" & CP_ISSUE_D <= "2017-04-30") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

March2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-03-01" & CP_ISSUE_D <= "2017-03-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Feb2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-02-01" & CP_ISSUE_D <= "2017-02-28") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")

Jan2017 <- permitProjectSubset_df %>% 
  filter(CP_IMP_TYP == "NEW") %>%
  filter(CP_ISSUE_D >= "2017-01-01" & CP_ISSUE_D <= "2017-01-31") %>%
  distinct(BP_NBR, .keep_all = TRUE) %>%
  count(PROJ_NUMBE, CP_USE_TYP) %>%
  mutate(UNIT_STATUS = "ISSD")


# Join tidyProject_df with monthly issued permits and convert unmatched rows in column n from NA's to zeros

Feb2019Pipeline <- tidyProject_df %>%
  left_join(Feb2019, by = c("PROJ_NUMBE", "UNIT_STATUS", "UNIT_TYPE" = "CP_USE_TYP")) %>%
  mutate(n = as.numeric(as.character(n))) %>%
  #mutate_if(is.numeric, coalesce, 0)
  mutate_if(is.numeric, replace_na, replace = 0) # Replaced above commented out code after it returned an error with dplyr update to 0.8.1.




