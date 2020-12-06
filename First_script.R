# 1. Set the Environment up ####

rm(list = ls(all = TRUE))
setwd("E:/PRIDE/R/Policy-UA-vs-RO/Data")

.libPaths('E:/PRIDE/R/Library')
.libPaths()

#install.packages(c("tidyverse", "sf"))
#install.packages("lwgeom")
#install.packages("raster")

library(lwgeom)
library(sf)
library(tidyverse)
library(raster)

#NCEI <- sum(Natura2000$All_PrArea)/sum(Natura2000$area)

All_Protection <- st_read("E:/PRIDE/R/Policy-UA-vs-RO/Data/Shapefiles/all_protection_cover.shp")
All_Protection_sorted <- All_Protection
#All_Protection_sorted$category <- "Null"
All_Protection_sorted$DESIG_ENG
PA_Types <- unique(All_Protection_sorted$DESIG_ENG)
ggplot(All_Protection) + geom_sf()

# lets check the data

All_Protection %>%
  st_set_geometry(NULL) %>%
  glimpse() # And Wholly Jesus Christ, its a mess - Rows: 824; Columns: 18

# Lets intersect Protected areas over Romania

Rou_Adm <- st_read("E:/PRIDE/R/Policy-UA-vs-RO/Data/Shapefiles/ROU_adm0.shp")
#PC_Habitats_in_RO <- intersect(Rou_Adm, All_Protection_sorted)
#ggplot(PC_Habitats_in_RO) + geom_sf() # seems like it does not work, lets try something else
PC_Habitats_in_RO <- st_intersection(Rou_Adm, All_Protection_sorted)
ggplot(PC_Habitats_in_RO) + geom_sf()

# Now Lets Dissolve
PC_Habitats_in_RO$area <- st_area(PC_Habitats_in_RO)
PC_Hab_RO_Dissolved <-
  PC_Habitats_in_RO %>%
  summarise(area = sum(area))
ggplot(PC_Hab_RO_Dissolved) + geom_sf()

# Now lets calculate intersection between Natura 2000 and PC habitats in RO
All_pr <- st_read("E:/PRIDE/R/Policy-UA-vs-RO/Data/Shapefiles/all_pr_areas.shp")
ggplot(All_pr) + geom_sf()
HD <- subset(All_Protection, All_Protection$DESIG_ENG == "Site of Community Importance (Habitats Directive)")
ggplot(HD) + geom_sf()
HD$area <- st_area(HD)
HD_Dissolved <- HD %>% summarise(area = sum(area))
ggplot(HD_Dissolved) + geom_sf()

st_crs(HD_Dissolved)
st_crs(PC_Hab_RO_Dissolved)

#PC_Hab_HD_Coveragege <- (HD_Dissolved$area*100)/PC_Hab_RO_Dissolved$area # =21.5% so it didnt'work
PC_Hab_HD_Coveragege <- st_intersection(PC_Hab_RO_Dissolved, HD_Dissolved)
ggplot(PC_Hab_HD_Coveragege) + geom_sf()
PC_Hab_HD_Coveragege$area.1*100/PC_Hab_HD_Coveragege$area


# Create a fresh area variable for counties
nc <- mutate(nc, county_area = st_area(nc))

# Merge by county name
nc <- merge(nc, intersect_pct, by = "NAME", all.x = TRUE)

# Calculate coverage
nc <- nc %>% 
  mutate(coverage = as.numeric(intersect_area/county_area))


# All_Pritection Dissolved
All_Protection_sorted$area <- st_area(All_Protection_sorted)
All_Protection_sorted

PCProtection <-
  All_Protection_sorted %>%
  summarise(area = sum(area))
ggplot(PCProtection) + geom_sf()

1370099870.014*100/(1348984298.755+89972561.865) # HD 95%
1419378354.007*100/(1348984298.755+89972561.865) # BD 99%
1172581856.606*100/1220331278.231 # EMERALD 96%
390810055.325*100/1220331278.231  #National UA 32%
106143936.306*100/(1348984298.755+89972561.865) #National RO 7%
558771652.153*100/1220331278.231 # 45%
1279939379.963*100/(1348984298.755+89972561.865) #Ramsar RO 89%
700008226.269*100/1220331278.231 # RAMSAR UA # 57
390809561.243*100/1220331278.231 # Unesco UA # 32
1059652851.999*100/(1348984298.755+89972561.865)# 74%
