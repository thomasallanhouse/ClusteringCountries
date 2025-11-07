#################################################################
#  This script is needed to produce a visualisation             #
#  of the clustering                                            #
#                                                               #
#                                                               # 
#  Author: Leonardo Gada                                        #   
#                                                               # 
#################################################################




######################## Load Libraries #######################################

library(tidyverse)
library(maps)
library(sf)

#################### Data Import and Manipulation #############################

# Use map package to download world map
world_coordinates <- map_data("world")

# CLUSTER SUBDIVISION v0 (Euclidean + CART + static tree cutting)
# (1) Bangladesh, Cambodia, Djibouti, Indonesia
# (2) Azerbaijan, Egypt, Iraq, Turkey, Vietnam, Thailand
# (3) Canada
# (4) China

world_coordinates <- world_coordinates %>% 
  mutate(
    ClusterID_v0 = case_when(
      region %in% c("Bangladesh", "Cambodia", "Djibouti", "Indonesia") ~ "Cluster1",
      region %in% c("Azerbaijan", "Egypt", "Iraq", "Turkey", "Vietnam", "Thailand") ~ "Cluster2",
      region %in% c("Canada") ~ "Cluster3",
      region %in% c("China")  ~ "Cluster4",
    )
    )

# CLUSTER SUBDIVISION v1 (Manhattan + CART + dynamic tree cutting)
# (1) Bangladesh, Cambodia, Djibouti, Indonesia, China, Vietnam, Thailand
# (2) Azerbaijan, Egypt, Iraq, Turkey
# (3) Canada

world_coordinates <- world_coordinates %>% 
  mutate(
    ClusterID_v1 = case_when(
      region %in% c("Bangladesh", "Cambodia", "Djibouti", 
                    "Indonesia", "China", "Vietnam", "Thailand") ~ "Cluster1",
      region %in% c("Azerbaijan", "Egypt", "Iraq", "Turkey") ~ "Cluster2",
      region %in% c("Canada") ~ "Cluster3"
    )
  )

# CLUSTER SUBDIVISION v1 (Manhattan + CART + dynamic tree cutting)
# WHOLE WORLD MAP (not tracker/line-list countries only)

# Import Sheet with clustering

clustered_world <- readxl::read_excel("/insert path to file/CART_Clusters_1.xlsx", sheet = "Sheet2")

clustered_world_og <- clustered_world

# To make sure the joining works, we need to make sure country names match the world coordinates

#clustered_world$Country[clustered_world$Country %in% c("Hong Kong SAR, China", "Macao SAR, China")] <- "China"
clustered_world$Country[clustered_world$Country %in% c("Korea, Dem. People's Rep.")] <- "North Korea"
clustered_world$Country[clustered_world$Country %in% c("Korea, Rep.")] <- "South Korea"
clustered_world$Country[clustered_world$Country %in% c("Russian Federation")] <- "Russia"
clustered_world$Country[clustered_world$Country %in% c("United States")] <- "USA"
clustered_world$Country[clustered_world$Country %in% c("United Kingdom")] <- "UK"
clustered_world$Country[clustered_world$Country %in% c("Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
clustered_world$Country[clustered_world$Country %in% c("Congo, Rep.")] <- "Republic of Congo"
clustered_world$Country[clustered_world$Country %in% c("Cote d'Ivoire")] <- "Ivory Coast"
clustered_world$Country[clustered_world$Country %in% c("Gambia, The")] <- "Gambia"
clustered_world$Country[clustered_world$Country %in% c("Lao PDR")] <- "Laos"
clustered_world$Country[clustered_world$Country %in% c("Micronesia, Fed. Sts.")] <- "Micronesia"
#clustered_world$Country[clustered_world$Country %in% c("Sint Maarten (Dutch part)","St. Martin (French part)")] <- "Saint Martin"
clustered_world$Country[clustered_world$Country %in% c("Viet Nam")] <- "Vietnam"
clustered_world$Country[clustered_world$Country %in% c("Yemen, Rep.")] <- "Yemen"
clustered_world$Country[clustered_world$Country %in% c("Bahamas, The")] <- "Bahamas"
clustered_world$Country[clustered_world$Country %in% c("Brunei Darussalam")] <- "Brunei"
clustered_world$Country[clustered_world$Country %in% c("Cabo Verde")] <- "Cape Verde"
clustered_world$Country[clustered_world$Country %in% c("Czechia")] <- "Czech Republic"
clustered_world$Country[clustered_world$Country %in% c("Egypt, Arab Rep.")] <- "Egypt"
clustered_world$Country[clustered_world$Country %in% c("Iran, Islamic Rep.")] <- "Iran"
clustered_world$Country[clustered_world$Country %in% c("Kyrgyz Republic")] <- "Kyrgyzstan"
clustered_world$Country[clustered_world$Country %in% c("Slovak Republic")] <- "Slovakia"
clustered_world$Country[clustered_world$Country %in% c("Syrian Arab Republic")] <- "Syria"
clustered_world$Country[clustered_world$Country %in% c("Eswatini")] <- "Swaziland"
clustered_world$Country[clustered_world$Country %in% c("Turkiye")] <- "Turkey"
clustered_world$Country[clustered_world$Country %in% c("Venezuela, RB")] <- "Venezuela"
#clustered_world$Country[clustered_world$Country %in% c("Virgin Islands (U.S.)", "British Virgin Islands")] <- "Virgin Islands"
clustered_world$Country[clustered_world$Country %in% c("West Bank and Gaza")] <- "Palestine"

#world_coordinates$region[world_coordinates$region %in% c("St. Vincent", "Grenadines")] <- "St. Vincent and the Grenadines"
#world_coordinates$region[world_coordinates$region %in% c("Trinidad", "Tobago")] <- "Trinidad and Tobago"


# Join clustering from algorithm to world coordinates data

world_coordinates_clust <- left_join(world_coordinates, clustered_world, by = c("region" = "Country"))


############################# Create world map ################################

ggplot() +  
  geom_map(data = world_coordinates, 
           map = world_coordinates, aes(long, lat, map_id = region, fill = ClusterID_v0)) +
  scale_fill_manual(values = c("#2a6ebb", "#e37222", "#7d5cc6", "#f0ab00")) +
  theme_minimal() +
  ggtitle("Clustering",
          subtitle = "Euclidean, CART, static pruning") +
  guides(fill=guide_legend(title=""))
  

ggplot() +  
  geom_map(data = world_coordinates, 
           map = world_coordinates, aes(long, lat, map_id = region, fill = ClusterID_v1)) +
  scale_fill_manual(values = c("#2a6ebb", "#e37222","#7d5cc6")) +
  theme_minimal() +
  ggtitle("Clustering",
          subtitle = "Manhattan, CART, dynamic pruning") +
  guides(fill=guide_legend(title=""))

ggplot() +  
  geom_map(data = world_coordinates_clust, 
           map = world_coordinates_clust, aes(long, lat, map_id = region, fill = Cluster)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("All-World Clustering",
          subtitle = "Manhattan, CART, dynamic pruning") +
  guides(fill=guide_legend(title="")) 

# the rows like Virgin Islands for US and British, St Martin, Macao/Hong Kong mess up the clustering bc of many to many in join
# 
# for some reason:
# - French Guyana is not recognised as its own country
# - Western Sahara is considered a disputed territory
# 
# There's also an amount of smaller islands that do not appear under World Bank data

######################### Attempt with World Bank Polygons #####################


world_poly <- read_sf("insert path to file/WB_countries_Admin0_10m.shp")
  
clustered_world <- readxl::read_excel("insert path to file/CART_Clusters_1.xlsx", sheet = "Sheet2")
    
# To make sure the joining works, we need to make sure country names match the world coordinates

clustered_world$Country[clustered_world$Country %in% c("United States")] <- "United States of America"
clustered_world$Country[clustered_world$Country %in% c("Czechia")] <- "Czech Republic"
clustered_world$Country[clustered_world$Country %in% c("Congo, Dem. Rep.")] <- "Congo, Democratic Republic of"
clustered_world$Country[clustered_world$Country %in% c("Congo, Rep.")] <- "Congo, Rep. of"
clustered_world$Country[clustered_world$Country %in% c("Cote d'Ivoire")] <- "Côte d'Ivoire"
clustered_world$Country[clustered_world$Country %in% c("Eswatini")] <- "eSwatini"
clustered_world$Country[clustered_world$Country %in% c("Venezuela, RB")] <- "Venezuela, Republica Bolivariana de"
clustered_world$Country[clustered_world$Country %in% c("Yemen, Rep.")] <- "Yemen, Republic of"
clustered_world$Country[clustered_world$Country %in% c("Greenland")] <- "Greenland (Den.)"
clustered_world$Country[clustered_world$Country %in% c("Egypt, Arab Rep.")] <- "Egypt, Arab Republic of"
clustered_world$Country[clustered_world$Country %in% c("Turkiye")] <- "Turkey"
clustered_world$Country[clustered_world$Country %in% c("Iran, Islamic Rep.")] <- "Iran, Islamic Republic of"
clustered_world$Country[clustered_world$Country %in% c("Viet Nam")] <- "Vietnam"
clustered_world$Country[clustered_world$Country %in% c("Korea, Dem. People's Rep.")] <- "Korea, Democratic People's Republic of"
clustered_world$Country[clustered_world$Country %in% c("Korea, Rep.")] <- "Korea, Republic of"
clustered_world$Country[clustered_world$Country %in% c("Lao PDR")] <- "Lao People's Democratic Republic"

  
world_poly_clusters <- left_join(world_poly, clustered_world, by = c("WB_NAME" = "Country"))
world_poly_clusters <- world_poly_clusters %>% 
  select(c(OBJECTID, LEVEL, TYPE, FORMAL_EN, SUBREGION, NAME_EN, WB_NAME, WB_RULES, Cluster, Shape_Leng, Shape_Area, geometry))

world_poly_clusters$Cluster <- as.factor(world_poly_clusters$Cluster) 
  
world_poly_clusters %>%
  ggplot(aes(fill = Cluster)) +
  scale_fill_viridis_d() +
  geom_sf() +
  theme_minimal()


















