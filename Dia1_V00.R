
# INICIO ------------------------------------------------------------------

rm(list = ls())
graphics.off()
opar <- par()

# LIBRERIAS ---------------------------------------------------------------

library(arm)
library(car)
library(corrplot)
library(FRK)
library(gghighlight)
library(ggplot2)
library(ggmap)
library(GISTools)
library(gridExtra)
library(gstat)
library(jtools)
library(kableExtra)
library(knitr)
library(lme4)
library(lmtest)
library(lubridate)
library(MASS)
library(merTools)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(sf)
library(sjPlot)
library(sp)
library(spgwr)
library(spatialreg)
library(spacetime)
library(stargazer)
library(tidyverse)
library(tint)
library(tmap)
library(viridis)


# DETACH ------------------------------------------------------------------

detachAllPackages <- function() {
        
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        
        package.list <- setdiff(package.list,basic.packages)
        
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}

detachAllPackages()


# MAPPING DATA ------------------------------------------------------------
# file:///D:/Proyectos_R/2022/R_General/Curso_UDD2021/udd_gds_course-main/01-mapping-data.html

df_long <- read.csv(file = "udd_gds_course-main/data/internal_migration/Detailed_Estimates_2020_LA_2021_Dataset_1.csv")
head(df_long)




# id for origins and destinations -----
orig_la_nm <- as.data.frame(unique(df_long$OutLA))
dest_la_nm <- as.data.frame(unique(df_long$InLA))

head(df_long)


# read shapefile
la_shp <- st_read("udd_gds_course-main/data/Local_Authority_Districts_(May_2021)_UK_BFE_V3/LAD_MAY_2021_UK_BFE_V2.shp")

str(la_shp)

# Computing mobility indicators ----------------

# out-migration
outflows <- df_long %>% 
        group_by(OutLA) %>%
        dplyr::summarise( n = sum(Moves, na.rm = T))

# in-migration
inflows <- df_long %>% 
        group_by(InLA) %>%
        dplyr::summarise( n = sum(Moves, na.rm = T))

# net migration
indicators <- full_join(outflows, 
                        inflows,
                        by = c("OutLA" = "InLA")) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
        mutate_if(is.numeric, round) %>% 
        rename(
                outflows = n.x,
                inflows = n.y
        ) %>% 
        mutate(
                netflows = (inflows - outflows)
        ) 

# Joining spatial data

la_shp <- left_join(la_shp, indicators, by = c("LAD21CD" = "OutLA"))


# Mapping categorical data -----------------


# Let’s start by mapping categorical data and learning about the UK.

# id for country name initial
la_shp$ctry_nm <- substr(la_shp$LAD21CD, 1, 1)
la_shp$ctry_nm <- as.factor(la_shp$ctry_nm)

# simplify boundaries
la_shp_simple <- st_simplify(la_shp, 
                             preserveTopology =T,
                             dTolerance = 1000) # 1km

# ensure geometry is valid
la_shp_simple <- sf::st_make_valid(la_shp_simple)

tm_shape(la_shp_simple) +
        tm_fill(col = "ctry_nm", style = "cat", palette = viridis(4), title = "Country") +
        tm_borders(lwd = 0)  +
        tm_layout(legend.title.size = 1,
                  legend.text.size = 0.6,
                  legend.position = c("right","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1)

# Mapping continous data --------------


# Equal interval
# An option is ‘equal intervals’. The intuition is to divide the distribution into equal size segments.

tm_shape(la_shp_simple) +
        tm_fill(col = "netflows", style = "equal", palette = viridis(6), title = "Net migration") +
        tm_borders(lwd = 0) +
        tm_layout(legend.title.size = 1,
                  legend.text.size = 0.6,
                  legend.position = c("right","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1)


# Quantiles

# This algorithm ensures that the same number of data points fall into each category. 
# A potential issue could be that bin ranges can vary widely.

tm_shape(la_shp_simple) +
        tm_fill(col = "netflows", style = "quantile", palette = viridis(6), title = "Net migration") +
        tm_borders(lwd = 0) +
        tm_layout(legend.title.size = 1,
                  legend.text.size = 0.6,
                  legend.position = c("right","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1)


# Fisher-Jenks

# The Fisher-Jenks algorithm, known as ‘natural breaks’, 
# identifies groups of similar values in the data and maximises the differences between categories i.e. ‘natural breaks’.


tm_shape(la_shp_simple) +
        tm_fill(col = "netflows", style = "jenks", palette = viridis(6), title = "Net migration") +
        tm_borders(lwd = 0) +
        tm_layout(legend.title.size = 1,
                  legend.text.size = 0.6,
                  legend.position = c("right","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1)
# Order

# Order helps presenting a large number of colors over continuous surface of colours and can be very useful for rasters. 
# order can help display skewed distributions.

tm_shape(la_shp_simple) +
        tm_fill(col = "netflows", style = "order", palette = viridis(256), title = "Net migration") +
        tm_borders(lwd = 0) +
        tm_layout(legend.title.size = 1,
                  legend.text.size = 0.6,
                  legend.position = c("right","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1)

# SPATIAL WEIGHTS ---------------------------------------------------------
# file:///D:/Proyectos_R/2022/R_General/Curso_UDD2021/udd_gds_course-main/02-spatial_weights.html

rm(list = ls())
# la_shp <- st_read("/Users/Franciscorowe 1/Dropbox/Francisco/Research/github_projects/courses/udd_course/data/Local_Authority_Districts_(May_2021)_UK_BFE_V3/LAD_MAY_2021_UK_BFE_V2.shp")
la_shp <- st_read("udd_gds_course-main/data/Local_Authority_Districts_(May_2021)_UK_BFE_V3/LAD_MAY_2021_UK_BFE_V2.shp")

# simplify boundaries
la_shp_simple <- st_simplify(la_shp, 
                             preserveTopology =T,
                             dTolerance = 500) # .5km

# ensure geometry is valid
la_shp_simple <- sf::st_make_valid(la_shp_simple)

str(la_shp_simple)

# Building Spatial Weights -----------------------------

# Queen
# Based on the queen criteria, two spatial units are contiguous if they share a vortex (a single point) of their boundaries.

# library(spdep)
wm_queen <- spdep::poly2nb(la_shp_simple, queen = TRUE)
summary(wm_queen)



# How do we interpret the outcome?
        
        # Finding the most connected area:

la_shp_simple$LAD21NM[373]

# Its neighbours:

wm_queen[[373]]

# Their names:

la_shp_simple$LAD21NM[c(19,  48, 354, 356, 358, 359, 361, 363, 367, 369, 371, 374)]

# Visualising the weights matrix:

coords <- st_centroid(st_geometry(la_shp_simple))
plot(st_geometry(la_shp_simple), border="grey")
plot(wm_queen, coords, add = TRUE)

# Rook
# The rook defines two observations as neighbours if they share some of their boundaries. 
# For irregular polygons, differences between the rook and queen definitions are minimal and tend to boil down to geocoding. 
# For regular polygons, such as rasters or grids, differences are more noticeable.

wm_rook <- spdep::poly2nb(la_shp_simple, queen = FALSE)
summary(wm_rook)


# Distance-based matrices

# K-Nearest Neighbours

col.knn <- spdep::knearneigh(coords, k=4)
head(col.knn[[1]], 5)

# Displaying the network.

plot(st_geometry(la_shp_simple), border="grey")
plot(spdep::knn2nb(col.knn), coords, add=TRUE)


# Distance Band

# An alternative way to define is to draw a circle of certain radius
# and consider neighbours all observations (i.e. centroids) within that radious.

wm_dist <- spdep::dnearneigh(coords, 0, 20000, longlat = TRUE) # En metros
wm_dist


plot(st_geometry(la_shp_simple), border="grey")
plot(wm_dist, coords, add=TRUE)


# Standardised Weights Matrices

rswm_queen <- spdep::nb2listw(wm_queen, style = "W", zero.policy = TRUE)
rswm_queen$weights[1]

# This unit has 2 neighbours and each is assigned a 0.5 of the total weight.


# VIENDO TODOS LOS MAPAS 
# Se pueden cambiar los parametros para ver como varian los mapas

plot(st_geometry(la_shp_simple), border="grey")
plot(wm_queen, coords, add = TRUE)

plot(st_geometry(la_shp_simple), border="grey")
plot(spdep::knn2nb(col.knn), coords, add=TRUE)

plot(st_geometry(la_shp_simple), border="grey")
plot(wm_dist, coords, add=TRUE)




# SPATIAL ECONOMETRICS ----------------------------------------------------

# file:///D:/Proyectos_R/2022/R_General/Curso_UDD2021/udd_gds_course-main/03-spatial_econometrics.html

# clean workspace
rm(list=ls())

# load data
df_long <- read_csv("udd_gds_course-main/data/internal_migration/Detailed_Estimates_2020_LA_2021_Dataset_1.csv")

# id for origins and destinations
orig_la_nm <- as.data.frame(unique(df_long$OutLA))
dest_la_nm <- as.data.frame(unique(df_long$InLA))

# read shapefile
la_shp <- st_read("udd_gds_course-main/data/Local_Authority_Districts_(May_2021)_UK_BFE_V3/LAD_MAY_2021_UK_BFE_V2.shp")

#
#

# out-migration
outflows <- df_long %>% 
  group_by(OutLA) %>%
  dplyr::summarise( n = sum(Moves, na.rm = T))

# in-migration
inflows <- df_long %>% 
  group_by(InLA) %>%
  dplyr::summarise( n = sum(Moves, na.rm = T))

# net migration
indicators <- full_join(outflows, 
                        inflows,
                        by = c("OutLA" = "InLA")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate_if(is.numeric, round) %>% 
  rename(
    outflows = n.x,
    inflows = n.y
  ) %>% 
  mutate(
    netflows = (inflows - outflows)
  ) 

# join dfs
la_shp <- left_join(la_shp, indicators, by = c("LAD21CD" = "OutLA"))

# id for country name initial
la_shp$ctry_nm <- substr(la_shp$LAD21CD, 1, 1)
la_shp$ctry_nm <- as.factor(la_shp$ctry_nm)

# simplify boundaries
la_shp_simple <- st_simplify(la_shp, 
                             preserveTopology =T,
                             dTolerance = 1000) # 1km

# ensure geometry is valid
la_shp_simple <- sf::st_make_valid(la_shp_simple)

# Exploratory Spatial Data Analysis (ESDA)
# ESDA

ggplot(la_shp_simple, aes(x = outflows, y = inflows)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("In-migration") + 
  xlab("Out-migration") +
  theme_classic()

# Now let’s run a simple linear regression model. 
# Since we know inflows and outflows are highly correlated, we will use only outflows and avoid potential problems of multicollinearity.

eq1 <- netflows ~ outflows

m1 <- lm(
  eq1,
  la_shp_simple
)

summary(m1)

tm_shape(la_shp_simple) +
  tm_fill(col = "netflows", style = "equal", palette = viridis(6), title = "Net migration") +
  tm_borders(lwd = 0) +
  tm_facets(by = "ctry_nm", ncol = 2) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)


# Spatial lag

# Creating a spatial weight matrix
# So first let’s build and standardise a spatial weight matrix. For this example, we’ll use the 10 k nearest neighbours.

# replace nas with 0s to avoid issues
la_shp_simple <- la_shp_simple %>% mutate_if(
  is.numeric, ~replace(., is.na(.), 0)
)

# create knn list
coords <- st_centroid(st_geometry(la_shp_simple))
col_knn <- spdep::knearneigh(coords, k = 10)
# create nb object
hnb <- spdep::knn2nb(col_knn)
# create spatial weights matrix (note it row-standardizes by default)
hknn <- spdep::nb2listw(hnb)
hknn

# Creating a spatial lag

outflows_lag <- spdep::lag.listw(hknn, la_shp_simple$outflows)
head(outflows_lag)

# The way to interpret the spatial lag outflows_lag for the first observation: 
# Hartlepool, where 2,660 people out-migrated is surrounded by neighbouring local authorities where, on average, 4,533 people also left.



# Spatial Autocorrelation:
# We first start exploring global spatial autocorrelation. 
# To this end, we will focus on the Moran Plot and Moran’s I statistics.

# Moran Plot


ggplot(la_shp_simple, aes(x = outflows, y = outflows_lag)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("Out-migration lag") + 
  xlab("Out-migration") +
  theme_classic()

la_shp_simple_eng <- cbind(la_shp_simple, as.data.frame(outflows_lag))

la_shp_simple_eng <- la_shp_simple_eng %>% filter(
  ctry_nm == "E"
) %>% 
  mutate(
    st_outflows = ( outflows - mean(outflows)) / sd(outflows),
    st_outflows_lag = ( outflows_lag - mean(outflows_lag)) / sd(outflows_lag)
  )




ggplot(la_shp_simple_eng, aes(x = st_outflows, y = st_outflows_lag)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, color = "grey", alpha =.5) +
  geom_vline(xintercept = 0, color = "grey", alpha =.5) +
  ylab("Out-migration lag") + 
  xlab("Out-migration") +
  theme_classic()


# Moran’s I

# To measure global spatial autocorrelation, we can use the Moran’s I. 
# The Moran Plot and intrinsicaly related. 
# The value of Moran’s I corresponds with the slope of the linear fit on the Moran Plot. 
# We can compute it by running:


spdep::moran.test(la_shp_simple$outflows, listw = hknn, zero.policy = TRUE, na.action = na.omit)

# Exogenous spatial effects model

eq2 <- netflows ~ outflows + outflows_lag
m2 <- lm(
  eq2,
  la_shp_simple
)

summary(m2)

