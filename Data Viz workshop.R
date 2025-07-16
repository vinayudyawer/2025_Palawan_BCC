##
## R Script to follow along with the Data Viz and Analysis workshop
## Link to the workshop: 
## https://vinayudyawer.github.io/2025_Palawan_BCC/Data-Viz-workshop.html


## ------------------------------------------------------------------------------ ##
## Installing packages
## ------------------------------------------------------------------------------ ##
## Packages that are on CRAN
install.packages(c("tidyverse",
                   "sf",
                   "terra",
                   "ggspatial",
                   "ggaminate",
                   "ggrepel",
                   "ggforce",
                   "gifski",
                   "scatterpie",
                   "leaflet",
                   "remotes",
                   "patchwork",
                   "rnaturalearthhires"), 
                 dependencies = TRUE)

## Install packages from GitHub and other external repositories
remotes::install_github("r-spatial/mapview", build_vignettes = TRUE)
install.packages("aniMotum", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE)



## ------------------------------------------------------------------------------ ##
## Example 1: ish Market Surveys in the Philippines
## ------------------------------------------------------------------------------ ##
### Step 1.1: Load Libraries and Create Mock Market Data

# Load necessary libraries for this analysis
library(tidyverse)
library(ggrepel)
library(sf)
library(ggspatial)
library(patchwork)
library(scatterpie) # For pie charts on maps

# Lets input some mock market data
market_data <- read_csv("https://raw.githubusercontent.com/vinayudyawer/2025_Palawan_BCC/refs/heads/main/Data/market_data.csv")

head(market_data)

# Prepare the data for the pie chart map
market_summary <- 
  market_data %>%
  group_by(location, lon, lat) %>%
  summarise(across("Blue Shark":number_of_boats, sum))

head(market_summary)


## Lets set up some non-standard basemaps for plotting on maps
esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

esri_topo <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                    'World_Street_Map/MapServer/tile/${z}/${y}/${x}.jpeg')

esri_terrain <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                       'World_Terrain_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


## ------------------------------------------------------------------------------ ##
### Step 1.2: Map of Survey Sites with Species Composition

# Define a bounding box for the Philippines
ph_bbox <- c(xmin = 117, xmax = 127, ymin = 5, ymax = 19)

# Create a bubble plot map of fishing effort
effort_map <-
  ggplot() +
  annotation_map_tile(type = esri_sat, zoom = 6) +
  geom_spatial_label_repel(data = market_summary, box.padding = 1, size = 3,
                           aes(x = lon, y = lat, label = location)) +
  geom_spatial_point(data = market_summary, color = "red",
                     aes(x = lon, y = lat, size = number_of_boats)) +
  coord_sf(xlim = ph_bbox[1:2], ylim = ph_bbox[3:4], crs = 4326) +
  annotation_scale(location = "bl", line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(line_col = "white",
                                                                text_col = "white")) +
  labs(title = "Fishing effort", size = "Number of boats",
       subtitle = "Number of boats from 2014-2023",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Create a scatterplot map of catch composition
market_map <-
  ggplot() +
  annotation_map_tile(type = esri_sat, zoom = 6) +
  geom_spatial_label_repel(data = market_summary, box.padding = 1, size = 3,
                           aes(x = lon, y = lat, label = location)) +
  geom_scatterpie(data = market_summary, lwd = 0.2, 
                  aes(x = lon, y = lat, r = 0.4), # r controls pie size
                  cols = names(market_summary)[-c(1:3,12)]) +
  coord_sf(xlim = ph_bbox[1:2], ylim = ph_bbox[3:4], crs = 4326) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  annotation_scale(location = "bl", line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(line_col = "white",
                                                                text_col = "white")) +
  labs(title = "Elasmobranch catch composition",
       subtitle = "Total catch from 2014-2023",
       x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Lets put them together
final_map <- wrap_plots(effort_map, market_map)

final_map


## ------------------------------------------------------------------------------ ##
### Step 1.3: Temporal Analysis of Catch Composition

## Summarised area chart across all ports
# Prepare data for temporal plot
temporal_data_summary <-
  market_data %>%
  group_by(year) %>%
  summarise(across("Blue Shark":"Whitespotted Guitarfish", sum)) %>%
  pivot_longer(cols = -year, names_to = "species", values_to = "count")

# Create a stacked area chart
temporal_plot_summary <- 
  temporal_data_summary %>% 
  ggplot(aes(x = year, y = count, fill = species)) +
  geom_area(position = 'fill', color="white", linewidth=0.2) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2014, 2023, by = 1), expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  labs(title = "Overall change in market catch composition", 
       subtitle = "Across all surveyed sites in the Philippines", 
       x = "Year", y = "Proportion of Total Catch") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 2))


temporal_plot_summary


# Create a stacked area chart
temporal_plot_ports <-
  market_data %>%
  pivot_longer(cols = -c(1:4, 13), names_to = "species", values_to = "count") %>% 
  ggplot(aes(x = year, y = count, fill = species)) +
  geom_area(position = 'fill', color="white") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion of Total Catch", expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2014, 2023, by = 2), expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  facet_wrap(~location) +
  labs(title = "Port-specific change in market catch composition",
       x = "Year") +
  theme_bw() +
  theme(legend.position = c(0.68, 0.15), legend.direction = "horizontal")

temporal_plot_ports

# Create a line chart showing changes in fishing effort over time
fishing_effort_plot <-
  market_data %>% 
  group_by(year, location) %>% 
  summarise(number_of_boats = sum(number_of_boats, na.rm = T)) %>% 
  ggplot(aes(x = year, y = number_of_boats, col = location)) +
  geom_point() +
  geom_path() +
  scale_x_continuous(breaks = 2014:2023, expand = c(0,0)) +
  scale_color_brewer(palette = "Reds", name = "Location") +
  labs(title = "Fishing effort over time", 
       x = "Year", y = "Number of boats per year") +
  theme_bw() +
  theme(legend.position = "top")

# Create a line chart showing change in catch over time
species_plot <-
  market_data %>%
  pivot_longer(cols = -c(1:4, 13), names_to = "species", values_to = "count") %>% 
  ggplot(aes(x = year, y = count, color = species, fill = species)) +
  stat_summary(fun = "mean") + 
  stat_summary(fun = "mean", geom = "line") + 
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = 2014:2023, expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  scale_color_brewer(palette = "Set2", name = "Species") +
  labs(title = "Species-specific total catch over time", 
       x = "Year", y = "Total Catch (count)") +
  theme_bw() +
  theme(legend.position = "top")

temporal_plots <- wrap_plots(fishing_effort_plot, species_plot)

temporal_plots

# Lets integrate the fishing effort into this plot
# calculate catch per unit effort (numbers per boat)
cpue_summary <-
  market_data %>%
  group_by(year) %>%
  summarise(across("Blue Shark":"Whitespotted Guitarfish", ~.x/number_of_boats)) %>%
  pivot_longer(cols = -year, names_to = "species", values_to = "cpue")

cpue_plot_species <-
  cpue_summary %>% 
  ggplot(aes(x = year, y = cpue, color = species, fill = species)) +
  stat_summary(fun = "mean") +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2, color = NA) +
  scale_y_continuous(name = "Catch per unit effort (count per boat)") +
  scale_x_continuous(breaks = 2014:2023, expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  scale_color_brewer(palette = "Set2", name = "Species") +
  labs(title = "Catch per unit effort per species over time",
       x = "Year") +
  theme_bw() +
  theme(legend.position = "top")

cpue_plot_species



## ------------------------------------------------------------------------------ ##
## Example 2: Satellite Tracking in the Philippines
## ------------------------------------------------------------------------------ ##
### Step 2.1: Load Libraries

# Load libraries to visualise and make animations of maps
library(gganimate)
library(gifski) 
library(ggforce)
library(terra)
library(mapview)
library(leaflet)
library(aniMotum)
library(rnaturalearthhires)


## ------------------------------------------------------------------------------ ##
### Step 2.2: Static Map of All Shark Tracks

# Lets import some mock tracking data
all_tracks_sf <- st_read("https://raw.githubusercontent.com/vinayudyawer/2025_Palawan_BCC/refs/heads/main/Data/track_data.geojson")

head(all_tracks_sf)

# Now lets import spatial data on marine parks and fishing pressure
mpa_sf <- st_read("https://raw.githubusercontent.com/vinayudyawer/2025_Palawan_BCC/refs/heads/main/Data/MPA_data.geojson")

head(mpa_sf)

# Lets get some fishing effort data from Global Fishing Watch (already processed)
fishing_effort <- read_csv("https://raw.githubusercontent.com/vinayudyawer/2025_Palawan_BCC/refs/heads/main/Data/GFW_FishingEffort.csv")

head(fishing_effort)


# Create a non-spatial version for plotting geoms that need x/y aesthetics
all_tracks_df <- 
  all_tracks_sf %>%
  st_drop_geometry()

# Create the static map
static_track_map_with_error <-
  ggplot() +
  annotation_map_tile(type = esri_topo, zoom = 6) +
  # --- Add Core Track Data ---
  geom_spatial_path(data = all_tracks_sf, size = 1, crs = 4326,
                    aes(x = lon, y = lat, color = id)) +
  # --- Add Error Visualizations ---
  # 1. Tiger Shark: Error Ellipses
  # Note: converting meters to degrees is an approximation and varies with latitude.
  # 1 degree of latitude is ~111.32 km.
  geom_ellipse(data = filter(all_tracks_df, id %in% "Tiger Shark"),
               aes(x0 = lon, y0 = lat,
                   a = smaj / 111320,
                   b = smin / 111320,
                   angle = eor),
               fill = "firebrick", alpha = 0.2, color = NA) +
  # 2. Thresher Shark: Error Ellipses
  geom_ellipse(data = filter(all_tracks_df, id %in% "Thresher Shark"),
               aes(x0 = lon, y0 = lat, 
                   a = abs(lonerr), 
                   b = abs(laterr), 
                   angle = 0),
               fill = "forestgreen", alpha = 0.2, color = NA) +
  # Use facet_wrap to give each shark its own mini-plot with its track
  facet_wrap(~id, nrow = 1) +
  coord_sf(crs = 4326) +
  annotation_scale() +
  labs(title = "Satellite Tracks of Three Shark Species with Location Error",
       x = "Longitude", y = "Latitude") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "none")


static_track_map_with_error


# process the fishing effort data to a summary raster
fishing_effort_summary <-
  fishing_effort %>% 
  group_by(lon, lat) %>% 
  summarise(fishing_hours = sum(fishing_hours, na.rm = T)) %>% 
  terra::rast()

# we have to add the spatial reference
crs(fishing_effort_summary) <- "epsg:4326"

# Lets plot the overall pattern in fishing effort
ggplot() +
  annotation_map_tile(type = esri_terrain, zoom = 5) +
  layer_spatial(fishing_effort_summary) +
  layer_spatial(mpa_sf, fill = "darkgreen", col = NA, alpha = 0.7) +
  coord_sf(xlim = range(fishing_effort$lon),
           ylim = range(fishing_effort$lat),
           crs = 4326) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = viridisLite::plasma(100), 
    na.value = NA,
    labels = scales::comma) +
  labs(x = NULL, y = NULL, fill = "Fishing hours\n(2021-2024)", 
       caption = "Source: Global Fishing Watch") +
  theme_bw() +
  theme(legend.key.height = unit(2, "cm"))


# Lets plot fishing effort over time
fishing_over_time <-
  ggplot() +
  annotation_map_tile(type = esri_terrain, zoom = 5) +
  geom_tile(data = fishing_effort,
            aes(x = lon, y = lat, fill = fishing_hours)) +
  coord_sf(xlim = range(fishing_effort$lon),
           ylim = range(fishing_effort$lat),
           crs = 4326) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = viridisLite::plasma(100), 
    na.value = NA,
    labels = scales::comma) +
  facet_wrap(~year, nrow = 1) +
  labs(x = NULL, y = NULL, fill = "Fishing hours", 
       caption = "Source: Global Fishing Watch") +
  theme_bw() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

fishing_over_time


# Interactive (but static) plot of the raw data with MPA and fishing effort data

# First lets convert the points into trajectories
all_tracks_traj <-
  all_tracks_sf %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

m <- 
  mapview(all_tracks_traj, zcol = "id", map.type = "Esri.WorldTerrain",
          burst = TRUE, legend = FALSE, homebutton = FALSE) +
  mapview(all_tracks_sf, zcol = "id", 
          burst = TRUE, cex = 3, legend = FALSE, homebutton = FALSE) +
  mapview(log10(fishing_effort_summary + 1), layer.name = "Fishing Effort",
          legend = FALSE, homebutton = FALSE, na.color = "transparent", alpha = 0.8) +
  mapview(mpa_sf, color = NA, col.regions = "darkgreen", col.alpha = 0.8, alpha = 0,
          layer.name = "MPA", legend = FALSE, homebutton = FALSE)

m@map %>% 
  addLayersControl(baseGroups = unique(all_tracks_df$id), 
                   overlayGroups = c("MPA", "Fishing Effort"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(group = c("MPA", "Fishing Effort"))


## ------------------------------------------------------------------------------ ##
### Step 2.3: Using State-space models to refine tracking data


# Lets fit a Correlated Ramdon Walk model to the data and re-route the paths around land
fit <- 
  fit_ssm(all_tracks_df, 
          model = "crw") %>% 
  route_path(map_scale = 10)

## Note that map_scale = 10 is only available if you have the `rnaturalearthhires` package


## Lets have a look at the fitted component of the model 

plot(fit, 
     what = "rerouted", ## what component of the model to plot ('fitted', 'predicted' or 'rerouted')
     type = 2, ## type of plot to make
     pages = 1, 
     ncol = 3)


## Lets plot our own version of the rerouted component using mapview
re_data <- grab(fit, what = "rerouted")

## Lets convert the dataset into points and paths using the `sf` package
re_point <-
  re_data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

re_path <- 
  re_point %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

## Now lets plot a nice interactive plot of the move persistence data using `mapview` and 'leaflet'

mm <- 
  mapview(re_path, zcol = "id", map.type = "Esri.WorldTerrain",
          burst = TRUE, legend = FALSE, homebutton = FALSE) +
  mapview(re_point, zcol = "id", 
          burst = TRUE, cex = 3, legend = FALSE, homebutton = FALSE) +
  mapview(log10(fishing_effort_summary + 1), layer.name = "Fishing Effort",
          legend = FALSE, homebutton = FALSE, na.color = "transparent", alpha = 0.8) +
  mapview(mpa_sf, color = NA, col.regions = "darkgreen", col.alpha = 0.8, alpha = 0,
          layer.name = "MPA", legend = FALSE, homebutton = FALSE)

mm@map %>% 
  addLayersControl(baseGroups = unique(re_path$id), 
                   overlayGroups = c("MPA", "Fishing Effort"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(group = c("MPA", "Fishing Effort"))


## ------------------------------------------------------------------------------ ##
### Step 2.4: Animating the Shark Movements

# Note: Rendering animations can be slow.
# Create the animation object
anim <-
  ggplot() +
  annotation_map_tile(type = esri_terrain, zoom = 7, progress = "none") +
  geom_spatial_point(data = re_data, size = 2, crs = 4326,
                     aes(x = lon, y = lat, color = id, group = id)) +
  geom_spatial_path(data = re_data, size = 1, crs = 4326, alpha = 0.5,
                    aes(x = lon, y = lat, color = id, group = id)) +
  coord_sf(crs = 4326) +
  labs(x = "Longitude", y = "Latitude", color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top") +
  # the gganimate magic happens here!
  transition_reveal(date)



# Animate and save the gif
# This will save a file named 'shark_animation.gif' in your working directory
## ---------------------- CAUTION ---------------------- ##
# The rendering process may take a bit of time...
anim_save("shark_animation.gif", animation = anim, 
          width = 6, height = 5, units = "in", res = 150,
          fps = 20, duration = 15, end_pause = 10,
          renderer = gifski_renderer(loop = FALSE))
## ----------------------------------------------------- ##




## ------------------------------------------------------------------------------ ##
## End of workshop
## ------------------------------------------------------------------------------ ##




