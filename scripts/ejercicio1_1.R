################################################
# Taller 5 Economia Urbana: Ejercicio 1.1
################################################

#Mapas del cambio en la poblacion minoritaria de Estados Unidos en census tracts. 

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(sf, haven, tidyverse, RColorBrewer)


# Data --------------------------------------------------------------------
boundaries_census <- st_read("../data/Boundaries - Census Tracts - 2010") %>%
  st_transform(crs = 4326)
panel_data <- read_dta("../data/Combined_data_Panel.dta")

#Agregar la informacion geografica a panel_data

#Columnas que no nos interesan de boundaries
columns_noninterest_boundaries <- c("commarea", "commarea_n", "countyfp10", 
                                    "name10", "namelsad10", "notes", "statefp10", "tractce10")

#Join
panel_data_sf <- panel_data %>%
  inner_join(boundaries_census, by = c("cod_census_track" = "geoid10")) %>% #Hay algunos tracts que no se pueden mapear
  select(-all_of(columns_noninterest_boundaries)) %>%
  mutate(prop_black = 100*Black_Pop/Total_Pop, 
         prop_hispanic = 100*Hispanic_Pop/Total_Pop,
         prop_white = 100*White_Pop/Total_Pop) %>%
  st_as_sf()

#Es útil tener una base en formato wide con poblacion de hispanos y afroamericanos y sus cambios
wide_minorities <- panel_data_sf %>%
  select(c(cod_census_track, year, Median_Inc, White_Pop,Total_Pop, Black_Pop, Hispanic_Pop, prop_white, prop_black, prop_hispanic, geometry)) %>%
  pivot_wider(names_from = "year",
              id_cols = c("cod_census_track", "geometry"),
              values_from = c("Median_Inc", "Total_Pop", "White_Pop","Black_Pop", "Hispanic_Pop", "prop_white", "prop_black", "prop_hispanic")) %>%
  mutate(change_black = prop_black_2020 - prop_black_2000,
         change_hispanic = prop_hispanic_2020 - prop_hispanic_2000) %>%
  st_as_sf()


# Datos adicionales -------------------------------------------------------

cbd_chicago <- st_read("../data/Boundaries - Central Business District.geojson") %>%
  mutate(name = "CBD, Chicago") %>%
  st_centroid()


# Mapas -------------------------------------------------------------------

color_bins <- c("#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850")

map_black_change <- ggplot(wide_minorities) +
  geom_sf(aes(fill = change_black)) +
  geom_sf(data = cbd_chicago) +
  ggrepel::geom_label_repel(data = cbd_chicago, aes(label = name, geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(fill = "Cambio en la proporción\n(Puntos porcentuales)") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(color_bins)),
    guide="coloursteps", breaks=seq(-80,60,20), limits=c(-80, 60),
    show.limits=TRUE) 

map_hispanic_change <- ggplot(wide_minorities) +
  geom_sf(aes(fill = change_hispanic)) +
  geom_sf(data = cbd_chicago) +
  ggrepel::geom_label_repel(data = cbd_chicago, aes(label = name, geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(fill = "Cambio en la proporción\n(Puntos porcentuales)") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(color_bins)),
               guide="coloursteps", breaks=seq(-80,60,20), limits=c(-80, 60),
               show.limits=TRUE)

ggsave("../views/map_black_change.png", map_black_change, dpi = 300)
ggsave("../views/map_hispanic_change.png", map_hispanic_change, dpi = 300)

# Correlacion con el ingreso mediano --------------------------------------

#Modificar ligeramente panel_data_sf
cors_panel_data <- panel_data_sf %>%
  filter(year == 2020) %>%
  pivot_longer(cols = c(prop_black, prop_hispanic), names_to = "minority", values_to = "prop_in_pop") %>%
  mutate(minority = ifelse(minority=="prop_black", "Afroamericanos", "Hispanos"))

cor_inc <- ggplot(cors_panel_data, aes(x = prop_in_pop, y = Median_Inc, color = minority)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  ylab("Ingreso mediano") +
  xlab("Proporción de la minoría en la población del census tract") +
  labs(color = "Minoría") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal() 

ggsave("../views/cor_minority_inc.png", cor_inc, dpi = 300)

# Ubicacion de las minorias -----------------------------------------------

color_bins_prop <- c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081", "#020a26")
  
#Mapa simple de las proporciones
map_hispanics <- ggplot(wide_minorities) +
  geom_sf(aes(fill = prop_hispanic_2020)) +
  geom_sf(data = cbd_chicago) +
  ggrepel::geom_label_repel(data = cbd_chicago, aes(label = name, geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(fill = "Proporción de hispanos (%)") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(color_bins_prop)),
               guide="coloursteps", breaks=seq(0,100,10), limits=c(0, 100),
               show.limits=TRUE)

map_blacks <- ggplot(wide_minorities) +
  geom_sf(aes(fill = prop_black_2020)) +
  geom_sf(data = cbd_chicago) +
  ggrepel::geom_label_repel(data = cbd_chicago, aes(label = name, geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(fill = "Proporción de\nafroamericanos (%)") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(color_bins_prop)),
               guide="coloursteps", breaks=seq(0,100,10), limits=c(0, 100),
               show.limits=TRUE)



# Ammenities: Crimen ---------------------------------------------------

crime_chicago <- read_csv("../data/Crimes_-_One_year_prior_to_present.csv")

#sf - group
crime_chicago_sf <- crime_chicago %>%
  drop_na(LOCATION) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
  `st_crs<-`(4326) %>%
  st_join(boundaries_census) %>%
  group_by(geoid10) %>%
  summarise(n_crimes = n()) %>%
  drop_na() %>%
  as_data_frame() %>%
  select(-geometry)

#Agregar a wide
wide_minorities <- wide_minorities %>%
  left_join(crime_chicago_sf, by = c("cod_census_track" = "geoid10")) %>%
  mutate(crimes_capita = n_crimes / Total_Pop_2020) %>%
  mutate(crimes_capita = ifelse(crimes_capita ==Inf, NA, crimes_capita))

#Map crime
col_crime <- c("#E7E1EF", "#D4B9DA", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F")
map_crime <- ggplot(wide_minorities) +
  geom_sf(aes(fill = crimes_capita)) + 
  theme_void() +
  geom_sf(data = cbd_chicago) +
  ggrepel::geom_label_repel(data = cbd_chicago, aes(label = name, geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(fill = "Crímenes per cápita") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(col_crime)),
               guide="coloursteps", breaks=seq(0, 0.6, 0.1), limits=c(0, 0.6),
               show.limits=TRUE)

ggsave("../views/crime_capita.png", map_crime, dpi = 300)

