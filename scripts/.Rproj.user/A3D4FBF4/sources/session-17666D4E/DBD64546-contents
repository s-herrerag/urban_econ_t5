################################################
# Taller 5 Economia Urbana: Ejercicio 1.1
################################################

#Mapas del cambio en la poblacion minoritaria de Estados Unidos en census tracts. 

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(sf, haven, tidyverse, RColorBrewer)


# Data --------------------------------------------------------------------
boundaries_census <- st_read("../data/Boundaries - Census Tracts - 2010")
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
         prop_hispanic = 100*Hispanic_Pop/Total_Pop) %>%
  st_as_sf()

#Es útil tener una base en formato wide con poblacion de hispanos y afroamericanos y sus cambios
wide_minorities <- panel_data_sf %>%
  select(c(cod_census_track, year, Median_Inc, Black_Pop, Hispanic_Pop, prop_black, prop_hispanic, geometry)) %>%
  pivot_wider(names_from = "year",
              id_cols = c("cod_census_track", "geometry"),
              values_from = c("Median_Inc", "Black_Pop", "Hispanic_Pop","prop_black", "prop_hispanic")) %>%
  mutate(change_black = prop_black_2020 - prop_black_2000,
         change_hispanic = prop_hispanic_2020 - prop_hispanic_2000) %>%
  st_as_sf()

# Mapas -------------------------------------------------------------------

color_bins <- c("#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B")

map_black_change <- ggplot(wide_minorities) +
  geom_sf(aes(fill = change_black)) +
  labs(fill = "Cambio en la proporción\n(Puntos porcentuales)") +
  theme_void() +
  binned_scale("fill", "foo", ggplot2:::binned_pal(scales::manual_pal(color_bins)),
    guide="coloursteps", breaks=seq(-80,60,20), limits=c(-80, 60),
    show.limits=TRUE) 

map_hispanic_change <- ggplot(wide_minorities) +
  geom_sf(aes(fill = change_hispanic)) +
  labs(fill = "Cambio en la proporción\n(Puntos porcentuales)") +
  theme_void()

# Correlacion con el ingreso mediano --------------------------------------

#Modificar ligeramente panel_data_sf
cors_panel_data <- panel_data_sf %>%
  filter(year == 2020) %>%
  pivot_longer(cols = c(prop_black, prop_hispanic), names_to = "minority", values_to = "prop_in_pop") %>%
  mutate(minority = ifelse(minority=="prop_black", "Afroamericanos", "Hispanos"))

cor_inc <- ggplot(cors_panel_data, aes(x = prop_in_pop, y = Median_Inc, color = minority)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  ylab("Ingreso mediano") +
  xlab("Proporción en la población del census tract") +
  labs(color = "Minoría") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal() 
















