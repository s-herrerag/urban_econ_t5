################################################
# Taller 5 Economia Urbana: Ejercicio 1.3
################################################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(sf, haven, tidyverse, RColorBrewer, janitor, stargazer)

#Usamos los datos del ejercicio 1.1
source("ejercicio1_1.R")

# Cálculo de los tipping points -------------------------------------------

# 1) Shares de todas las poblaciones de interés
wide_minorities <- wide_minorities %>%
  mutate(across(starts_with("prop"), ~./100)) %>%
  mutate(prop_nw_2000 = (Total_Pop_2000 - White_Pop_2000) / Total_Pop_2000, 
         prop_nw_2015 = (Total_Pop_2015 - White_Pop_2015) / Total_Pop_2015, 
         Dwhite_2000_2015 = (White_Pop_2015 - White_Pop_2000) / Total_Pop_2000,
         Dwhite_2000_2020 = (White_Pop_2020 - White_Pop_2000) / Total_Pop_2000,
         Dwhite_2015_2020 = (White_Pop_2020 - White_Pop_2015) / Total_Pop_2015)
  
# 2) Encontrar tippings

tipping <- function(df, minority, dwhite, year){
  
  #Modelo
  rsquared_values <- numeric(600) 
  for (i in 1:600) {
    
    df1 <- df %>%
      mutate(dummy = ifelse(eval(parse(text = paste0("prop_", minority, "_", year))) > i/1000, 1, 0))
    
    regression_model <- lm(eval(parse(text = dwhite)) ~ dummy, data = df1)
    
    rsquared_values[i] <- summary(regression_model)$r.squared
  }
  
  #Max R2
  max_rsquared <- max(rsquared_values)
  best_percentage <- which(rsquared_values == max_rsquared) /10
  
  if (length(best_percentage) >1) {
    best_percentage = min(best_percentage)
  }
  
  best_percentage
  
}  

plot_tipping <- function(tipping, df, minority, year, dwhite, xlab, ylab){
  
  vector_dwhite <- as.data.frame(df) %>%
    select(dwhite) %>%
    pull(dwhite)
  
  plot <- ggplot(df, aes(x = eval(parse(text = paste0("prop_", minority, "_", year))), y = eval(parse(text = dwhite)))) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = mean(vector_dwhite, na.rm = TRUE), color = "blue", linetype = "dashed") +
    geom_vline(xintercept = tipping/100, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", span = 0.5, kernel = "epanechnikov", formula = y ~ x, fullrange = FALSE, level = 0) +
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-.5, .2))
  
  plot
}
 

#Non white
nw_tip_2000_2015 <- tipping(wide_minorities, "nw", "Dwhite_2000_2015", 2000)
plot_nw_2000_2015 <- plot_tipping(nw_tip_2000_2015, wide_minorities, "nw",2000, 
                                  "Dwhite_2000_2015", "Proporción de no blancos en el 2000",
                                  "Cambio en la población blanca 2000 - 2015" )
ggsave("../views/tipping/nw_tip_2000-2015.png", plot_nw_2000_2015, dpi = 300)

nw_tip_2000_2020 <- tipping(wide_minorities, "nw", "Dwhite_2000_2020", 2000)
plot_nw_2000_2020 <- plot_tipping(nw_tip_2000_2020, wide_minorities, "nw",2000, 
                                  "Dwhite_2000_2020", "Proporción de no blancos en el 2000",
                                  "Cambio en la población blanca 2000 - 2020" )
ggsave("../views/tipping/nw_tip_2000-2020.png", plot_nw_2000_2020, dpi = 300)

nw_tip_2015_2020 <- tipping(wide_minorities, "nw", "Dwhite_2015_2020", 2015)
plot_nw_2015_2020 <- plot_tipping(nw_tip_2015_2020, wide_minorities, "nw", 2015, 
                                  "Dwhite_2015_2020", "Proporción de no blancos en el 2015",
                                  "Cambio en la población blanca 2015 - 2020" )
ggsave("../views/tipping/nw_tip_2015-2020.png", plot_nw_2015_2020, dpi = 300)


#Black
black_tip_2000_2015 <- tipping(wide_minorities, "black", "Dwhite_2000_2015", 2000)
plot_black_2000_2015 <- plot_tipping(black_tip_2000_2015, wide_minorities, "black",2000, 
                                  "Dwhite_2000_2015", "Proporción de afroamericanos en el 2000",
                                  "Cambio en la población blanca 2000 - 2015" )
ggsave("../views/tipping/black_tip_2000-2015.png", plot_black_2000_2015, dpi = 300)

black_tip_2000_2020 <- tipping(wide_minorities, "black", "Dwhite_2000_2020", 2000)
plot_black_2000_2020 <- plot_tipping(black_tip_2000_2020, wide_minorities, "black",2000, 
                                  "Dwhite_2000_2020", "Proporción de afroamericanos en el 2000",
                                  "Cambio en la población blanca 2000 - 2020" )
ggsave("../views/tipping/black_tip_2000-2020.png", plot_black_2000_2020, dpi = 300)

black_tip_2015_2020 <- tipping(wide_minorities, "black", "Dwhite_2015_2020", 2015)
plot_black_2015_2020 <- plot_tipping(black_tip_2015_2020, wide_minorities, "black", 2015, 
                                  "Dwhite_2015_2020", "Proporción de afroamericanos en el 2015",
                                  "Cambio en la población blanca 2015 - 2020" )
ggsave("../views/tipping/black_tip_2015-2020.png", plot_black_2015_2020, dpi = 300)


#Hispanic
hispanic_tip_2000_2015 <- tipping(wide_minorities, "hispanic", "Dwhite_2000_2015", 2000)
plot_hispanic_2000_2015 <- plot_tipping(hispanic_tip_2000_2015, wide_minorities, "hispanic", 2000, 
                                     "Dwhite_2000_2015", "Proporción de hispanos en el 2000",
                                     "Cambio en la población blanca 2000 - 2015" )
ggsave("../views/tipping/hispanic_tip_2000-2015.png", plot_hispanic_2000_2015, dpi = 300)

hispanic_tip_2000_2020 <- tipping(wide_minorities, "hispanic", "Dwhite_2000_2020", 2000)
plot_hispanic_2000_2020 <- plot_tipping(hispanic_tip_2000_2020, wide_minorities,"hispanic", 2000, 
                                     "Dwhite_2000_2020", "Proporción de hispanos en el 2000",
                                     "Cambio en la población blanca 2000 - 2020" )
ggsave("../views/tipping/hispanic_tip_2000-2020.png", plot_hispanic_2000_2020, dpi = 300)

hispanic_tip_2015_2020 <- tipping(wide_minorities, "hispanic", "Dwhite_2015_2020", 2015)
plot_hispanic_2015_2020 <- plot_tipping(hispanic_tip_2015_2020, wide_minorities, "hispanic",2015, 
                                     "Dwhite_2015_2020", "Proporción de hispanos en el 2015",
                                     "Cambio en la población blanca 2015 - 2020" )
ggsave("../views/tipping/hispanic_tip_2015-2020.png", plot_hispanic_2015_2020, dpi = 300)



# Mapas -------------------------------------------------------------------

#Todos los tippings

wide_minorities <- wide_minorities %>%
  mutate(dum_nw_tip_2000_2015 = ifelse(prop_nw_2000 >= nw_tip_2000_2015/100, 1, 0), 
         dum_nw_tip_2000_2020 = ifelse(prop_nw_2000 >= nw_tip_2000_2020/100, 1, 0),
         dum_nw_tip_2015_2020 = ifelse(prop_nw_2015 >= nw_tip_2015_2020/100, 1, 0),
         dum_black_tip_2000_2015 = ifelse(prop_black_2000 >= black_tip_2000_2015/100, 1, 0), 
         dum_black_tip_2000_2020 = ifelse(prop_black_2000 >= black_tip_2000_2020/100, 1, 0),
         dum_black_tip_2015_2020 = ifelse(prop_black_2015 >= black_tip_2015_2020/100, 1, 0),
         dum_hispanic_tip_2000_2015 = ifelse(prop_hispanic_2000 >= hispanic_tip_2000_2015/100, 1, 0), 
         dum_hispanic_tip_2000_2020 = ifelse(prop_hispanic_2000 >= hispanic_tip_2000_2020/100, 1, 0),
         dum_hispanic_tip_2015_2020 = ifelse(prop_hispanic_2015 >= hispanic_tip_2015_2020/100, 1, 0))


#Nw
map_tip_nw_2000_2015 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_nw_tip_2000_2015))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_nw_2000_2015.png", map_tip_nw_2000_2015, dpi = 300)

map_tip_nw_2000_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_nw_tip_2000_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_nw_2000_2020.png", map_tip_nw_2000_2020, dpi = 300)

map_tip_nw_2015_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_nw_tip_2015_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_nw_2015_2020.png", map_tip_nw_2015_2020, dpi = 300)


#Black
map_tip_black_2000_2015 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_black_tip_2000_2015))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_black_2000_2015.png", map_tip_black_2000_2015, dpi = 300)

map_tip_black_2000_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_black_tip_2000_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_black_2000_2020.png", map_tip_black_2000_2020, dpi = 300)

map_tip_black_2015_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_black_tip_2015_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_black_2015_2020.png", map_tip_black_2015_2020, dpi = 300)

#Hispanic
map_tip_hispanic_2000_2015 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_hispanic_tip_2000_2015))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_hispanic_2000_2015.png", map_tip_hispanic_2000_2015, dpi = 300)

map_tip_hispanic_2000_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_hispanic_tip_2000_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_hispanic_2000_2020.png", map_tip_hispanic_2000_2020, dpi = 300)

map_tip_hispanic_2015_2020 <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_hispanic_tip_2015_2020))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_hispanic_2015_2020.png", map_tip_hispanic_2015_2020, dpi = 300)


# Tabla con resumen de resultados de tipping ------------------------------

results_tipping <- data.frame(t_year = c ("2000-2015", "2000-2020", "2015-2020"),
                              nw = c(nw_tip_2000_2015, nw_tip_2000_2020, nw_tip_2015_2020), 
                              black = c(black_tip_2000_2015, black_tip_2000_2020, black_tip_2015_2020),
                              hispanic = c(hispanic_tip_2000_2015, hispanic_tip_2000_2020, hispanic_tip_2015_2020)) %>%
  t() %>%
  row_to_names(row_number = 1)

rownames(results_tipping) <- c("Minorías (no blancos)", "Afroamericanos", "Hispanos")

stargazer(results_tipping, type = "latex", summary = F, out = "../views/tipping/results_tipping.tex")

