################################################
# Taller 5 Economia Urbana: Ejercicio 1.3
################################################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(sf, haven, tidyverse, RColorBrewer, seg)

#Usamos los datos del ejercicio 1.1
source("ejercicio1_1.R")

# Cálculo de los tipping points -------------------------------------------

# 1) Shares de todas las poblaciones de interés
wide_minorities <- wide_minorities %>%
  mutate(across(starts_with("prop"), ~./100)) %>%
  mutate(prop_nw_2000 = (Total_Pop_2000 - White_Pop_2000) / Total_Pop_2000, 
         prop_nw_2015 = (Total_Pop_2015 - White_Pop_2015) / Total_Pop_2015, 
         Dnw = (Total_Pop_2015 - White_Pop_2015) - (Total_Pop_2000 - White_Pop_2000) / Total_Pop_2000, 
         Dhisp = (Hispanic_Pop_2015 - Hispanic_Pop_2000) / Total_Pop_2000, 
         Dblack = (Black_Pop_2015 - Black_Pop_2000) / Total_Pop_2000, 
         Dwhite = (White_Pop_2015 - White_Pop_2000) / Total_Pop_2000)
  
# 2) Encontrar tippings

tipping <- function(df, minority){
  
  #Modelo
  rsquared_values <- numeric(60) 
  for (i in 1:60) {
    
    df1 <- df %>%
      mutate(dummy = ifelse(eval(parse(text = paste0("prop_", minority, "_2000"))) > i/100, 1, 0))
    
    regression_model <- lm(Dwhite ~ dummy, data = df1)
    
    rsquared_values[i] <- summary(regression_model)$r.squared
  }
  
  #Max R2
  max_rsquared <- max(rsquared_values)
  best_percentage <- which(rsquared_values == max_rsquared) 
  
  if (length(best_percentage) >1) {
    best_percentage = min(best_percentage)
  }
  
  best_percentage
  
}  

plot_tipping <- function(tipping, df, minority, xlab){
  
  plot <- ggplot(df, aes(x = eval(parse(text = paste0("prop_", minority, "_2000"))), y = Dwhite)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = mean(df$Dwhite, na.rm = TRUE), color = "darkblue", linetype = "dashed") +
    geom_vline(xintercept = tipping/100, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", span = 0.5, kernel = "epanechnikov", formula = y ~ x, fullrange = FALSE, level = 0) +
    labs(x = xlab, y = "Cambio en la población blanca 2000 - 2015") +
    theme_minimal() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-.5, .2))
  
  plot
}
 

#Non white
nw_tip <- tipping(wide_minorities, "nw")
plot_nw <- plot_tipping(nw_tip, wide_minorities, "nw", "Proporción de no blancos en el 2000")
ggsave("../views/tipping/nw_tip_2000-2015.png", plot_nw, dpi = 300)

#Black
black_tip <- tipping(wide_minorities, "black")
plot_black <- plot_tipping(black_tip, wide_minorities, "black", "Proporción de afroamericanos en el 2000")
ggsave("../views/tipping/black_tip_2000-2015.png", plot_black, dpi = 300)

#Hispanic
hisp_tip <- tipping(wide_minorities, "hispanic")
plot_hisp <- plot_tipping(hisp_tip, wide_minorities, "hispanic", "Proporción de hispanos en el 2000")
ggsave("../views/tipping/hispanic_tip_2000-2015.png", plot_black, dpi = 300)



# Mapas -------------------------------------------------------------------

#Usemos el tip de 2000 - 2015 

nw_tip_2015 <- 1
black_tip_2015 <- 1
hisp_tip_2015 <- 12


wide_minorities <- wide_minorities %>%
  mutate(dum_nw_tip = ifelse(prop_nw_2000 >= nw_tip_2015/100, 1, 0), 
         dum_black_tip = ifelse(prop_black_2000 >= black_tip_2015/100, 1, 0),
         dum_hisp_tip = ifelse(prop_hispanic_2000 >= hisp_tip_2015/100, 1, 0))


#Nw
map_tip_nw <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_nw_tip))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_nw.png", map_tip_nw, dpi = 300)

#Black
map_tip_black <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_black_tip))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_black.png", map_tip_black, dpi = 300)

#Hisp
map_tip_hispanic <- ggplot(wide_minorities) +
  geom_sf(aes(fill = factor(dum_hisp_tip))) +
  scale_fill_manual(values = c("1" = "#D73027", "0" = "lightblue")) +
  labs(fill = "Tracts y tipping\n(1 a la derecha del tipping)") + 
  theme_void()

ggsave("../views/tipping/map_tipping_hispanic.png", map_tip_hispanic, dpi = 300)

