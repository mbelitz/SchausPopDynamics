library(tidyverse)

preds <- read.csv(file = "data/weatherData/predictor_vars.csv")

prec <- ggplot(filter(preds, year >= 1985 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = prcp_dry), color = "darkgoldenrod") +
  geom_smooth(aes(y = prcp_dry), method = "lm", color = "darkgoldenrod") +
  geom_path(aes(y = prcp_rainy), color = "royalblue") +
  geom_smooth(aes(y = prcp_rainy), method = "lm", color = "royalblue") +
  geom_text(aes(label = "Rainy season", x = 1990, y = 1250), color = "royalblue", size = 3) +
  geom_text(aes(label = "Dry season", x = 2000, y = 250), color = "darkgoldenrod", size = 3) +
  labs(x = "Year", y = "Precipitation (mm)") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
prec

temp <- ggplot(filter(preds, year >= 1985 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = tmax_larvae), color = "red") +
  geom_smooth(aes(y = tmax_larvae), method = "lm", color = "red") +
  geom_path(aes(y = tmin_larvae), color = "blue") +
  geom_smooth(aes(y = tmin_larvae), method = "lm", color = "blue") +
  geom_text(aes(label = "Tmax", x = 2000, y = 30), color = "red", size = 3) +
  geom_text(aes(label = "Tmin", x = 2000, y = 22), color = "blue", size = 3) +
  labs(x = "Year", y = "Temperature °C") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
temp

maxWind <- ggplot(filter(preds, year >= 1985 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = max_wind)) +
  geom_smooth(aes(y = max_wind), method = "lm") + 
  labs(x = "Year", y = "Maximum windspeed") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
maxWind

EWE <- ggplot(filter(preds, year >= 1985 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = EWE_wind)) +
  geom_smooth(aes(y = EWE_wind), method = "lm") + 
  labs(x = "Year", y = "Number of extreme \n wind events") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
EWE


cp <- cowplot::plot_grid(temp, prec, EWE, maxWind, 
                         labels = c("B", "C", "D", "E"),
                         ncol = 2, nrow = 2, 
                         label_y = 1.01)

cp


# add in population estimate
library(data.table)
tbl_fread <- 
  list.files(path = "data/populationEstimates", full.names = T) %>% 
  map_df(~fread(.))

tbl_fread2 <- tbl_fread %>% 
  dplyr::filter(year != 2019,
                year != 2009) %>% 
  mutate(dif = ucl - lcl)

popEstP <- ggplot(tbl_fread2) +
#  geom_smooth(mapping = aes(x = year, y = estimate), method = "glm.nb", 
#              linetype = "dashed", color = "grey10", fill = "grey80", alpha = 0.7) +
#  geom_rect(data = rects, 
#            mapping = aes(xmin = xstart, xmax = xend, 
#                          ymin = 0, ymax = Inf),
#            fill = "snow3", alpha = 0.4) +
  geom_point(mapping = aes(x = year, y = estimate)) +
  geom_line(mapping = aes(x = year, y = estimate), alpha = 0.3) +
  geom_errorbar(mapping = aes(x = year, ymin = lcl, ymax = ucl)) +
  scale_y_log10() +
  labs(x = "Year", y = "Population estimate") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
popEstP

popEstP2 <- popEstP + 
  geom_vline(xintercept = 2010, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 2005, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 1992, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 1991, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 1986, linetype = "dotted", color = "#219ebc") +
  
  geom_label(aes(x = 2010, y = 10500, label = "Bonnie\nTS"), fill = "white") + 
  geom_label(aes(x = 2004.5, y = 60, label = "Katrina\nH1"), fill = "white") + 
  geom_label(aes(x = 1991, y = 10500, label = "Fabian\nTS"), fill = "white") +
  geom_label(aes(x = 1992.5, y = 60, label = "Andrew\nH5"), fill = "white") +
  geom_label(aes(x = 1986, y = 60, label = "Floyd\nH1"), fill = "white") 
  

popEstP3 <- popEstP + 
  geom_vline(xintercept = 2017, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 2005, linetype = "dotted", color = "#219ebc") +
  geom_vline(xintercept = 1992, linetype = "dotted", color = "#219ebc") +
  
  geom_label(aes(x = 2017, y = 50, label = "Irma"), fill = "white") + 
  geom_label(aes(x = 2005, y = 50, label = "Wilma"), fill = "white") + 
  geom_label(aes(x = 1992, y = 50, label = "Andrew"), fill = "white") 

popEstP3

rects <- data.frame(release = c(1,2),
                    xstart = c(1995,2014),
                    xend = c(1997,2021))

popEstP3 
  
cp2 <- cowplot::plot_grid(popEstP3, cp, 
                          nrow = 2, ncol = 1, labels = c("A", ""),
                          rel_heights = c(0.7,1))

ggsave(filename = "Figures/pop_est&weatherCovariates_annotated.png", plot = cp2,
       width = 7, height = 8, dpi = 450)
