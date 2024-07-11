library(tidyverse)

preds <- read.csv(file = "data/weatherData/predictor_vars.csv")

prec <- ggplot(filter(preds, year >= 1986 & year <= 2021)) +
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

temp <- ggplot(filter(preds, year >= 1986 & year <= 2021)) +
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

maxWind <- ggplot(filter(preds, year >= 1986 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = max_wind)) +
  geom_smooth(aes(y = max_wind), method = "lm") + 
  labs(x = "Year", y = "Maximum windspeed") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
maxWind

EWE <- ggplot(filter(preds, year >= 1986 & year <= 2021)) +
  aes(x = year) +
  geom_path(aes(y = EWE_wind)) +
  geom_smooth(aes(y = EWE_wind), method = "lm") + 
  labs(x = "Year", y = "Number of extreme \n wind events") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
EWE


cp <- cowplot::plot_grid(temp, prec, EWE, maxWind, labels = c("B", "C", "D", "E"))

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
  geom_smooth(mapping = aes(x = year, y = estimate), method = "lm", 
              linetype = "dashed", color = "grey10", fill = "grey80", alpha = 0.7) +
  geom_point(mapping = aes(x = year, y = estimate)) +
  geom_line(mapping = aes(x = year, y = estimate), alpha = 0.3) +
  geom_errorbar(mapping = aes(x = year, ymin = lcl, ymax = ucl)) +
  scale_y_log10() +
  labs(x = "Year", y = "Population estimate") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5))
popEstP

cp2 <- cowplot::plot_grid(popEstP, cp, 
                          nrow = 2, ncol = 1, labels = c("A", ""),
                          rel_heights = c(0.7,1))

ggsave(filename = "Figures/pop_est&weatherCovariates.png", 
       width = 6, height = 8, dpi = 450)
