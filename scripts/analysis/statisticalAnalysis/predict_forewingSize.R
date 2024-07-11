library(dplyr)
library(ggplot2)
library(boot)
library(MuMIn)
library(lme4)
library(lmerTest)
library(sjPlot)

# read in data with forewing length
df <- read.csv("data/forewingSize/SchausForewingData.csv") %>% 
  mutate(year = lubridate::year(lubridate::dmy(Date)))

# remove outliers and things that don't have sex ID
fw_df <- df %>% 
  filter(!is.na(FW.Size)) %>% 
  filter(Sex != "") %>% 
  filter(FW.Size > 7)


# get bootstrapped mean and ci for mean FW by year
meanf <- function(d, i){
  m <-  mean(d[i])
  return(m)
} 

mean_ci <- function(x){
  
  b <- boot(data = x, statistic = meanf, R = 1000)
  b_ci <- boot.ci(b, conf = 0.95, type = "bca")
  
  return(b_ci)
  
}

b <- boot(data = c(1,2,5,8,9), statistic = meanf, R = 1000)

b_ci <- boot.ci(b, conf = 0.95, type = "bca")

b_ci$bca

gb <- fw_df %>% 
  group_by(Sex, year) %>% 
  summarise(count = n(), 
            meanFW = mean(FW.Size),
            FW.ci.low = mean_ci(FW.Size)$bca[4],
            FW.ci.high = mean_ci(FW.Size)$bca[5])

## bring in weather data
# read in preds
preds <- read.csv(file = "data/weatherData/predictor_vars.csv")

#create lags in preds
preds <- preds %>% 
  mutate(tmax_adult.m1 = lag(tmax_adult),
         tmin_adult.m1 = lag(tmin_adult),
         prcp_adult.m1 = lag(prcp_adult),
         
         tmax_larvae.m1 = lag(tmax_larvae),
         tmin_larvae.m1 = lag(tmin_larvae),
         
         prcp_dry.m1 = lag(prcp_dry),
         prcp_rainy.m1 = lag(prcp_rainy),
         
         EWE_wind.m1 = lag(EWE_wind),
         max_wind.m1 = lag(max_wind)
         
  )


fw_df_lj <- left_join(fw_df, preds) %>% 
  dplyr::select(Sex, year, FW.Size, tmax_adult.m1, tmax_larvae, 
         prcp_dry, prcp_dry.m1, prcp_rainy, prcp_rainy.m1,
         max_wind.m1, EWE_wind.m1) %>% 
  mutate(tmax_adult.m1 = scale(tmax_adult.m1),
         tmax_larvae = scale(tmax_larvae),
         prcp_dry = scale(prcp_dry),
         prcp_dry.m1 = scale(prcp_dry.m1),
         prcp_rainy = scale(prcp_rainy),
         prcp_rainy.m1 = scale(prcp_rainy.m1), 
         max_wind.m1 = scale(max_wind.m1),
         EWE_wind.m1 = scale(EWE_wind.m1))

cor(fw_df_lj$tmax_adult.m1, fw_df_lj$tmax_larvae) # good
cor(fw_df_lj$prcp_dry, fw_df_lj$prcp_rainy) # good
cor(fw_df_lj$prcp_dry, fw_df_lj$prcp_dry.m1) # good
cor(fw_df_lj$prcp_rainy, fw_df_lj$prcp_rainy.m1) # good
cor(fw_df_lj$prcp_rainy.m1, fw_df_lj$prcp_dry.m1) # good
cor(fw_df_lj$max_wind.m1, fw_df_lj$EWE_wind.m1) # good


m1 <- lm(formula = FW.Size ~ 
              tmax_larvae +
             prcp_rainy.m1 +
               Sex, 
                     na.action = "na.fail",
                     data = fw_df_lj)
m1sum <- summary(m1)
ci <- confint(m1)
model_table <- as.data.frame(ci) %>% 
  mutate(parameters = rownames(m1sum$coefficients))

r.squaredGLMM(m1)
model_table

## what is the r2 if sex is removed
m2 <- lm(formula = FW.Size ~ 
           tmax_larvae +
           prcp_rainy.m1, 
         na.action = "na.fail",
         data = fw_df_lj)

r.squaredGLMM(m2)

#write.csv(x = model_table, file = "Tables/ForewingSizeModel.csv", row.names = F)

tmax_plot_avg <- plot_model(m1, type = "eff", terms = c("tmax_larvae", "Sex"))$data

tmax_larvae_plot_avg <- plot_model(m1, type = "eff", terms = c("tmax_larvae", "Sex"))$data

tmax_larvae <- ggplot() +
  geom_point(fw_df_lj, mapping = aes(x = tmax_larvae, y = FW.Size, color = Sex), position = position_jitter(width = 0.1, height = 0.1)) +
  geom_line(tmax_larvae_plot_avg, 
            mapping = aes(x = x, y = predicted, color = group_col)) +
  geom_ribbon(tmax_larvae_plot_avg,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, 
                            fill = group_col), alpha = 0.3) +
  labs(color = "Sex", fill = "Sex", x = "Tmax (Larvae)", y = "Forewing length (mm)") +
  scale_color_manual(values = c("#001219", "#ffd60a")) +
  scale_fill_manual(values = c("#001219", "#ffd60a")) +
  ggtitle("B") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        title = element_text(size = 14, face = "bold"), 
        axis.title=element_text(size=13.5, face = "plain"),
        legend.title = element_text(size = 12, face = "plain")
  )

tmax_larvae


prec_plot <- plot_model(m1, type = "eff", terms = c("prcp_rainy.m1", "Sex"))$data

prec <- ggplot() +
  geom_point(fw_df_lj, mapping = aes(x = prcp_dry.m1, y = FW.Size, color = Sex), position = position_jitter(width = 0.1, height = 0.1)) +
  geom_line(prec_plot, 
            mapping = aes(x = x, y = predicted, color = group_col)) +
  geom_ribbon(prec_plot,
              mapping = aes(x = x, ymin = conf.low, ymax = conf.high, 
                            fill = group_col), alpha = 0.3) +
  labs(color = "Sex", fill = "Sex", x = "Precip (Wet [t-1])", y = "Forewing length (mm)") +
  scale_color_manual(values = c("#001219", "#ffd60a")) +
  scale_fill_manual(values = c("#001219", "#ffd60a")) +
  ggtitle("C") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        title = element_text(size = 14, face = "bold"), 
        axis.title=element_text(size=13.5, face = "plain"),
        legend.title = element_text(size = 12, face = "plain")
  )

prec

var <- ggplot(gb, mapping = aes(x = year, y = meanFW)) +
  geom_errorbar(aes(ymin = FW.ci.low, ymax = FW.ci.high,
                    color = Sex), alpha = 0.7) +
  geom_point(aes(color = Sex)) +
  geom_line(aes(color = Sex)) +
  scale_color_manual(values = c("#001219", "#ffd60a")) +
  scale_fill_manual(values = c("#001219", "#ffd60a")) +
  labs(x = "Year", y = "Forewing length (mm)") +
  ggtitle("A") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        title = element_text(size = 14, face = "bold"), 
        axis.title=element_text(size=13.5, face = "plain"),
        legend.title = element_text(size = 12, face = "plain")
  )

var

#ggsave(filename = "Figures/Mansucript/ForwewingSize.png", plot = var, width = 5,height = 4)

# grob these together
library(gridExtra)
layout <- rbind(c(1,1),
            c(1,1),
            c(1,1),
            c(2,3),
            c(2,3))
  
ga <- grid.arrange(var, tmax_larvae, prec,
                   layout_matrix = layout
                    
)

ggsave(filename = "Figures/ForewingModels.png", plot = ga, width = 8, height = 8)

#library(ggpubr)
#ga2 <- ggarrange(var, 
#                 tmax_adult, tmax_larvae,
#                 prec, wind,
#                 ncol = 2, nrow = 3,
#                 heights = c(1,0.75,0.75))
#ggsave(filename = "Figures/Mansucript/ForewingModels2.png", plot = ga2, width = 8, height = 8)
