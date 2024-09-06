library(tidyverse)
library(data.table)
library(MuMIn)
library(MASS)
library(sjPlot)

# read in population estimates
tbl_fread <- 
  list.files(path = "data/populationEstimates", full.names = T) %>% 
  map_df(~fread(.))

pop_ests <- tbl_fread %>% 
  dplyr::filter(year != 2019,
                year != 2009) %>% 
  mutate(dif = ucl - lcl)

# get median parameter estimates for models
median(pop_ests$p.Female) #0.0716
median(pop_ests$p.Male) #0.126

median(pop_ests$phi.Female) # 0.653
median(pop_ests$phi.Male) #0.738

# growth rate

growth_rate <- pop_ests %>% 
  mutate(canModel = year - lag(year)) %>% 
  filter(canModel == 1 | year == 1988)
growth_rate <- growth_rate %>% 
  mutate(log_est = log(estimate)) %>% 
  mutate(densLogEst = lag(log_est)) %>% 
  mutate(lambda = estimate/lag(estimate)) %>% 
  mutate(r = log(estimate / lag(estimate))) %>% 
  filter(!is.na(r))

# read in preds
preds <- read.csv(file = "data/weatherData/predictor_vars.csv")

#create lags in preds
preds <- preds %>% 
  mutate(tmax_adult.m1 = lag(tmax_adult),
         tmin_adult.m1 = lag(tmin_adult),
         prcp_adult.m1 = lag(prcp_adult),
         
         tmax_larvae.m1 = lag(tmax_larvae),
         tmin_larvae.m1 = lag(tmin_larvae),
         prcp_larvae.m1 = lag(prcp_larvae),
         
         tmax_pupae.m1 = lag(tmax_pupae),
         tmin_pupae.m1 = lag(tmin_pupae),
      
         prcp_rainy.m1 = lag(prcp_rainy),
         prcp_dry.m1 = lag(prcp_dry),
         
         EWE_wind.m1 = lag(EWE_wind),
         max_wind.m1 = lag(max_wind),
         EWE_wind.m2 = lag(EWE_wind, n = 2),
         max_wind.m2 = lag(max_wind, n = 2),
         EWE_wind.m3 = lag(EWE_wind, n = 3),
         max_wind.m3 = lag(max_wind, n = 3),
         EWE_wind.m4 = lag(EWE_wind, n = 4),
         max_wind.m4 = lag(max_wind, n = 4)
         
         
  )
         
         
         
# combine to model data frame
mdf <- left_join(pop_ests, preds)

mdf2 <- mdf %>% 
  mutate(log_est = log(estimate)) 

# first let's estimate log_est then we will try growth rate estimations
# make global model
# first let's look into temp variables
gm_tmp <- lm(log_est ~ 
         tmax_adult +
         tmax_adult.m1 + 
         tmin_adult +
         tmin_adult.m1 + 
         tmax_larvae +
         tmax_larvae.m1 +
         tmin_larvae +
         tmin_larvae.m1 +
         tmax_pupae + 
         tmax_pupae.m1 +
         tmin_pupae +
         tmin_pupae.m1 ,
         weights =SampleDays,
         data = mdf2,
         na.action = "na.fail")

dd_tmp <- dredge(gm_tmp, m.lim = c(1,1))
models_tmp <- get.models(dd_tmp, subset = delta < 2)
models_tmp # all variables are considered competitive, let's look at correlation matrix
temp_m <- data.frame(  mdf2$tmax_adult,
                       mdf2$tmax_adult.m1, 
                       mdf2$tmin_adult,
                       mdf2$tmin_adult.m1, 
                       mdf2$tmax_larvae,
                       mdf2$tmax_larvae.m1,
                       mdf2$tmin_larvae,
                       mdf2$tmin_larvae.m1,
                       mdf2$tmax_pupae,
                       mdf2$tmax_pupae.m1,
                       mdf2$tmin_pupae,
                       mdf2$tmin_pupae.m1)

cor(temp_m)

# re run models with new selecttions
gm_tmp <- lm(log_est ~ 
               tmax_adult.m1 + 
               tmin_adult.m1 + 
               tmax_larvae +
               tmax_pupae.m1 ,
             weights = SampleDays,
             data = mdf2,
             na.action = "na.fail")

dd_tmp <- dredge(gm_tmp, m.lim = c(1,1))
models_tmp <- get.models(dd_tmp, subset = delta < 2)
models_tmp # all variables are considered competitive, let's look at correlation matrix

# look at correlations again
temp_m <- data.frame(mdf2$tmax_adult.m1, 
                     
                     mdf2$tmin_adult.m1, 
                     mdf2$tmax_larvae,
                    
                     mdf2$tmax_pupae.m1)

cor(temp_m)

# prcp variables
gm_prcp <- lm(log_est ~ 
                prcp_rainy +
                prcp_rainy.m1 +
                prcp_dry +
                prcp_dry.m1,
              weights = SampleDays,
              data = mdf2,
              na.action = "na.fail")
  
dd_prcp <- dredge(gm_prcp, m.lim = c(1,1))
models_prcp <- get.models(dd_prcp, subset = delta < 2)
models_prcp

# top prcp vars are 
# prcp_dry
# precipitation will affect plants, so make this more seasonal not butterfly related

# now wind 
gm_wind <- gm <- lm(log_est ~ 
                    EWE_wind +  
                    max_wind +
                    EWE_wind.m1 +
                    max_wind.m1 +
                    EWE_wind.m2 +
                    max_wind.m2 +
                    EWE_wind.m3 +
                    max_wind.m3 +
                    EWE_wind.m4 +
                    max_wind.m4 ,
                    weights = SampleDays,
                    data = mdf2,
                    na.action = "na.fail")

dd_wind <- dredge(gm_wind, m.lim = c(1,1))
models_wind <- get.models(dd_wind, subset = delta < 2)
models_wind
# retained vars are max_wind.m2, max_wind.m4, EWE_wind.m4
m <- data.frame(mdf2$max_wind.m1, mdf2$max_wind.m4, mdf2$EWE_wind.m4, mdf2$max_wind)

c_df <- cor(m)
c_df

# global model of top individual predictor groups
mdf2_scaled <- mdf2 %>% 
  mutate(tmax_larvae = scale(tmax_larvae),
         tmax_adult.m1 = scale(tmax_adult.m1),
         tmin_adult.m1 = scale(tmin_adult.m1),
         tmax_pupae.m1 = scale(tmax_pupae.m1),
         prcp_dry = scale(prcp_dry),
         max_wind = scale(max_wind),
         max_wind.m1 = scale(max_wind.m2),
         max_wind.m4 = scale(max_wind.m4),
         EWE_wind.m4 = scale(EWE_wind.m4))

gm_overall <- lm(log_est ~ 
                   tmax_larvae + tmin_adult.m1 + tmax_adult.m1 + tmax_pupae.m1 + 
                   prcp_dry +
                   max_wind + max_wind.m1 + max_wind.m4 + EWE_wind.m4, 
   weights = SampleDays, 
   data = mdf2_scaled,
   na.action = "na.fail")

dd_overall <- dredge(gm_overall, m.lim = c(2,2))

selection_table <- model.sel(dd_overall)
selection_table2 <- get.models(dd_overall, delta <= 2)
selection_table <- as.data.frame(selection_table) %>% 
  mutate(cumweight = cumsum(weight)) 

# save selection table of 36 competing models
st_toSave <- selection_table %>% 
  mutate(EWE_wind.m4 = if_else(is.na(EWE_wind.m4), "", "ExtremeWind[t-4]"),
         max_wind = if_else(is.na(max_wind), "", "MaxWind"),
         max_wind.m1 = if_else(is.na(max_wind.m1), "", "MaxWind[t-1]"),
         max_wind.m4 = if_else(is.na(max_wind.m4), "", "MaxWind[t-4]"),
         prcp_dry = if_else(is.na(prcp_dry), "", "Precip[dry]"),
         tmax_adult.m1 = if_else(is.na(tmax_adult.m1), "", "MaxTempAdult[t-1]"),
         tmax_larvae = if_else(is.na(tmax_larvae), "", "MaxTempLarvae"),
         tmax_pupae.m1 = if_else(is.na(tmax_pupae.m1), "", "MaxTempPupae[t-1]"),
         tmin_adult.m1 = if_else(is.na(tmin_adult.m1), "", "MinTempAdult[t-1]"),
  ) %>% 
  mutate(Model = paste(EWE_wind.m4, max_wind, max_wind.m1, max_wind.m4, prcp_dry,
                       tmax_adult.m1, tmax_larvae, tmax_pupae.m1, tmin_adult.m1, sep = ""))

#write.csv(x = st_toSave, file = "Tables/PopDynamics_36CompetingModels.csv", row.names = F)

# retain estiamtes delta <=2
selection_table_d2 <- as.data.frame(selection_table) %>% 
  dplyr::filter(delta <= 2)
colnames(selection_table_d2)

# does the top model fit model assumptions okay?
tm <- lm(log_est ~ max_wind + prcp_dry, 
         weights = SampleDays, 
         data = mdf2_scaled,
         na.action = "na.fail")
car::vif(tm)
library(DHARMa)
s <- DHARMa::simulateResiduals(tm)
DHARMa::testResiduals(s) ## looks good 
testDispersion(s)
testOutliers(s)
DHARMa::plotQQunif(s)
testResiduals(s)

m_estimates <- as.data.frame(model.avg(selection_table2)$coefficients)[2,]
m_estimates
m_ci <- confint(model.avg(selection_table2), full = F)

model_outputs <- as.data.frame(m_ci) %>% 
                  tibble::rownames_to_column() %>% 
  mutate(estimate = as.vector(m_estimates[1,]))

model_outputs$estimate <- as.numeric(model_outputs$estimate)

#write.csv(x = selection_table_d2, file = "Tables/PopulationEstimateCompetingModels.csv", row.names = F)
  
# plot the parameter estimates

plot_df <- model_outputs %>% 
  filter(rowname != "(Intercept)")

plot_df <- plot_df %>% 
  mutate(rowname2 = case_when(
    rowname == "prcp_dry" ~ "Precip[dry]",
    rowname == "max_wind.m4" ~ "MaxWind[t-4]",
    rowname == "tmin_adult.m1" ~ "MinTemp_Adult[t-1]",
    rowname == "max_wind" ~ "MaxWind",
    rowname == "max_wind.m1" ~ "MaxWind[t-1]"
  ))%>% 
  arrange(desc(estimate)) %>%
  mutate(rowname2=fct_reorder(rowname2,estimate)) 

ce_plot <- ggplot(plot_df) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(x = estimate, y = rowname2),size = 3) +
  geom_errorbar(aes(xmin = `2.5 %`, xmax = `97.5 %`,y = rowname2)) +
  labs(x = "Coefficient estimate", y = "") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5)
  )


ce_plot

# what about the variable contribution of full model?
head(selection_table)
# Get weight parameter for each weather class 
wind <- as.data.frame(selection_table) %>% 
  dplyr::select(EWE_wind.m4, max_wind, max_wind.m1, max_wind.m4, weight)
nas_wind <- rowSums(is.na(wind))
wind <- wind %>% 
  mutate(nas = nas_wind)
wind <- wind %>% 
  filter(nas != 4)

temp <- as.data.frame(selection_table) %>% 
  dplyr::select(tmax_adult.m1, tmax_larvae, tmax_pupae.m1, tmin_adult.m1, weight)
nas_temp <- rowSums(is.na(temp))
temp <- temp %>% 
  mutate(nas = nas_temp)
temp <- temp %>% 
  filter(nas != 4)


prec <- as.data.frame(selection_table) %>% 
  dplyr::select(prcp_dry, weight)
nas_prec <- rowSums(is.na(prec))
prec <- prec %>% 
  mutate(nas = nas_prec)
prec <- prec %>% 
  filter(nas != 1)

sum(prec$weight)

weight_df <- data.frame(
  weatherClass = c("Wind", "Precipitation", "Temperature"),
  varImp = c(sum(wind$weight),
             sum(prec$weight),
             sum(temp$weight))
)  %>% 
  arrange(desc(varImp)) %>%
  mutate(weatherClass=fct_reorder(weatherClass,rev(varImp))) 



weather_plot <- ggplot(weight_df, mapping = aes(x = weatherClass, y = varImp, fill = weatherClass)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#f7a738", "#388697", "#9a150e")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  labs(x = "", y = "Probability variable \n is included in best model") + 
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=13.5)
  )

weather_plot

# growth rate modeling
growth_rate <- left_join(growth_rate,preds)

growth_rate_scaled <- growth_rate %>% 
  mutate(tmax_larvae = scale(tmax_larvae),
         tmax_adult.m1 = scale(tmax_adult.m1),
         tmin_adult.m1 = scale(tmin_adult.m1),
         tmax_pupae.m1 = scale(tmax_pupae.m1),
         prcp_dry = scale(prcp_dry),
         max_wind = scale(max_wind),
         max_wind.m1 = scale(max_wind.m2),
         max_wind.m4 = scale(max_wind.m4),
         EWE_wind.m4 = scale(EWE_wind.m4),
         densLogEst = scale(densLogEst))

# growth rate model given predictors within delta < 2 of population estimate model

growth_rate_model_overall <- lm(r ~ 
                   max_wind + max_wind.m4 +
                   max_wind.m1 + prcp_dry + tmin_adult.m1 +
                  densLogEst, 
                 data = growth_rate_scaled,
                 na.action = "na.fail")

dd_growthRate <- dredge(growth_rate_model_overall, m.lim = c(2,2))

selection_table_growthRate  <- model.sel(dd_growthRate)
selection_table2_growthRate  <- get.models(dd_growthRate, delta <= 2)
selection_tablegrowthRate  <- as.data.frame(selection_table_growthRate) %>% 
  mutate(cumweight = cumsum(weight)) 

# save selection table for growth rate models (15)
st_toSave_growthRate <- selection_tablegrowthRate %>% 
  mutate(densLogEst = if_else(is.na(densLogEst), "", "PopSize[t-1]"),
         max_wind = if_else(is.na(max_wind), "", "MaxWind"),
         max_wind.m1 = if_else(is.na(max_wind.m1), "", "MaxWind[t-1]"),
         max_wind.m4 = if_else(is.na(max_wind.m4), "", "MaxWind[t-4]"),
         prcp_dry = if_else(is.na(prcp_dry), "", "Precip[dry]"),
         tmin_adult.m1 = if_else(is.na(tmin_adult.m1), "", "MinTempAdult[t-1]"),
  ) %>% 
  mutate(Model = paste(densLogEst, max_wind, max_wind.m1, max_wind.m4, prcp_dry,
                       tmin_adult.m1, sep = ""))

#write.csv(x = st_toSave_growthRate, file = "Tables/densDependentPopDynamics_15CompetingModels_v2.csv", row.names = F)


# retain estiamtes delta <=2
selection_table_d2_growthRate <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::filter(delta <= 2)
colnames(selection_table_d2_growthRate)

# does the top model fit model assumptions okay?
tm <- lm(r ~ prcp_dry + 
           densLogEst, 
         data = growth_rate_scaled,
         na.action = "na.fail")
car::vif(tm)
s <- DHARMa::simulateResiduals(tm)
testDispersion(s)
testOutliers(s)
DHARMa::plotQQunif(s)
testResiduals(s)
d2 <- DHARMa::testResiduals(s) ## looks good 


m_estimates_growthRate <- as.data.frame(model.avg(selection_table2_growthRate)$coefficients)[2,]
m_estimates_growthRate
m_ci_growthRate <- confint(model.avg(selection_table2_growthRate), full = F)

model_outputs_growthRate <- as.data.frame(m_ci_growthRate) %>% 
  tibble::rownames_to_column() %>% 
  mutate(estimate = as.vector(m_estimates_growthRate[1,]))

model_outputs_growthRate$estimate <- as.numeric(model_outputs_growthRate$estimate)


# plot the parameter estimates

plot_df_growthRate <- model_outputs_growthRate %>% 
  filter(rowname != "(Intercept)") 

plot_df_growthRate <- plot_df_growthRate %>% 
  mutate(rowname2 = case_when(
    rowname == "max_wind.m4" ~ "MaxWind[t-4",
    rowname == "prcp_dry" ~ "Precip[dry]",
    rowname == "max_wind.m1" ~ "MaxWind[t-1]",
    rowname == "densLogEst" ~ "PopSize[t-1]"
  ))%>% 
  arrange(desc(estimate)) %>%
  mutate(rowname2=fct_reorder(rowname2,estimate)) 

ce_plot_growthRate <- ggplot(plot_df_growthRate) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(x = estimate, y = rowname2), size = 3) +
  geom_errorbar(aes(xmin = `2.5 %`, xmax = `97.5 %`, y = rowname2)) +
  labs(x = "Coefficient estimate", y = "") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13.5)
  )

ce_plot_growthRate

# what about the variable contribution of full model?
head(selection_table_growthRate)
# Get weight parameter for each weather class 
wind_growthRate <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::select(max_wind, max_wind.m1, max_wind.m4, weight)
nas_wind_growthRate <- rowSums(is.na(wind_growthRate))
wind_growthRate <- wind_growthRate %>% 
  mutate(nas = nas_wind_growthRate)
wind_growthRate <- wind_growthRate %>% 
  filter(nas != 3)

temp_growthRate <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::select(tmin_adult.m1, weight)
nas_temp_growthRate <- rowSums(is.na(temp_growthRate))
temp_growthRate <- temp_growthRate %>% 
  mutate(nas = nas_temp_growthRate)
temp_growthRate <- temp_growthRate %>% 
  filter(nas != 1)


prec_growthRate <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::select(prcp_dry, weight)
nas_prec_growthRate <- rowSums(is.na(prec_growthRate))
prec_growthRate <- prec_growthRate %>% 
  mutate(nas = nas_prec_growthRate)
prec_growthRate <- prec_growthRate %>% 
  filter(nas != 1)

sum(prec_growthRate$weight)

densDependence <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::select(densLogEst, weight)
nas_densDependence <- rowSums(is.na(densDependence))
densDependence <- densDependence %>% 
  mutate(nas = nas_densDependence)
densDependence <- densDependence %>% 
  filter(nas != 1)

weight_df_growthRate <- data.frame(
  weatherClass = c("Wind", "Precipitation", "Temperature", "Density dependence"),
  varImp = c(sum(wind_growthRate$weight),
             sum(prec_growthRate$weight),
             sum(temp_growthRate$weight),
             sum(densDependence$weight))
) %>% 
  mutate(weatherClass=fct_reorder(weatherClass,varImp)) 

#write.csv(x = selection_table_d2_growthRate, file = "Tables/GrowthRateCompetingModels_v2.csv", row.names = F)


weather_plot_growthRate <- ggplot(weight_df_growthRate, mapping = aes(x = fct_rev(weatherClass), y = varImp, fill = weatherClass)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(c("#aa998f", "#388697","#f7a738","#9a150e"))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  labs(x = "", y = "Probability variable \n is included in best model") + 
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=13.5)
  )
weather_plot_growthRate

## combine plots together
library(ggpubr)

ces <- ggarrange(ce_plot, ce_plot_growthRate, labels = c("B", "D"),ncol = 1, nrow = 2, align = "hv")
wps <- ggarrange(weather_plot, weather_plot_growthRate, labels = c("A", "C"), ncol = 1, nrow = 2, align = "hv")

ga <- ggarrange(wps, ces, widths = c(1,0.9), ncol = 2)





# ga <- ggarrange(weather_plot,ce_plot,
#                 weather_plot_growthRate, ce_plot_growthRate, 
#                 labels = c("A", "B","C", "D"))
 
ga

ggsave(filename = "Figures/populationDynamics.png", plot = ga, width = 8, height = 7)
