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
  mutate(lambda = estimate/lag(estimate)) %>% 
  mutate(r = log(estimate / lag(estimate))) %>% 
  mutate(densEst = lag(estimate)) %>% 
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
  mutate(
    tmax_adult = scale(tmax_adult),
    tmax_adult.m1 = scale(tmax_adult.m1),
    tmin_adult = scale(tmin_adult),
    tmin_adult.m1 = scale(tmin_adult.m1),
    tmax_larvae = scale(tmax_larvae),
    tmax_larvae.m1 = scale(tmax_larvae.m1),
    tmin_larvae = scale(tmin_larvae),
    tmin_larvae.m1 = scale(tmin_larvae.m1),
    tmax_pupae = scale(tmax_pupae),
    tmax_pupae.m1 = scale(tmax_pupae.m1),
    tmin_pupae = scale(tmin_pupae),
    tmin_pupae.m1 = scale(tmin_pupae.m1),
    prcp_rainy = scale(prcp_rainy),
    prcp_rainy.m1 = scale(prcp_rainy.m1),
    prcp_dry = scale(prcp_dry),
    prcp_dry.m1 = scale(prcp_dry.m1), 
    EWE_wind.m1 = scale(EWE_wind.m1),
    max_wind.m1 = scale(max_wind.m1),
    EWE_wind.m2 = scale(EWE_wind.m2),
    max_wind.m2 = scale(max_wind.m2),
    EWE_wind.m3 = scale(EWE_wind.m3),
    max_wind.m3 = scale(max_wind.m3),
    EWE_wind.m4 = scale(EWE_wind.m4),
    max_wind.m4 = scale(max_wind.m4)
  )



gm_all <- glm.nb(round(estimate) ~ 
                   tmax_adult +
                   tmax_adult.m1 + 
                   tmin_adult +
                   tmin_adult.m1 + 
                   tmax_larvae +
                   tmax_larvae.m1 +
                   tmin_larvae +
                   tmin_larvae.m1 +
                   #    tmax_pupae + 
                   #   tmax_pupae.m1 +
                   #   tmin_pupae +
                   tmin_pupae.m1 +
                   prcp_rainy +
                   prcp_rainy.m1 +
                   prcp_dry +
                   prcp_dry.m1 +
                   EWE_wind.m1 +
                   max_wind.m1 +
                   EWE_wind.m2 +
                   max_wind.m2 +
                   EWE_wind.m3 +
                   max_wind.m3 +
                   EWE_wind.m4 +
                   max_wind.m4,
                 weights =SampleDays,
                 data = mdf2,
                 na.action = "na.fail")

dd_all <- dredge(gm_all, m.lim = c(2,2))

selection_table <- model.sel(dd_all)
selection_table2 <- get.models(dd_all, delta <= 2)
selection_table2

topModel <-  glm.nb(round(estimate) ~ 
                      max_wind.m1 + max_wind.m4,
                    weights =SampleDays,
                    data = mdf2,
                    na.action = "na.fail")
summary(topModel)
r.squaredGLMM(topModel)

car::vif(topModel)
library(DHARMa)
s <- DHARMa::simulateResiduals(topModel)
DHARMa::testResiduals(s) ## looks good 
testDispersion(s)
testOutliers(s)
DHARMa::plotQQunif(s)
testResiduals(s)

m_estimates <- as.data.frame(topModel$coefficients)
m_estimates
m_ci <- confint(topModel)

model_outputs <- as.data.frame(m_ci) %>% 
  tibble::rownames_to_column() %>% 
  mutate(estimate = m_estimates$`topModel$coefficients`)

model_outputs$estimate <- as.numeric(model_outputs$estimate)


plot_df <- model_outputs %>% 
  filter(rowname != "(Intercept)")

plot_df <- plot_df %>% 
  mutate(rowname2 = case_when(
    rowname == "max_wind.m4" ~ "MaxWind[t-4]",
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


# growth rate modeling
growth_rate <- left_join(growth_rate,preds)

growth_rate_scaled <- growth_rate %>% 
  mutate(max_wind.m1 = scale(max_wind.m2),
         max_wind.m4 = scale(max_wind.m4),
         lagEst = scale(densEst))

# growth rate model given predictors within delta < 2 of population estimate model
growth_rate_model_overall <- lm(r ~ 
                                  max_wind.m1 + max_wind.m4 +
                                  lagEst, 
                                data = growth_rate_scaled,
                                na.action = "na.fail")

dd_growthRate <- dredge(growth_rate_model_overall, m.lim = c(2,2))

selection_table_growthRate  <- model.sel(dd_growthRate)
selection_table2_growthRate  <- get.models(dd_growthRate, delta <= 2)
selection_tablegrowthRate  <- as.data.frame(selection_table_growthRate) %>% 
  mutate(cumweight = cumsum(weight)) 

# save selection table for growth rate models (15)
st_toSave_growthRate <- selection_tablegrowthRate %>% 
  mutate(lagEst = if_else(is.na(lagEst), "", "PopSize[t-1]"),
         max_wind = if_else(is.na(max_wind.m1), "", "MaxWind[t-4]"),
         max_wind.m4 = if_else(is.na(max_wind.m4), "", "MaxWind[t-4]")
  ) %>% 
  mutate(Model = paste(lagEst, max_wind, max_wind.m4, sep = ""))

#write.csv(x = st_toSave_growthRate, file = "Tables/revision/densDependentPopDynamics_15CompetingModels_v2.csv", row.names = F)


# retain estiamtes delta <=2
selection_table_d2_growthRate <- as.data.frame(selection_table_growthRate) %>% 
  dplyr::filter(delta <= 2)
colnames(selection_table_d2_growthRate)

# does the top model fit model assumptions okay?
tm <- lm(r ~ max_wind + 
           lagEst, 
         data = growth_rate_scaled,
         na.action = "na.fail")
car::vif(tm)
s <- DHARMa::simulateResiduals(tm)
testDispersion(s)
testOutliers(s)
DHARMa::plotQQunif(s)
testResiduals(s)
d2 <- DHARMa::testResiduals(s) ## looks good 


m_estimates_growthRate <-selection_table_d2_growthRate
m_estimates_growthRate
m_ci_growthRate <- confint(selection_table2_growthRate[[1]])

model_outputs_growthRate <- as.data.frame(m_ci_growthRate) %>% 
  tibble::rownames_to_column() %>% 
  mutate(estimate = c(m_estimates_growthRate$`(Intercept)`,
                      m_estimates_growthRate$lagEst,
                      m_estimates_growthRate$max_wind.m1))

model_outputs_growthRate$estimate <- as.numeric(model_outputs_growthRate$estimate)

# for table

m2 <- lm(r ~  max_wind.m4 + lagEst, 
         data = growth_rate_scaled,
         na.action = "na.fail")

confint(m2)

m3 <- lm(r ~ 
           max_wind.m1 + max_wind.m4, 
         data = growth_rate_scaled,
         na.action = "na.fail")

ci_m3 <- confint(m3)


# plot the parameter estimates

plot_df_growthRate <- model_outputs_growthRate %>% 
  filter(rowname != "(Intercept)") 

plot_df_growthRate <- plot_df_growthRate %>% 
  mutate(rowname2 = case_when(
    rowname == "max_wind.m4" ~ "MaxWind[t-4]",
    rowname == "max_wind.m1" ~ "MaxWind[t-1]",
    rowname == "lagEst" ~ "PopSize[t-1]"
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


## combine plots together
library(ggpubr)

ces <- ggarrange(
  ce_plot + theme(plot.margin = margin(.8,0,0,0, "cm")), 
  ce_plot_growthRate, 
  labels = c("A) Population size", "B) Growth rate"),
  ncol = 1, nrow = 2, align = "hv", label.x = 0, label.y = 1,
  vjust = 1.05, hjust = -0.05)

ggsave(filename = "Figures/Figure4_coefficientEstimates.png",
       plot = ces, width = 5, height = 6)
