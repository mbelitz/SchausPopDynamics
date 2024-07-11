source("scripts/analysis/setUpData.R")
source("scripts/analysis/mark_formulas.R")

# estimate population size for each year
mrr_1985 <- mrr %>% 
  filter(year == 1985)%>% 
  arrange(doy)

ch_1985 <- mrr_1985 %>% 
  dplyr::select(doy, year, Sex, Tag..) %>% 
  dplyr::filter(Tag.. != "N/A",
                Tag.. != "",
                Tag.. != "NA ",
                Tag.. != "f",
                Tag.. != "p",
                Tag.. != "?") %>% 
  dplyr::rename(Tag = Tag..) %>% 
  dplyr::mutate(detect = 1) %>% 
  dplyr::filter(!is.na(Tag))  %>%
  distinct() %>%
  spread(doy, detect, fill = 0) %>% 
  unite("ch", 4:tail(names(.),1), sep = "") 

ch_1985$Sex <- na_if(ch_1985$Sex, "")

raw_1985 <- mrr_1985 %>% 
  dplyr::filter(Tag.. != "N/A",
                Tag.. != "",
                Tag.. != "NA ",
                Tag.. != "f",
                Tag.. != "p",
                Tag.. != "?") %>% 
  dplyr::rename(Tag = Tag..) %>% 
  filter(!is.na(Tag))

ti <- diff(unique(raw_1985$doy))
startDoy <- min(unique(raw_1985$doy))


popan <- process.data(as_tibble(ch_1985), 
                      model = "POPAN",
                      time.intervals = ti, 
                      begin.time = startDoy, 
                      groups = "Sex")
popan_dd <- make.design.data(popan)
mark_model_sex <- mark(popan, popan_dd,  
                       model.parameters = list(Phi = Phisex, p = psex,
                                               pent = pentdot, N = Nsex))

# we don't have any survey_info, except for one day, so skipping that step

# check out new models, see what is best
mark_model <- mark(popan, popan_dd,  
                   model.parameters = list(Phi = Phidot, p = pdot,
                                           pent = pentdot, N = Ndot))

mark_model_sex <- mark(popan, popan_dd,  
                       model.parameters = list(Phi = Phisex, p = psex,
                                               pent = pentdot, N = Nsex))

mark_model_sex_Ndot <- mark(popan, popan_dd,  
                            model.parameters = list(Phi = Phisex, p = psex,
                                                    pent = pentdot, N = Ndot))


# write DataFrame of model outputs
out_df <- data.frame(
  
  model = c("InterceptOnly",
            "sex",
            "sex_NDot"),
  
  AIC = c(mark_model$results$AICc,
          mark_model_sex$results$AICc,
          mark_model_sex_Ndot$results$AICc)
)



# top model is sex_Ndot, so model that includes sex
mark_model$results$real[1:14,]
mark_model$results$derived$`Gross N* Population Size`
out <- sapply(mark_model$results$derived$`Gross N* Population Size`, sum)
out
# model summary
ms <- summary(mark_model)

tdf <- data.frame(
  estimate = out[[1]],
  se = out[[2]],
  lcl = out[[3]],
  ucl = out[[4]],
  SampleDays = length(unique(mrr_1985$doy)),
  markedIndividuals = nrow(ch_1985),
  phi.Female = mean(ms$reals$Phi[1,]),
  phi.Male = mean(ms$reals$Phi[2,]),
  p.Female = mean(ms$reals$p[1,]),
  p.Male = mean(ms$reals$p[2,]),
  year = 1985
)
head(tdf)

# model has too high of CI, do not include
write.csv(x = tdf, file = "populationEstimates/popEst_1985.csv", row.names = F)
