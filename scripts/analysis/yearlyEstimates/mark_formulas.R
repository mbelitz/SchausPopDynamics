## model formulas
## survival: uniform or differing between sexes
Phidot <- list(formula = ~ 1)
Phisex <- list(formula = ~ Sex)
## resighting: uniform, differing between sexes, or differing by observer
pdot <- list(formula = ~ 1)
psex <- list(formula = ~ Sex)
ptime <- list(formula = ~ time)
pobs <- list(formula = ~ Primary.Observer)
pLL <- list(formula = ~ listLength)
psexObs <- list(formula = ~ Sex + Primary.Observer)
psexLL <- list(formula = ~ Sex + listLength)
pobsLL <- list(formula = ~ Primary.Observer + listLength)
psexObsLL <- list(formula = ~ Sex + Primary.Observer + listLength)
## observer-sex interaction
pSxOb <- list(formula = ~ Primary.Observer:Sex)
## probability of entry: constant
pentdot <- list(formula = ~ 1)
## population: combined or by sex
Ndot <- list(formula = ~ 1)
Nsex <- list(formula = ~ Sex)
