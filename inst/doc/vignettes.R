## ----setup, include=FALSE-----------------------------------------------------
# Set the working directory to the project root
knitr::opts_knit$set(root.dir = getwd())

## -----------------------------------------------------------------------------
#Load the relevant packages
library(did) #Callaway and Sant'Anna (2021) 
library(cdid) #Bellego, Benatia, and Dortet-Bernadet (2024)

set.seed(123)
#Generate a dataset: 500 units, 8 time periods, with unit fixed-effects. 
# The parameter sigma_alpha controls the unit-specific time-persistent unobserved
# heterogeneity.
data0=fonction_simu_attrition(N = 500,T = 8,theta2_alpha_Gg = 0.5, 
                              lambda1_alpha_St = 0, sigma_alpha = 2, 
                              sigma_epsilon = 0.1, tprob = 0.5)

# The true values of the coefficients are based on time-to-treatment. The treatment
# effect is zero before the treatment, 1.75 one period after, 1.5 two period after,
# 1.25 three period after, 1 four period after, 0.75 five period after, 0.5 six  
# period after, etc.

#We keep all observations, so we have a balanced dataset
data0$S <- 1

#Look at the data
head(data0,20)

## -----------------------------------------------------------------------------
#run did library on balanced panel
did.results = att_gt(
  yname="Y",
  tname="date",
  idname = "id",
  gname = "date_G",
  xformla = ~X,
  data = data0,
  weightsname = NULL,
  allow_unbalanced_panel = FALSE,
  panel = TRUE,
  control_group = "notyettreated",
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "ipw",
  base_period = "varying",
  print_details = FALSE,
  pl = FALSE,
  cores = 1
)

#run cdid with 2step weighting matrix
result_2step = att_gt_cdid(yname="Y", tname="date",
                         idname="id",
                         gname="date_G",
                         xformla=~X,
                         data=data0,
                         control_group="notyettreated",
                         alp=0.05,
                         bstrap=TRUE,
                         biters=1000,
                         clustervars=NULL,
                         cband=TRUE,
                         est_method="2-step",
                         base_period="varying",
                         print_details=FALSE,
                         pl=FALSE,
                         cores=1)

#run cdid with identity weighting matrix
result_id = att_gt_cdid(yname="Y", tname="date",
                        idname="id",
                        gname="date_G",
                        xformla=~X,
                        data=data0,
                        control_group="notyettreated",
                        alp=0.05,
                        bstrap=TRUE,
                        biters=1000,
                        clustervars=NULL,
                        cband=TRUE,
                        est_method="Identity",
                        base_period="varying",
                        print_details=FALSE,
                        pl=FALSE,
                        cores=1)

## -----------------------------------------------------------------------------
print(did.results)
# Remark that standard errors are smaller for most ATT(g,t) when using cdid
print(result_2step)
print(result_id)

## -----------------------------------------------------------------------------
# Aggregation
agg.es.did <- aggte(MP = did.results, type = 'dynamic')
agg.es.2step <- aggte(MP = result_2step, type = 'dynamic')
agg.es.id <- aggte(MP = result_id, type = 'dynamic')

## -----------------------------------------------------------------------------
# Print results
agg.es.did

## -----------------------------------------------------------------------------
#Remark that standard errors are smaller, notably so for the 2step estimator
agg.es.2step

## -----------------------------------------------------------------------------
agg.es.id

## -----------------------------------------------------------------------------
#Generate a dataset: 500 units, 8 time periods, with unit fixed-effects (alpha)
set.seed(123)
data0=fonction_simu_attrition(N = 500,T = 8,theta2_alpha_Gg = 0.5, 
                              lambda1_alpha_St = 0, sigma_alpha = 2, 
                              sigma_epsilon = 0.1, tprob = 0.5)

#We discard observations based on sampling indicator S
data0 <- data0[data0$S==1,]

#run did
did.results =  att_gt(
  yname="Y",
  tname="date",
  idname = "id",
  gname = "date_G",
  xformla = ~X,
  data = data0,
  weightsname = NULL,
  allow_unbalanced_panel = FALSE,
  panel = FALSE,
  control_group = "notyettreated",
  alp = 0.05,
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  clustervars = NULL,
  est_method = "ipw",
  base_period = "varying",
  print_details = FALSE,
  pl = FALSE,
  cores = 1
)

#run cdid with 2step weighting matrix
result_2step = att_gt_cdid(yname="Y", tname="date",
                         idname="id",
                         gname="date_G",
                         xformla=~X,
                         data=data0,
                         control_group="notyettreated",
                         alp=0.05,
                         bstrap=TRUE,
                         biters=1000,
                         clustervars=NULL,
                         cband=TRUE,
                         est_method="2-step",
                         base_period="varying",
                         print_details=FALSE,
                         pl=FALSE,
                         cores=1)

#run cdid with identity weighting matrix
result_id = att_gt_cdid(yname="Y", tname="date",
                        idname="id",
                        gname="date_G",
                        xformla=~X,
                        data=data0,
                        control_group="notyettreated",
                        alp=0.05,
                        bstrap=TRUE,
                        biters=1000,
                        clustervars=NULL,
                        cband=TRUE,
                        est_method="Identity",
                        base_period="varying",
                        print_details=FALSE,
                        pl=FALSE,
                        cores=1)

## -----------------------------------------------------------------------------
#Note the precision gains
print(did.results)
print(result_2step)
print(result_id)

## -----------------------------------------------------------------------------
# Aggregation
# There are other ways to aggregate, see the did library
agg.es.did <- aggte(MP = did.results, type = 'dynamic')
agg.es.2step <- aggte(MP = result_2step, type = 'dynamic')
agg.es.id <- aggte(MP = result_id, type = 'dynamic')

## -----------------------------------------------------------------------------
#Note the precision gains
agg.es.did

## -----------------------------------------------------------------------------
agg.es.2step

## -----------------------------------------------------------------------------
agg.es.id

