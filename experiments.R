########## Experiments: Min 10 obs ##########

## Packages ##
library(parallel)

source("R/parametric_models.R")

start_time = Sys.time()

set.seed(123)

dt_scalar = readRDS("data/treated/scalar/scalar_features_10obs.rds")
dt_rate = readRDS("data/treated/rates/poisson_rates_10obs.rds")
dt_curves = readRDS("data/treated/wallets_log_smoothed_balanced_10obs.rds")

###comment
# addr_samples = readRDS("data/treated/balanced samples/group_service_exchange_mixer_faucet_10obs.rds")
# dt_scalar = dt_scalar[address %in% unlist(addr_samples)]
# dt_scalar[category %in% c("Mixer Service", "Payment System", "Faucet"), category := "Service"]
# dt_scalar[category %in% c("Broker", "CEX", "DEX"), category := "Exchange"]
# #dt_scalar[category == "Faucet", category := "Mining"]
# dt_rate = dt_rate[address %in% unlist(addr_samples)]
# dt_rate[category %in% c("Mixer Service", "Payment System", "Faucet"), category := "Service"]
# dt_rate[category %in% c("Broker", "CEX", "DEX"), category := "Exchange"]
# #dt_rate[category == "Faucet", category := "Mining"]
# dt_curves = dt_curves[address %in% unlist(addr_samples)]
# dt_curves[category %in% c("Mixer Service", "Payment System", "Faucet"), category := "Service"]
# dt_curves[category %in% c("Broker", "CEX", "DEX"), category := "Exchange"]
# #dt_curves[category == "Faucet", category := "Mining"]

## Parallel experiment function

parallel_experiment = function(param_list, dt_curves, dt_rate, dt_scalar){
  min_obs = param_list[["min_obs"]]
  n_fpcs = param_list[["n_fpcs"]]
  lambda_smooth = param_list[["lambda_smooth"]]
  binary = param_list[["binary"]]
  exp = experiment(dt_curves, dt_rate, dt_scalar, min_obs, n_fpcs, 10^(-lambda_smooth), binary)
  saveRDS(exp, paste0("models/multiclass/min_obs_", min_obs, "/lambda", lambda_smooth, "_nfpcs", n_fpcs,".rds"))
  return(NULL)
}

param_list_10obs = list()
i = 1
#for (lambda_smooth in c(1,10)){
for (lambda_smooth in c(1)){ #
  for (n_fpcs in c(1,3,5,7)){
    param_list_10obs[[i]] = list("min_obs" = 10, "lambda_smooth" = lambda_smooth, "n_fpcs" = n_fpcs, "binary" = FALSE)
    i = i + 1
  }
}

start_time = Sys.time()
mclapply(param_list_10obs, parallel_experiment, copy(dt_curves), copy(dt_rate), copy(dt_scalar))
print(Sys.time() - start_time)

dt_scalar = readRDS("data/treated/scalar/scalar_features_20obs.rds")
dt_rate = readRDS("data/treated/rates/poisson_rates_20obs.rds")
dt_curves = readRDS("data/treated/wallets_log_smoothed_balanced_20obs.rds")

param_list_20obs = list()
i = 1
#for (lambda_smooth in c(1),10){
for (lambda_smooth in c(1)){
  for (n_fpcs in c(1,3,5,7)){
    param_list_20obs[[i]] = list("min_obs" = 20, "lambda_smooth" = lambda_smooth, "n_fpcs" = n_fpcs, "binary" = FALSE)
    i = i + 1
  }
}

start_time = Sys.time()
mclapply(param_list_20obs, parallel_experiment, copy(dt_curves), copy(dt_rate), copy(dt_scalar))
print(Sys.time() - start_time)
