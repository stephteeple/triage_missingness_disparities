

# functions
source("./code/functions.R") 
source("./code/bootstrap.R")
source("./code/missingness_manip.R")
source("./code/predict.R")
source("./code/gen_metrics.R")

# libraries
library(foreach)
library(Matrix)
library(glmnet)
library(tictoc)
library(RODBC)
library(data.table)
library(pROC)
library(coefplot)
library(stringr)
library(dplyr)
library(randomForest)
library(ranger)
library(pdp)
library(caTools)
library(fastDummies)
library(cowplot)





# Data/inputs -----------------------------------------------------------------------

test_orig <- fread("./data/test_clean.csv")
train_orig <- fread("./data/train_clean.csv")
active_triage_only <- fread("./data/probls_active_at_triage.csv")
mmargs <- fread("./data/make_metrics_args.csv")


B <- 500 # Bootstrap iters
datetime <- format(Sys.time(), "%Y-%m-%d_%H.%M")


# Analysis --------------------------------------------------------------------------

    
# Create dir for bootstrapped results 
dir.create(file.path("./data/", datetime))

run_boot <- function(i) {
    
    traintest <- func_02_bootstrap(i, train = train_orig, test = test_orig, pointest = TRUE)
    fits_and_imps <- func_03_fits(traintest = traintest, fit_summaries = FALSE)
    obs_manip <- func_04_missingness(traintest = traintest, fits_and_imps = fits_and_imps,
                                 active_triage_only = active_triage_only, 
                                 manipulation_type = "all", exclude = FALSE,
                                 exclude_vars = NULL, manipulation_check = FALSE)
    preds <- func_05_predict(obs_manip = obs_manip, fits_and_imps = fits_and_imps,
                             pred_summaries = FALSE, complaint = NULL)
    metrics <- func_06_gen_metrics(i, preds = preds, mmargs = mmargs, metrics_spot_check = FALSE, 
                                   timestamp = datetime)

    return(metrics)

}
tic()
results <- rbindlist(lapply(1:B, run_boot))
toc() 

fwrite(results, file = paste0("./data/results_B_", B, "_", datetime, ".csv"))

