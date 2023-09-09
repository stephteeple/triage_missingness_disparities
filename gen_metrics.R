
func_06_gen_metrics <- function(i, preds, mmargs, metrics_spot_check, timestamp) {


# 1. Get data and prep ---------------------------------------------------------------

obs_preds <- preds$obs_preds
manip_preds <- preds$manip_preds
date_path <- datetime



# Keep only Black and non-Hispanic white patients 
obs_preds <- obs_preds[obs_preds$race == "Black or African American" | 
                      (obs_preds$race == "White or Caucasian" & obs_preds$ethnicity == "Not Hispanic or Latino"),]
manip_preds <- manip_preds[manip_preds$race == "Black or African American" | 
                          (manip_preds$race == "White or Caucasian" & manip_preds$ethnicity == "Not Hispanic or Latino"),]


# Keep only needed vars 
vars <- c("patid", "visitid", "race", "disp_adm", "em_proc", "crit_out", "pred_hosp",
          "pred_emproc", "pred_crit")
obs_preds <- obs_preds[, ..vars]
# obs_preds$iter <- 1
# vars <- c(vars, "iter")
manip_preds <- manip_preds[, ..vars]







# 2. Calculate metrics --------------------------------------------------------------

print("Making metrics")

# Observed
# B <- length(unique(obs_preds$iter))
metrics_obs <- make_metrics(data = obs_preds, args = mmargs)
metrics_obs$data <- "obs"

# Manipulated
# B <- length(unique(manip_preds$iter))
metrics_manip <- make_metrics(data = manip_preds, args = mmargs)
metrics_manip$data <- "manip"

metrics <- rbind(metrics_obs, metrics_manip)
metrics$iter <- i






# 3. Checks -------------------------------------------------------------------------

if (metrics_spot_check == TRUE) {

    mycheckd <- obs_preds[obs_preds$race == "Black or African American"]
    print("Spot checks for metrics (Black patients, hospitalization model")
    
    # Brier score (mean of squared residuals)
    check_brier <- mean((mycheckd$pred_hosp - mycheckd$disp_adm)^2)
    print(paste0("Checking Brier: ", check_brier))
    
    # C-statistic
    # Check with package pROC (use colAUC from caTools in functions.R)
    myroc <- roc(response = mycheckd$disp_adm, predictor = mycheckd$pred_hosp)
    check_auc <- auc(myroc)
    print(paste0("Checking AUC: ", check_auc))

}




# 4. Export ------------------------------------------------------------------------

# Write iter to file 
fwrite(metrics, file = paste0("./data/", date_path, "/iter_", i, ".csv"))
    
return(metrics)
    
}


    
    
    
    
    