
func_03_fits <- function(traintest, fit_summaries) {


# 1. Get data ----------------------------------------------------------------------

train <- traintest$train

# vars for models
tomatch <- c("_bin$", "^[A-Z][A-Z0-9][A-Z0-9]$")
matches <- grep(paste(tomatch, collapse="|"), 
                        names(train), value = TRUE)
indeps <- c("arrival_method", "sex", "age", matches, "newcomplaint")
f <- paste(indeps, collapse = " + ")







# 2. Random forests -----------------------------------------------------------------
    
print("Fitting random forests")

# Hospitalization
tic("hospitalization")
set.seed(5252)
m_hosp <- ranger(formula = as.formula(paste0("disp_adm ~ ", f)),
                       data = train, importance = "permutation")
toc()


# Emergency procedure
tic("Emergency procedure")
set.seed(5252)
m_emproc <- ranger(formula = as.formula(paste0("em_proc ~ ", f)),
                   data = train, importance = "permutation")
toc()


# Critical outcome
tic("critical outcome")
set.seed(5252)
m_crit <- ranger(formula = as.formula(paste0("crit_out ~ ", f)),
                       data = train, importance = "permutation")





# 3. Diagnostics --------------------------------------------------------------------



if (fit_summaries == TRUE) {
    
    print("Generating model fit summary .csv")

    ## Fit summaries
    models <- list(m_hosp, m_emproc, m_crit)
    model_names <- list("m_hosp", "m_emproc", "m_crit")
    obs_vars <- list("disp_adm", "em_proc", "crit_out")
    args <- list(models, model_names, obs_vars)
    results <- list()
    summaries <- rbindlist(lapply(1:length(args[[1]]), modelsum, args = args, data = train,
                                  results = results))
    fwrite(summaries, file = paste0("./data/model_summaries_",
                                    format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"))

}





}


