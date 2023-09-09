

func_04_missingness <- function(traintest, fits_and_imps, active_triage_only, 
                                 exclude, exclude_vars, manipulation_check, 
                                 manipulation_type) {


# 1. Data prep --------------------------------------------------------------------

test <- traintest$test
imp_vars <- fits_and_imps$topimp
acttriage <- active_triage_only




# 2. Create OBSERVED test data (e.g., active ONLY at triage) ----------------------

# Select only lassoed problem list vars in the 'active at triage only' dataset
lpattern <- c("visitid", "^[A-Z][A-Z0-9][A-Z0-9]$")
lvars <- grep(paste(lpattern, collapse="|"), 
                names(test), value = TRUE) # Get vars from lassoed test set
acttriage <- active_triage_only[, ..lvars] # Subset active at triage set 


### Replace original test problem list columns with active at triage only  

# Drop problem list columns from test data 
prob_list_vars <- grep("^[A-Z][A-Z0-9][A-Z0-9]$", colnames(test), value = TRUE)
obs_data <- select(test, -matches(prob_list_vars))

# Add only active at triage 
obs_data <- left_join(x = obs_data, y = acttriage, by = "visitid")





# 3. Create MANIPULATED test data (e.g., important vars ALL active at end of encounter) -------

### Manipulated data prep 
# TWO OPTIONS. manipulation_type = "most_imp", only for 10 most important problem
# list predictors. If manipulation_type = "all", use all problem list data 
# from end of encounter. 

if (manipulation_type == "most_imp") {
    
    # Replace 10 most important vars in observed data with original test data values 
    
    print(paste0("Manipulation type: ", manipulation_type))
    
    # Get imp vars 
    most_imp_vars <- unique(imp_vars$var)
    
    # Replace 
    manip_data <- select(obs_data, -matches(most_imp_vars))
    new_manip_cols <- select(test, "visitid", matches(most_imp_vars))
    manip_data <- full_join(x = manip_data, y = new_manip_cols, by = "visitid")
    
}

if (manipulation_type == "all") {
    
    # Replace all problem list vars with all active from end of encounter 
    
    print(paste0("Manipulation type: ", manipulation_type))
    
    # Replace 
    manip_data <- test
    
}

### Option: exclude vars from manipulation (e.g., sepsis, AKI) - for sensitivity analysis 
if (exclude == TRUE) {
    
    print(paste0("Excluding problem list vars ICD-10 ", paste(exclude_vars, collapse = ", "), " from manipulation"))
    
    # Drop these vars from the manipulated data set 
    my_exclude_vars <- exclude_vars
    manip_data <- select(manip_data, -matches(my_exclude_vars))
    
    # Add the 'active only at triage' back in
    manip_data <- left_join(x = manip_data, y = acttriage[, c('visitid', ..my_exclude_vars)], by = "visitid")
    
}



# 4. Checks -------------------------------------------------------------------------

### Calculate proportion of patients with diagnoses in observed and manipulated data 

# Add some 'non-important' vars to 'var' object in order to check manipulation type 
mycheckvars <- c(imp_vars$var, "C24", "G37", "C90", "I60", "A41", "N17")

# Append observed and manipulated data 
obs_check <- obs_data
obs_check$data <- "obs"
manip_check <- manip_data
manip_check$data = "manip"
check_data <- rbind(obs_check, manip_check)

# Calculate percentages by race and data set
check_data <- check_data %>%
    filter((race == "White or Caucasian" & ethnicity == "Not Hispanic or Latino") | 
            race == "Black or African American") %>%
    group_by(data, race) %>%
    summarise_at(vars(mycheckvars), mean) %>%
    mutate(race = recode(race, "White or Caucasian" = "Non-Hispanic White"))
check_data

# Reshape/format for interpretability
check_data$race <- factor(check_data$race, levels = c("Black or African American",
                                                      "Non-Hispanic White"),
                          labels = c("black", "nHwhite"))
check_data$data_race <- paste0(check_data$data, "_", check_data$race)
check_data$data_race <- factor(check_data$data_race, levels = c("obs_nHwhite", "manip_nHwhite", 
                                                                "obs_black", "manip_black"))
check_data$data <- NULL
check_data$race <- NULL
check_data <- check_data %>% arrange(data_race)
check_data <- check_data %>% 
    pivot_longer(cols = -c(data_race), names_to = "var", values_to = "percent") %>%
    pivot_wider(id_cols = var, names_from = data_race, values_from = percent)
print("Checking manipulation...")
print(check_data)


if (manipulation_check == TRUE) {
    
fwrite(check_data, file = paste0("./data/manipulation_check_",
                                    format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"))

}


# 5. Export -------------------------------------------------------------------------

results = list(observed = obs_data, manipulated = manip_data)
return(results)



}
