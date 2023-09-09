

func_05_predict <- function(obs_manip, fits_and_imps, pred_summaries, complaint) {


# 1. Get data and models  ------------------------------------------------------------

# data
obs <- obs_manip$observed
manip <- obs_manip$manipulated

    # Subset to particular complaint if relevant 
    if (!is.null(complaint)) {
        
        message(paste0("Generating predictions for chief complaint subset: ", complaint))
        
        obs <- obs[obs$newcomplaint == complaint,]
        manip <- manip[manip$newcomplaint == complaint,]
        
        message("Cross-tabs for observed data")
        print(table(obs$newcomplaint, obs$race))
        message("Cross-tabs for manipulated data")
        print(table(manip$newcomplaint, manip$race))
        
    }


# models
m_hosp <- fits_and_imps$m_hosp
m_emproc <- fits_and_imps$m_emproc
m_crit <- fits_and_imps$m_crit



# 2. Predict -----------------------------------------------------------------------

print("Predicting for observed and manipulated test data")

# Note: predicting for 'ranger' class objects 
# https://www.rdocumentation.org/packages/ranger/versions/0.14.1/topics/predict.ranger

# Obs
obs$pred_hosp <- predict(m_hosp, data = obs)[['predictions']]
obs$pred_emproc <- predict(m_emproc, obs)[['predictions']]
obs$pred_crit <- predict(m_crit, obs)[['predictions']]


# Manipulated
manip$pred_hosp <- predict(m_hosp, manip)[['predictions']]
manip$pred_emproc <- predict(m_emproc, manip)[['predictions']]
manip$pred_crit <- predict(m_crit, manip)[['predictions']]




# 3. Checks -------------------------------------------------------------------------


if (pred_summaries == TRUE) {
    
### Plots 
    
# Append and reshape data 
obs$dataset <- "obs"
manip$dataset <- "manip"
myd <- rbind(obs, manip)
myd$dataset <- factor(myd$dataset, levels = c("obs", "manip"))

plot1 <- ggplot(myd, aes(x = pred_hosp)) + 
    geom_histogram(bins = 30) + 
    facet_wrap("dataset")
plot2 <- ggplot(myd, aes(x = pred_emproc)) + 
    geom_histogram(bins = 30) + 
    facet_wrap("dataset")
plot3 <- ggplot(myd, aes(x = pred_crit)) + 
    geom_histogram(bins = 30) + 
    facet_wrap("dataset")


mytitle <- ggdraw() + draw_label("predicted values summary", fontface = "bold")

# Save plots 
p1 <- plot_grid(plot1, plot2, plot3, nrow = 2)
p1 <- plot_grid(mytitle, p1, nrow = 2, rel_heights = c(0.1, 1))
p1
ggsave(p1, file = paste0("./figs/pred_summaries_",
                         format(Sys.time(), "%Y-%m-%d_%H.%M"), ".png"))


### Numerics 
myd <- myd %>%
    group_by(dataset) %>%
    summarise(across(c("pred_hosp", "pred_emproc", "pred_crit"), 
              list(.pred_min = min, .pred_mean = mean, .pred_median = median, .pred_max = max))) %>%
    pivot_longer(cols = -c(dataset), names_to = "var", values_to = "value") %>%
    separate(var, into = c("model", "quantity"), sep = "_\\.") %>%
    pivot_wider(id_cols = c("model", "dataset"), names_from = quantity) %>%
    arrange(desc(model), dataset)

fwrite(myd, file = paste0("./data/pred_summaries_",
       format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"))

}





# 3. Save --------------------------------------------------------------------------


results = list(obs_preds = obs, manip_preds = manip)
return(results)


}
