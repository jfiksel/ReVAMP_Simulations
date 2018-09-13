library(dplyr)
library(openVA)
setting.df <- expand.grid(run = 1:500,
                          country = c("India", "Tanzania"),
                          calib.size = c(50, 100, 200, 400),
                          ncod = c(4, 8))
results.list <- lapply(1:nrow(setting.df), function(i){
    setting <- setting.df[i,]
    ncod <- setting$ncod
    sim.dir <- file.path("..", "..", "data", "country_runs",
                         paste0(setting$country, "_size_", setting$calib.size, "_ncod_", ncod),
                         paste0("run_", setting$run))
    results.file <- file.path(sim.dir, "results.rds")
    if(!file.exists(results.file)){
        return(NULL)
    } else {
        results <- readRDS(results.file)
        ### Get CSMFA between calib & test and train & test
        topcause <- results$topcause.df
        csmf.acc <- results$csmf.acc
        csmf.acc <- cbind(csmf.acc, setting)
        csmf.acc$csmfa.calib <- getCSMF_accuracy(topcause$ptest_orig, topcause$ptest)
        csmf.acc$csmfa.train <- getCSMF_accuracy(topcause$ptrain, topcause$ptest)
        return(csmf.acc)
    }
})
results.df <- do.call(rbind, results.list)
saveRDS(results.df, "../../data/country_results.rds")

### CSMF accuracy without other COD
csmf.acc.topcod.list <- lapply(1:nrow(setting.df), function(i){
    setting <- setting.df[i,]
    ncod <- setting$ncod
    sim.dir <- file.path("..", "..", "data", "country_runs",
                         paste0(setting$country, "_size_", setting$calib.size, "_ncod_", ncod),
                         paste0("run_", setting$run))
    results.file <- file.path(sim.dir, "results.rds")
    if(!file.exists(results.file)){
        return(NULL)
    } else {
        results <- readRDS(results.file)
        csmf.df <- results$csmf
        csmf.acc.df <-
            csmf.df %>%
            filter(cause.text != "Other") %>%
            group_by(method) %>%
            summarise(csmf.acc = getCSMF_accuracy(csmf.est, true.csmf))
        csmf.acc.df <- cbind(csmf.acc.df, setting)
        return(csmf.acc.df)
    }
})
csmf.acc.topcod.df <- do.call(rbind, csmf.acc.topcod.list)
saveRDS(csmf.acc.topcod.df, "../../data/country_csmf_topcod_results.rds")
