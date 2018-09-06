library(dplyr)
setting.df <- expand.grid(run = 1:100, split = 1:5, calib.size = c(100, 200, 400, 600))
results.list <- lapply(1:nrow(setting.df), function(i){
    setting <- setting.df[i,]
    sim.dir <- file.path("..", "data", "simulation_runs",
                         paste0("split_", setting$split, "_size_", setting$calib.size),
                         paste0("run_", setting$run))
    results.file <- file.path(sim.dir, "results.rds")
    results <- readRDS(results.file)
    csmf.acc.df <-
        results$csmf.acc  %>%
        rename(accuracy = csmf.acc)
    avg.ccc.df <-
        results$avg.ccc %>%
        rename(accuracy = ccc.mean)
    combined.results <- rbind(csmf.acc.df, avg.ccc.df)
    combined.results$measure <- rep(c('csmf', 'ccc'), c(nrow(csmf.acc.df), nrow(avg.ccc.df)))
    combined.results <- cbind(combined.results, setting)
    combined.results$csmf.acc.split <- rep(results$csmf.acc.split, nrow(combined.results))
    return(combined.results)
})
results.df <- do.call(rbind, results.list)
saveRDS(results.df, "../data/simulation_results.rds")
