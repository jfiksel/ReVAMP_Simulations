library(ggplot2)
library(dplyr)
results <- readRDS("../../data/simulation_results.rds")
results$csmf.acc.split <- round(results$csmf.acc.split, 2)

### relevel results
results$method <- factor(results$method,
                         levels = c('tariff_calib', 'tariff_train_and_calib',
                                    'tariff_train', 'tariff_mle', 'tariff_revamp',
                                    'insilico_calib', 'insilico_train_and_calib',
                                    'insilico_train', 'insilico_mle', 'insilico_revamp'))
results$is.revamp <- grepl("revamp", results$method)

csmf.plot <-
    results %>%
    filter(measure == "csmf") %>%
    ggplot(aes(x = method, y = accuracy, color = is.revamp)) +
    facet_grid(calib.size ~ csmf.acc.split) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ccc.plot <-
    results %>%
    filter(measure == "ccc") %>%
    ggplot(aes(x = method, y = accuracy, color = is.revamp)) +
    facet_grid(calib.size ~ csmf.acc.split) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
viz.dir <- "../../visualizations"
if(!dir.exists(viz.dir)){
    dir.create(viz.dir)
}
ggsave(filename = file.path(viz.dir, "csmf_results.jpg"),
       plot = csmf.plot, width = 14, height = 12)
ggsave(filename = file.path(viz.dir, "ccc_results.jpg"),
       plot = ccc.plot, width = 14, height = 12)