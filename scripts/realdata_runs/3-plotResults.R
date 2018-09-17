library(ggplot2)
library(dplyr)

### CSMF ccuracy boxplots
results <- readRDS("../../data/country_results.rds")

### relevel results
results$method <- factor(results$method,
                         levels = c('tariff_calib', 'tariff_train_and_calib',
                                    'tariff_train', 'tariff_mle', 'tariff_revamp',
                                    'insilico_calib', 'insilico_train_and_calib',
                                    'insilico_train', 'insilico_mle', 'insilico_revamp'))
results$is.revamp <- grepl("revamp", results$method)

ncod.vec <- c(4, 8)
for(i in 1:length(ncod.vec)){
    ncod <- ncod.vec[i]
    title <- paste("Top", ncod - 1, "COD + others")
    csmf.plot <-
        results %>%
        filter(ncod == ncod.vec[i]) %>%
        ggplot(aes(x = method, y = csmf.acc, color = is.revamp)) +
        facet_wrap(country ~calib.size , ncol = length(unique(results$calib.size))) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "bottom") +
        ggtitle(title)
    viz.dir <- "../../visualizations"
    if(!dir.exists(viz.dir)){
        dir.create(viz.dir)
    }
    ggsave(filename = file.path(viz.dir,
                                paste0("top", ncod - 1, "_with_others_results.jpg")),
           plot = csmf.plot, width = 14, height = 12)
}

### CSMFA plots as a function of CSMFA between calib & test 
### Only plot InSilico + Calib, InSilico + Train, Insilico + ReVAMP

ncod.vec <- c(4, 8)
for(i in 1:length(ncod.vec)){
    ncod <- ncod.vec[i]
    title <- paste("Top", ncod - 1, "COD + others")
    csmf.csmfa.calib.plot <-
        results %>%
        filter(ncod == ncod.vec[i], method %in% c("insilico_calib", "insilico_train", "insilico_revamp")) %>%
        ggplot(aes(x = csmfa.calib, y = csmf.acc, color = method)) +
        facet_grid(country ~calib.size) +
        geom_point(alpha = .2) +
        geom_smooth() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "bottom") +
        xlab("CSMFA between calib & test set") +
        ylab("CSMFA of each method (Smoothed)") +
        ggtitle(title)
    viz.dir <- "../../visualizations"
    if(!dir.exists(viz.dir)){
        dir.create(viz.dir)
    }
    ggsave(filename = file.path(viz.dir,
                                paste0("top", ncod - 1, "_by_csmfa_calib_test.pdf")),
           plot = csmf.csmfa.calib.plot, width = 14, height = 12)
}

### CSMFA plots as a function of CSMFA between train & test 
### Only plot InSilico + Calib, InSilico + Train, Insilico + ReVAMP

ncod.vec <- c(4, 8)
for(i in 1:length(ncod.vec)){
    ncod <- ncod.vec[i]
    title <- paste("Top", ncod - 1, "COD + others")
    csmf.csmfa.train.plot <-
        results %>%
        filter(ncod == ncod.vec[i], method %in% c("insilico_calib", "insilico_train", "insilico_revamp")) %>%
        ggplot(aes(x = csmfa.train, y = csmf.acc, color = method)) +
        facet_grid(country ~calib.size) +
        geom_point(alpha = .2) +
        geom_smooth() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "bottom") +
        xlab("CSMFA between train & test set") +
        ylab("CSMFA of each method (Smoothed)") +
        ggtitle(title)
    viz.dir <- "../../visualizations"
    if(!dir.exists(viz.dir)){
        dir.create(viz.dir)
    }
    ggsave(filename = file.path(viz.dir,
                                paste0("top", ncod - 1, "_by_csmfa_train_test.pdf")),
           plot = csmf.csmfa.train.plot, width = 14, height = 12)
}

### CSMFA plots as a function of pother in test
### Only plot InSilico + Calib, InSilico + Train, Insilico + ReVAMP

ncod.vec <- c(4, 8)
for(i in 1:length(ncod.vec)){
    ncod <- ncod.vec[i]
    title <- paste("Top", ncod - 1, "COD + others")
    csmf.pother.plot <-
        results %>%
        filter(ncod == ncod.vec[i], method %in% c("insilico_calib", "insilico_train", "insilico_revamp")) %>%
        ggplot(aes(x = pother, y = csmf.acc, color = method)) +
        facet_grid(country ~calib.size) +
        geom_point(alpha = .2) +
        geom_smooth() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "bottom") +
        xlab("CSMF of Other COD in test") +
        ylab("CSMFA of each method (Smoothed)") +
        ggtitle(title)
    viz.dir <- "../../visualizations"
    if(!dir.exists(viz.dir)){
        dir.create(viz.dir)
    }
    ggsave(filename = file.path(viz.dir,
                                paste0("top", ncod - 1, "_by_pother.pdf")),
           plot = csmf.pother.plot, width = 14, height = 12)
}


### CSMF accuracy without other COD
# csmf.acc.topcod.results <- readRDS("../../data/country_csmf_topcod_results.rds")
# 
# ### relevel results
# csmf.acc.topcod.results$method <- factor(csmf.acc.topcod.results$method,
#                                          levels = c('tariff_calib', 'tariff_train_and_calib',
#                                                     'tariff_train', 'tariff_mle', 'tariff_revamp',
#                                                     'insilico_calib', 'insilico_train_and_calib',
#                                                     'insilico_train', 'insilico_mle', 'insilico_revamp'))
# csmf.acc.topcod.results$is.revamp <- grepl("revamp", csmf.acc.topcod.results$method)
# 
# ncod.vec <- c(4, 8)
# for(i in 1:length(ncod.vec)){
#     ncod <- ncod.vec[i]
#     title <- paste("Top", ncod - 1, "COD w/o others")
#     csmf.plot <-
#         csmf.acc.topcod.results %>%
#         filter(ncod == ncod.vec[i]) %>%
#         ggplot(aes(x = method, y = csmf.acc, color = is.revamp)) +
#         facet_wrap(country ~calib.size , ncol = length(unique(results$calib.size))) +
#         geom_boxplot() +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1),
#               legend.position = "bottom") +
#         ggtitle(title)
#     viz.dir <- "../../visualizations"
#     if(!dir.exists(viz.dir)){
#         dir.create(viz.dir)
#     }
#     ggsave(filename = file.path(viz.dir,
#                                 paste0("top", ncod - 1, "_without_others_results.jpg")),
#            plot = csmf.plot, width = 14, height = 12)
# }


