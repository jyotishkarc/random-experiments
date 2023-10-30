
library(simulatr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(katlabutils)

#####################
results_rds <- readRDS("~/R/R Codes/Experiments with R/temp-spacrt-result/grid_explore_FFD_bin_NB_normal_B_5000_5e3_n5_n5_disp_1_LEFT_results.rds")

results_rds_results <- results_rds$results
#####################

#####################
(n_methods <- results_rds_results %>% dplyr::pull(method) %>% unique() %>% length())
(n_levels <- results_rds$metrics %>% dplyr::pull(metric) %>% unique() %>% length() - 3)
#####################

#####################
# 1. Parameter grid
#####################
varying_params <- list(n = c(1e3,5e3,1e4,5e4,1e5),
                       gamma_0 = seq.int(from = -6, to = -2),
                       beta_0 = seq.int(from = -6, to = -2))

baseline_params <- list(n = 5e3, gamma_0 = -5, beta_0 = -5)

(grid_ffd_orig <- create_param_grid_fractional_factorial(varying_params,baseline_params))

(grid_ffd <- grid_ffd_orig %>%
      select(-c(arm_n,arm_gamma_0,arm_beta_0)) %>%
      # select(-c(grid_id)) %>%
      `rownames<-`( NULL ) %>%
      add_row(n = 5e3, gamma_0 = -5, beta_0 = -5, .before = 7) %>%
      add_row(n = 5e3, gamma_0 = -5, beta_0 = -5, .before = 12))



df.pvals <- df.qq <- df.qq.proper <- qq_plots_NB <- list()

for(i in sort(unique(results_rds_results$grid_id))){
   
   df.qq[[i]] <- results_rds_results %>%
      # dplyr::select(run_id) %>%
      dplyr::filter(grid_id == i) %>%
      dplyr::select(-grid_id) #%>%
   # dplyr::filter(output != 0)
   
   df.qq.proper[[i]] <- data.frame(method = df.qq[[i]]$method %>% as.character(), 
                                   run_id = df.qq[[i]]$run_id %>% as.integer(),
                                   p.value = df.qq[[i]]$output %>% unlist()) %>% arrange(run_id)
   
   # reshape2::melt(id =  value.name = 'p-value') %>%
   
   # chosen <- sample.int(500000, 20000)
   
   (qq_plots_NB[[i]] <- df.qq.proper[[i]] %>%
         # filter(method == 'scoretest_est') %>%
         # filter(run_id %in% chosen) %>%
         # qq_plots[[i]] <- df.slice[[i]] %>%
         ggplot(aes(y = p.value, color = method)) +
         # geom_point() +
         stat_qq_points() +
         stat_qq_band() +
         geom_abline() +
         # scale_x_log10() +
         # scale_x_reverse(trans = revlog_trans()) +
         scale_x_continuous(trans = revlog_trans(),
                            breaks = c(1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)) +
         scale_y_continuous(trans = revlog_trans(),
                            breaks = c(1, 1e-2, 1e-4, 1e-6, 1e-8, 1e-10),
                            limits = c(1,1e-10)) +
         theme_light() +
         # ggtitle(paste0("n = ",grid_ffd$n[i],", ",
         #                "gamma_0 = ",grid_ffd$gamma_0[i],", ",
         #                "beta_0 = ",grid_ffd$beta_0[i])) +
         theme(legend.position = "bottom", 
               legend.title = element_blank(),
               plot.title = element_text(hjust = 0.5, face = 'bold')) +
         labs(x = "Expected null p-value",
              y = "Observed p-value"))
   
   print(i)
}

plt.qq <- list()

for(k in 1:15){
   if(k >= 1 & k <= 6) {plt.qq[[k]] <- qq_plots_NB[[k]] + 
      ggtitle(paste0("n = ",grid_ffd$n[k],", ",
                     "gamma_0 = ",grid_ffd$gamma_0[k],", ",
                     "beta_0 = ",grid_ffd$beta_0[k]))}
   if(k >= 7 & k <= 10) {plt.qq[[k+1]] <- qq_plots_NB[[k]] +
      ggtitle(paste0("n = ",grid_ffd$n[k+1],", ",
                     "gamma_0 = ",grid_ffd$gamma_0[k+1],", ",
                     "beta_0 = ",grid_ffd$beta_0[k+1]))}
   if(k >= 11 & k <= 13) {plt.qq[[k+2]] <- qq_plots_NB[[k]] +
      ggtitle(paste0("n = ",grid_ffd$n[k+2],", ",
                     "gamma_0 = ",grid_ffd$gamma_0[k+2],", ",
                     "beta_0 = ",grid_ffd$beta_0[k+2]))}
   
   if(k == 7 | k == 12) {plt.qq[[k]] <- qq_plots_NB[[2]] +
      ggtitle(paste0("n = ",grid_ffd$n[2],", ",
                     "gamma_0 = ",grid_ffd$gamma_0[2],", ",
                     "beta_0 = ",grid_ffd$beta_0[2]))}
}

#################

ggsave(filename = "plot-FFD-bin-NB-normal-5000-5e3-n5-n5-disp-1-LEFT-QQ.pdf", 
       plot = marrangeGrob(plt.qq, nrow = 1, ncol = length(varying_params$n),
                           top = quote("")),
       width = 20, height = 7)


