
library(simulatr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

#####################
results_rds <- readRDS("~/R/R Codes/Experiments with R/temp-spacrt-result/grid_explore_FFD_bin_NB_normal_B_20000_5e3_n5_n5_disp_1_only_scoretest_results.rds")
#####################

#####################
(n_methods <- results_rds$results %>% dplyr::pull(method) %>% unique() %>% length())
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

param_names <- names(baseline_params)

###################
results_rds_metrics <- results_rds$metrics[1:(n_methods * n_levels * nrow(grid_ffd_orig)), ]
results_rds_results <- results_rds$results
###################

plt <- list()
page.heading <- c()

for(toe in unique(results_rds_metrics$metric)){
   
   if(toe == 'toe.new.1') level <- 5e-2
   if(toe == 'toe.new.2') level <- 1e-2
   if(toe == 'toe.new.3') level <- 5e-3
   
   res.toe <- results_rds_metrics %>% dplyr::filter(metric == toe)
   unique.metrics <- unique(results_rds_metrics$metric)
   
   plt[[which(unique.metrics == toe)]] <- list()
   
   for(param in param_names){
      
      fixed <- param_names %>% setdiff(param)
      fixed_pos_1 <- which(fixed[1] == param_names)
      fixed_pos_2 <- which(fixed[2] == param_names)
      
      df <- res.toe %>% dplyr::filter(!!as.symbol(param_names[fixed_pos_1]) == 
                                         baseline_params[[param_names[fixed_pos_1]]]) %>%
         dplyr::filter(!!as.symbol(param_names[fixed_pos_2]) == 
                          baseline_params[[param_names[fixed_pos_2]]]) %>% 
         dplyr::select(method, mean, se, !!as.symbol(param))
      
      plt.df <- df %>% 
         ggplot(aes(x = !!as.symbol(param),
                    y = mean,
                    ymin = mean - 2*se,
                    ymax = mean + 2*se,
                    color = method)) +
         geom_point() +
         geom_line() +
         geom_errorbar(width = 0.2) +
         geom_hline(yintercept = level, linetype = "dashed", color = "red") +
         ylab("") +
         scale_y_log10() +
         {if(param == 'n') scale_x_log10()} +
         theme_light() +
         theme(legend.position = "bottom")
      
      plt[[which(unique.metrics == toe)]][[which(param_names == param)]] <- plt.df
      
      print(which(param_names == param))
   }
   
   plt[[which(unique.metrics == toe)]] <- 
      gridExtra::grid.arrange(arrangeGrob(
         plt[[which(unique.metrics == toe)]][[which(param_names == param_names[1])]],
         plt[[which(unique.metrics == toe)]][[which(param_names == param_names[2])]],
         plt[[which(unique.metrics == toe)]][[which(param_names == param_names[3])]],
         left = textGrob("Type-I error (in log_10 scale)", 
                         rot = 90, vjust = 1),
         nrow = 1),
         nrow = 1)
   
   print('M')
   
   page.heading[which(unique.metrics == toe)] <- 
      paste0("X|Z ~ Bernoulli, ",
             "Y|Z ~ NB : Level = ",level,"\n")
}

#################

ggsave(filename = "plot-FFD-bin-NB-normal-20000-5e3-n5-n5-disp-1-only-scoretest-ToE.pdf", 
       plot = marrangeGrob(plt, nrow=1, ncol=1,
                           top = quote(grid::textGrob(page.heading[g], 
                                                      gp=grid::gpar(fontsize=20,
                                                                    fontface=2)))),
       
       width = 12, height = 7)



