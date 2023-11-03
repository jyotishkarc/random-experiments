
library(simulatr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)


#####################
results_rds <- readRDS("~/R/R Codes/Experiments with R/temp-spacrt-result/power_grid_explore_FFD_bin_NB_normal_B_5000_5e3_n5_n5_disp_10_results.rds")
#####################

#####################
(n_methods <- results_rds$results %>% dplyr::pull(method) %>% unique() %>% length())
(n_levels <- results_rds$metrics %>% dplyr::pull(metric) %>% unique() %>% length() - 3)
#####################

#####################
# 1. Parameter grid
#####################
varying_params <- list(# n = c(1e3,5e3,1e4,5e4,1e5),
   n = 5000,
   gamma_0 = seq.int(from = -6, to = -2),
   beta_0 = seq.int(from = -6, to = -2))

baseline_params <- list(n = 5e3, gamma_0 = -5, beta_0 = -5)

grid_ffd_orig <- simulatr::create_param_grid_fractional_factorial(varying_params,
                                                                  baseline_params)

# get_ground_truth <- function(n){
#    return(NULL)
# }

rho <- 1:5 / 5
# rho <- 5 / 5

parameter_grid <- grid_ffd %>% dplyr::select(1:3) %>%
   slice(rep(1:n(), each = length(rho))) %>%
   cbind(rho = rep(rho, times = nrow(grid_ffd))) %>%
   cbind(grid_id = 1:(length(rho)*nrow(grid_ffd)))

# 
# (grid_ffd <- grid_ffd_orig %>%
#       select(-c(arm_n,arm_gamma_0,arm_beta_0)) %>%
#       # select(-c(grid_id)) %>%
#       `rownames<-`( NULL ) %>%
#       add_row(n = 5e3, gamma_0 = -5, beta_0 = -5, .before = 7) %>%
#       add_row(n = 5e3, gamma_0 = -5, beta_0 = -5, .before = 12))

param_names <- names(baseline_params) %>% setdiff('n') %>% c('rho')

###################
# results_rds_metrics <- results_rds$metrics[1:(n_methods * n_levels * nrow(grid_ffd_orig)), ]
results_rds_results <- results_rds$results
###################

methods <- results_rds_results %>% dplyr::pull(method) %>% unique() %>% as.character()
level <- c(5e-2, 1e-2, 5e-3)

power.temp <- list()

for(i in sort(unique(results_rds_results$grid_id))){
   print(i)
   
   df.pval.temp <- results_rds_results %>%
      dplyr::filter(grid_id == i) %>%
      dplyr::select(-grid_id)
   
   power.temp[[i]] <- matrix(NA, nrow = n_methods, ncol = length(level))
   
   for(j in 1:n_methods){
      
      pvals.temp <- df.pval.temp %>% 
                        dplyr::filter(method == methods[j]) %>% 
                        dplyr::pull(output) %>% 
                        lapply(function(lst) lst$p.right) %>%
                        unlist()
      
      power.temp[[i]][j,1] <- mean(pvals.temp >= level[1])
      power.temp[[i]][j,2] <- mean(pvals.temp >= level[2])
      power.temp[[i]][j,3] <- mean(pvals.temp >= level[3])
      
      rownames(power.temp[[i]]) <- methods
      colnames(power.temp[[i]]) <- paste0('level = ',level)
   }
}

cat("Results extracted successfully!\n")

##########################################
##########################################
##########################################
##########################################
##########################################

page.heading <- c()

param_names <- names(baseline_params) %>% setdiff('n')

plt.power <- plt.power.final <- list()

for(param in param_names){
   
   plt.power.final[[which(param_names == param)]] <- list()
   
   fixed <- param_names %>% setdiff(param)
   
   param_grid_fixed <- parameter_grid %>% 
                           filter(!!as.symbol(fixed) == baseline_params[[fixed]]) %>% 
                           dplyr::arrange(param)
   
   plt.power[[which(param_names == param)]] <- list()
   
   for(k in param_grid_fixed %>% pull(param) %>% unique() %>% sort()){
      
      plt.power[[which(param_names == param)]][[which(param_grid_fixed %>% pull(param) %>% unique() %>% sort() == k)]] <- list()
      
      for(l in 1:length(level)){
         df <- param_grid_fixed$grid_id[which(param_grid_fixed[[param]] == k)] %>% 
            lapply(function(val) power.temp[[val]]) %>% 
            sapply(function(mat) mat[,l])
         
         df.power <-
            cbind(method = rownames(df), 
                  df %>% as.data.frame()) %>% 
               `rownames<-`(NULL) %>% `colnames<-`(c('method',rho)) %>% 
               reshape2::melt(id = 'method')
         
         df.power$variable <- as.numeric(df.power$variable)
         
         plt.power[[which(param_names == param)]][[which(param_grid_fixed %>% pull(param) %>% unique() %>% sort() == k)]][[l]] <- 
            df.power %>% 
            ggplot(aes(x = variable, y = value, color = method)) + 
               geom_point() + 
               scale_x_continuous(labels = as.character(rho)) +
               geom_line() + 
               xlab("Coeff. of X") +
               ylab("") +
               theme_light() +
               ggtitle(paste0(param," = ",k,", ", 
                              fixed," = ",baseline_params[[fixed]], 
                              ", Level = ", level[l])) +
               theme(legend.position = "bottom",
                     plot.title = element_text(hjust = 0.5, face = 'bold', size = 14))
      }
   }
   
   for (l in 1:length(level)) {
      plt.power.final[[which(param_names == param)]][[l]] <- 
         gridExtra::grid.arrange(arrangeGrob(
            plt.power[[which(param_names == param)]][[1]][[l]],
            plt.power[[which(param_names == param)]][[2]][[l]],
            plt.power[[which(param_names == param)]][[3]][[l]],
            plt.power[[which(param_names == param)]][[4]][[l]],
            plt.power[[which(param_names == param)]][[5]][[l]],
            left = textGrob("Power", 
                            rot = 90, vjust = 1),
            nrow = 1),
            nrow = 1)
   }
   
   plt.power.final[[which(param_names == param)]] <-
      gridExtra::grid.arrange(
         plt.power.final[[which(param_names == param)]][[1]],
         plt.power.final[[which(param_names == param)]][[2]],
         plt.power.final[[which(param_names == param)]][[3]],
         nrow = 3)
         
   page.heading[which(param_names == param)] <- 
      paste0("X|Z ~ Bernoulli, ",
             "Y|Z ~ NB : Fixed n = 5000, Fixed ",fixed," = ",baseline_params[[fixed]],
             ", Variable ",param, "\n")
}

rm(plt.power)
cat("Plots generated successfully!\n")

#################

ggsave(filename = "plot-power-FFD-bin-NB-normal-5000-5e3-n5-n5-disp-10.pdf", 
       plot = marrangeGrob(plt.power.final, nrow=1, ncol=1,
                           top = quote(grid::textGrob(page.heading[g], 
                                                      gp=grid::gpar(fontsize=22,
                                                                    fontface=2)))),
       
       width = 40, height = 20)

#################

beepr::beep()




