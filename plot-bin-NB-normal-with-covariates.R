

library(dplyr)
library(ggplot2)
library(gridExtra)
library(katlabutils)

parameter_grid_NB <- expand.grid(
   # n = c(50,seq.int(from = 100, to = 500, by = 100))    # sample size
   n = seq.int(from = 50, to = 600, by = 50)    # sample size
   # p = c(3, 5, 8, 12, 15)    # dimension
   # n = c(20, 30, 40),
   # p = c(3, 8)    # dimension
)

bin_NB_normal_B_1000000_results <- readRDS("~/R/R Codes/Experiments with R/temp-spacrt-result/bin_NB_normal_B_1000000_results.rds")

for(k in 2:5){
   plot.temp <- bin_NB_normal_B_1000000_results$metrics[1:144, ] |>
      dplyr::filter(n != 50) |>
      dplyr::filter(metric == paste0('toe',k)) |>
      ggplot(aes(x = n,
                 y = mean,
                 ymin = mean - 2*se,
                 ymax = mean + 2*se,
                 color = method)) +
      geom_point() +
      geom_line() +
      geom_errorbar(width = 1) +
      geom_hline(yintercept = 10^(-k), linetype="dashed", color = "red") +
      labs(x = "Sample size",
           y = "Type-I error") +
      theme_light() +
      theme(legend.position = "bottom")
   
   assign(paste0('plot.NB.toe.',k), plot.temp)
   print(k)
}

ggpubr::ggarrange(plot.NB.toe.2,
                  plot.NB.toe.3,
                  plot.NB.toe.4,
                  plot.NB.toe.5,
                  common.legend = T,
                  legend = 'bottom')

ggsave('plot-bin-NB-normal-with-covariates-toe.pdf')


df.pvals <- df.qq <- df.qq.proper <- qq_plots_NB <- list()

for(i in sort(unique(bin_NB_normal_B_1000000_results$results$grid_id))){
   
   df.qq[[i]] <- bin_NB_normal_B_1000000_results$results %>%
      # dplyr::select(run_id) %>%
      dplyr::filter(grid_id == i) %>%
      dplyr::select(-grid_id) #%>%
   # dplyr::filter(output != 0)
   
   df.qq.proper[[i]] <- data.frame(method = df.qq[[i]]$method %>% as.character(), 
                                   run_id = df.qq[[i]]$run_id %>% as.integer(),
                                   p.value = df.qq[[i]]$output %>% unlist()) %>% arrange(run_id)
   
   # reshape2::melt(id =  value.name = 'p-value') %>%
   
   chosen <- sample.int(1000000, 20000)
   
   (qq_plots_NB[[i]] <- df.qq.proper[[i]] %>%
         filter(run_id %in% chosen) %>%
         # qq_plots[[i]] <- df.slice[[i]] %>%
         ggplot(aes(y = p.value, color = method)) +
         # geom_point() +
         stat_qq_points(max_pts_to_plot = 20000) +
         stat_qq_band() +
         geom_abline() +
         # scale_x_log10() +
         # scale_x_reverse(trans = revlog_trans()) +
         scale_x_continuous(trans = revlog_trans(),
                            breaks = c(1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)) +
         scale_y_continuous(trans = revlog_trans(),
                            breaks = c(1, 1e-2, 1e-4, 1e-6, 1e-8, 1e-10)) +
         # scale_y_reverse() +
         theme_light() +
         ggtitle(paste0("n = ",parameter_grid_NB$n[i])) +
         theme(legend.position = "bottom", 
               legend.title = element_blank(),
               plot.title = element_text(hjust = 0.5, face = 'bold')) +
         labs(x = "Expected null p-value",
              y = "Observed p-value"))
   
   print(i)
}


ggsave(filename = "plot-bin-pois-normal-covariate-qqplots.pdf", 
       plot = marrangeGrob(qq_plots_pois, nrow=1, ncol=2),
       width = 12, height = 7)




