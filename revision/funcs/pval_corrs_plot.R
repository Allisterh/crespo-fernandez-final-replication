pval.corrs.plot = function(my_model){ 
  
  correls = data.residuals %>% 
    select(country,date,my_model) %>% 
    pivot_wider(names_from = country, values_from = my_model) %>% 
    select(-date) %>% 
    as.matrix() %>% 
    rcorr(type = "pearson") 
  
  correls = data.frame("pval" = correls$P[lower.tri(correls$P)], 
                       "index" = 1:length(correls$P[lower.tri(correls$P)]))
  
  correls %>% 
    ggplot(aes(x = index, y = pval)) + 
    geom_point() + 
    geom_hline(yintercept=0.05, linetype="dashed", color = "red") + 
    xlab("Correlation index") + 
    ylab("P-value of residuals correlation") + 
    ggtitle(paste0("Acceptance rate:  ", 100 * round(as.numeric(table(correls$pval > 0.05)["TRUE"] / (table(correls$pval > 0.05)["TRUE"] + table(correls$pval > 0.05)["FALSE"])),4), "%")) +
    theme_bw()
  
}