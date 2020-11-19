# plot area under the curve with CIs

df %>% 
  ggplot(aes(cohort, auc)) +
  
  geom_rect(aes(xmin = min, xmax = max, 
                ymin = -Inf, ymax = Inf,
                fill = factor(col), 
                alpha = 0.1)) +
  scale_fill_manual(values = c("white", "gray53")) +
  
  geom_point(aes(x = as.numeric(cohort), 
                 y = auc),
             shape = 15,
             size = 3) +
  
  geom_errorbar(aes(ymin=lwr.CI, 
                    ymax=upr.CI,
                    x = as.numeric(as.factor(cohort))), 
                width=.2,
                position=position_dodge(.9)) +
  
  scale_x_discrete(breaks = AUC$cohort,
                   labels = AUC$cohort) +
  scale_y_continuous(breaks = seq(.5,1, by = 0.1))+
  labs(title = "AUROCs",
       x = "",
       y = "AUROC [-]") +
  coord_flip(ylim = c(0.5,1)) +
  theme_Publication() +
  theme(legend.position = "none")