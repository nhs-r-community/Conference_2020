
#### Plot: Late obs - proportions
library(tidyverse)

# turn 4 cohort datasets into a list
late_prop <- list (
  PreCovid18,
  PreCovid19,
  PreCovid20,
  COVID) %>% 
  
  set_names("pre-COVID 2018",
            "pre-COVID 2019",
            "pre-COVID 2020",
            "COVID-era") %>% 
  
  # create EWS categories and classify late/missed obs
  map(
    ~{.x  %>%
        # group into EWS categories
        mutate(EWS_category = cut(NEWS,
                                  breaks = c(-1, 2.1, 5.1, Inf),
                                  labels=c("0-2","3-5","6+"))) %>% 
        
        # late, missed, on time obs
        # ttno = time to next observation
        mutate(IS_LATE = case_when(
          ttno  <= obs_interv/60 * (1 + 1/3) ~ "on time",
          ttno <= obs_interv/60 * (1 + 2/3) & ttno > obs_interv/60 * (1 + 1/3) ~ "late",
          ttno > obs_interv/60 * (1 + 2/3) ~ "missed")) %>% 
        mutate(IS_LATE = factor(IS_LATE, levels = c("on time", "late", "missed")))
    }) %>% 
  
  # add count and proportions
  map(
    ~{.x %>% 
        count(EWS_category, IS_LATE) %>% 
        group_by(EWS_category) %>% 
        mutate(perc_obs = round(n/sum(n),digits = 3)*100,
               pos = (cumsum(n) - 0.5 * n))
      }) %>% 
  
  # plot bar charts
  imap(
    ~{.x %>%
        ggplot(aes(fill = as.factor(IS_LATE),
                   y = perc_obs,
                   x = EWS_category)) +
        geom_bar(position = position_stack(),
                 stat="identity") +
        scale_y_continuous(expand = c(0, 0)) +
        geom_text(aes(label=paste0(perc_obs,"%")),
                  position = position_stack(vjust = 0.5),
                  size = 4) +
        scale_fill_manual(values = c("green4","yellow","red2")) +
        
        # scale_colour_manual(values = 'black') +
        labs(title = .y,
             x = "EWS",
             y = "proportion of obs [%]",
             fill = "observations")

            })

# --- save plots ---
jpeg(file = paste(outpath,"COVID_lateobs_prop.jpg",sep = ""),   # The directory you want to save the file in
     width = 17, height = 15,
     units = "cm", res = 600)

ggpubr::ggarrange(plotlist = late_prop, 
                  nrow=2, ncol = 2, 
                  common.legend = TRUE, 
                  legend="right")

dev.off()
