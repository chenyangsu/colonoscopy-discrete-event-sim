library(ggplot2)
library(data.table)

# set theme for plotting
theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"), 
                  
                           panel.grid.major = element_blank(), 
                           
                           panel.grid.minor = element_blank(), 
                           
                           panel.border = element_blank(), 
                           
                           panel.background = element_blank()) ) 


# Plotting heatmap reference: https://statisticsglobe.com/add-values-heatmap-r


# load data ----
results_df <- fread("./3_intermediate/results.csv")

# subset dataframe ----
waittime <- results_df[,c("fit_compliance", "capacity", "median_wait_time")]  # median waitlist time
waitsize <- results_df[,c("fit_compliance", "capacity", "final_waitlist_size")]  # waitlist size

# Median Wait time ----
ggp1 <- ggplot(waittime, aes(fit_compliance, capacity)) +    # Create default ggplot2 heatmap
  geom_tile(aes(fill = median_wait_time)) +
  geom_text(aes(label = round(median_wait_time))) +    # Add values & modify color
  scale_fill_gradient(low = "white", high = "#1b98e0") +
  xlab("FIT compliance rate") +
  ylab("Capacity") +
  labs(fill = "Median wait time (days)")+  # change legend name
  scale_x_continuous(breaks = unique(waittime$fit_compliance)) +  # add all x ticks
  scale_y_continuous(breaks = unique(waittime$capacity))  # add all y ticks
ggp1       
ggsave("./4_output/fig4a_median_waittime.svg", plot = ggp1)

                                        


# Waitlist size ----
ggp2 <- ggplot(waitsize, aes(fit_compliance, capacity)) +    # Create default ggplot2 heatmap
  geom_tile(aes(fill = final_waitlist_size)) +
  geom_text(aes(label = final_waitlist_size)) +    # Add values & modify color
  scale_fill_gradient(low = "white", high = "#e01b49") +
  xlab("FIT compliance rate") +
  ylab("Capacity") +
  labs(fill = "Final waitlist size")+
  scale_x_continuous(breaks = unique(waitsize$fit_compliance)) +
  scale_y_continuous(breaks = unique(waitsize$capacity))
ggp2       
ggsave("./4_output/fig4b_waitlist_size.svg", plot = ggp2)

