library(tidyverse)

df_count = read.csv("./data/df_count.csv",
              colClasses = c("POSIXct", "numeric", "character", "numeric"))

df_count = df_count %>% 
  filter(count != 0) %>% 
  group_by(id) %>% 
  mutate(
    count = ifelse(row_number() == n(), lag(count, 1), count),
    last_point = ifelse(row_number() == n(), "yes", "no")
  ) %>% 
  ungroup() %>% 
  mutate(
    log_count = log(count),
    log_x = log(x),
    date = strftime(time, format = "%b %d")
  )

fig = df_count %>% 
  ggplot(aes(x = x, y = count, group = id)) +
  annotate("rect", xmin = 20, xmax = 300,
           ymin = 0, ymax = 100000, alpha = .3) +
  geom_line(color = "grey60", size = 1) +
  geom_point(data = df_count %>% filter(last_point == "yes"),
             color = "red", size = 2) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "grey20") +
  ggrepel::geom_label_repel(aes(label = date),
            data = df_count %>% filter(last_point == "yes"),
            hjust = 0.7, vjust = 0.5, nudge_y = 0.3,
            fill = "white", label.padding = unit(0.25, "lines"),
            color = "grey20", size = 4, fontface = "bold") +
  annotate("text", x = 23, y = 600, label = "20 mins",
           color = "grey20", size = 4, angle = -90) +
  scale_x_sqrt(breaks = scales::extended_breaks(n = 10)) + 
  scale_y_sqrt() +
  coord_cartesian(ylim = c(180, 60000),
                  xlim = c(0, 240)) +
  xlab("Minutes") +
  ylab("Retweet Count") +
  ggtitle("Cumulative Retweets of Trump's Restricted Tweets") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("./figures/cumulative_retweets.pdf",
       fig, width = 6, height = 5,
       units = "in", limitsize = FALSE)
