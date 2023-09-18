## Setup -----------------------------------------------------------------------
library(parallel)
n.cores = detectCores()
library(tidyverse)
library(furrr)
plan(multisession, workers = n.cores - 1)
options(future.rng.onMisuse="ignore")
options(stringsAsFactors = FALSE)

library(Synth)
source("./R/utility/synthetic_control.R")


## Data v1 ---------------------------------------------------------------------
# data_include = read.csv("./data/merged.csv")
# data_quote = read.csv("./data/merged_quoted.csv")
# 
# data_include = data_include %>% 
#   select(-X) %>% 
#   mutate(time = time + 1,
#          type = "included")
# 
# data_quote = data_quote %>% 
#   select(-X) %>% 
#   mutate(time = time + 1,
#          type = "quoted")
# 
# data = rbind(data_include, data_quote)
# 
# data = data %>% 
#   filter(time <= 120)


## Data v2 ---------------------------------------------------------------------

data = readRDS("./data/extracted_ts_v2.Rds")

impact_tweets = data %>%
  group_by(story, trump_id, trump_label) %>%
  summarise(
    include = sum(n_include),
    avg_60 = mean(volume[1:60]),
    avg_120 = mean(volume[61:120]),
    ratio = avg_120/avg_60
  ) %>%
  filter(ratio > 1)

data = semi_join(data, impact_tweets[,c(1,2)],
                 by = c("story", "trump_id"))

count_tweet = data %>% 
  group_by(story, trump_id, trump_label,) %>% 
  summarise(
    is_include = ifelse(any(n_include > 0), 1, 0),
    n_mention = sum(n_retweet) + sum(n_quote) + sum(n_reply),
    score = is_include*100000000 + n_mention
  ) %>% 
  ungroup %>% 
  filter(score > 0) %>% 
  group_by(trump_id, trump_label) %>% 
  mutate(
    target = ifelse(score == max(score), 1, 0)
  ) %>% 
  filter(target == 1) %>% 
  slice(1) %>% 
  ungroup
  

count_label = count_tweet %>% 
  group_by(story) %>% 
  summarise(
    n_restrict = sum(trump_label == "restricted"),
    n_warn = sum(trump_label == "warned"),
    n_unrestrict = sum(trump_label == "unrestricted")
  ) %>% 
  filter(n_restrict > 0 | n_warn > 0)

count_tweet = count_tweet %>% 
  filter(story %in% count_label$story)


## Functions -------------------------------------------------------------------
one_tweet = function(task){
  result = sc.analysis(data = task$df,
                       target.id = 1,
                       donor.id = 2:nrow(task$donors))
  result[["target"]] = task$target
  result[["donors"]] = task$donors
  
  return(result)
}


## Run -------------------------------------------------------------------------
tasks = NULL
for (i in 1:nrow(count_tweet)) {
  target_label = count_tweet$trump_label[i]
  if (target_label %in% c("restricted", "warned")) {
    target_id = count_tweet$trump_id[i]
    target_story = count_tweet$story[i]
    n_unrestrict = count_label$n_unrestrict[count_label$story == target_story]
    df1 = data %>%
      filter(story == target_story,
             trump_id == target_id)
    if (n_unrestrict >= 5) {
      donors = count_tweet %>% 
        filter(story == target_story, trump_label == "unrestricted")
    }else{
      donors = count_tweet %>% 
        filter(trump_label == "unrestricted")# %>% 
        #sample_n(300)
    }
    df2 = semi_join(data, donors[,c(1,2)], by = c("story", "trump_id"))
    df = rbind(
      data.frame(id = 1,
                 unit = df1$trump_id,
                 time = 1:300,
                 value = df1$volume),
      data.frame(id = rep(2:(nrow(donors)+1), each = 300),
                 unit = df2$trump_id,
                 time = rep(1:300, nrow(donors)),
                 value = df2$volume)
    )
    df = df %>% filter(time > 60 & time <= 180)
    df$time = df$time - 60
    tasks[[target_id]] = list(target = count_tweet[i,],
                              donors = donors,
                              df = df)
    print(i)
  }
}


results = NULL
for (i in 1:length(tasks)) {
  task = tasks[[i]]
  results[[i]] = one_tweet(task)
  print(i)
}



## Figure: Synthetic Control -- Warned -----------------------------------------
set.seed(20200101)

df_trump = read.csv("./data/trump_tweets_final.csv",
                    colClasses = c("character", "character", "numeric",
                                   "character", "character", "numeric",
                                   "character", "numeric", "numeric"))
trump_warn = data %>% 
  filter(trump_label == "warned") %>% 
  select(trump_id, trump_time) %>% 
  distinct

trump_warn = left_join(
  trump_warn,
  df_trump %>% 
    filter(soft == 1) %>% 
    select(id, date),
  by = c("trump_id" = "id")
) %>% 
  arrange(date)

  
for (i in 1:length(results)) {
  names(results)[i] = results[[i]]$target$trump_id
}

figs_warn = NULL
sample_warn = sort(sample(1:nrow(trump_warn), 15))
for (i in 1:length(sample_warn)) {
  id = trump_warn$trump_id[sample_warn[i]]
  time = trump_warn$date[sample_warn[i]]
  df = results[[id]]
  df.res = df$df.res %>% 
    filter(id %in% c(0,1,sample(2:max(df$df.res$id), 5)))
  value.max = max(df.res %>% filter(id %in% c(0, 1)) %>% .[["value"]],
                  na.rm = TRUE)
  value.min = min(df.res %>% filter(id %in% c(0, 1)) %>% .[["value"]],
                  na.rm = TRUE)
  value.110 = df.res %>% filter(id == 1) %>% .[[100, "value"]]
  if (i == 1) {
    legend_position = c(0.7, 0.7)
  }else{
    legend_position = "none"
  }
  figs_warn[[id]] = df.res %>% 
    ggplot(aes(x = time, y = value, group = unit, color = colors)) +
    annotate("rect", xmin = 20, xmax = 120,
             ymin = -0.2*(value.max), ymax = 1.2*(value.max), alpha = .1) +
    geom_vline(xintercept = 20, linetype="dashed", col = "grey20") +
    geom_line(size = 1, data = df.res %>% filter(colors == "Target")) +
    geom_line(size = 1, data = df.res %>%
                filter(colors == "Synthetic Control")) +
    scale_color_manual(name = NULL,
                       labels = c("Synthetic Control" = "No Restriction",
                                  "Target" = "Observed"),
                       values = c("Synthetic Control" = "#fe4a49",
                                  "Target" = "#2ab7ca")) +
    scale_x_continuous(breaks=(1:7)*20-20) +
    coord_cartesian(ylim = c(value.min, value.max),
                    xlim = c(0, 120)) +
    ggtitle(paste0(time, " GMT")) +
    theme_bw() +
    theme(legend.position = legend_position,
          axis.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_rect(color = "grey20"),
          plot.title = element_text(size = 12, hjust = 0.5))
}

fig_warn = ggpubr::ggarrange(plotlist = figs_warn, ncol = 3, nrow = 5)

fig_warn <- ggpubr::annotate_figure(
  fig_warn,
  bottom = ggpubr::text_grob("Time", face = "bold", size = 12),
  left = ggpubr::text_grob("Tweet Count per Minute",
                           face = "bold", size = 12, rot = 90)
)

ggsave("./figures/sc_warn.pdf",
       fig_warn, width = 8, height = 10,
       units = "in", limitsize = FALSE)


## Figure: Synthetic Control -- Restricted -------------------------------------
results = readRDS("./data/sc_results.Rds")

df_trump = read.csv("./data/trump_tweets_final.csv",
                    colClasses = c("character", "character", "numeric",
                                   "character", "character", "numeric",
                                   "character", "numeric", "numeric"))
trump_restrict = data %>% 
  filter(trump_label == "restricted") %>% 
  select(trump_id, trump_time) %>% 
  distinct

trump_restrict = left_join(
  trump_restrict,
  df_trump %>% 
    filter(hard == 1) %>% 
    select(id, date),
  by = c("trump_id" = "id")
)

for (i in 1:length(results)) {
  names(results)[i] = results[[i]]$target$trump_id
}

figs_restrict = NULL
for (i in 1:nrow(trump_restrict)) {
  id = trump_restrict$trump_id[i]
  time = trump_restrict$date[i]
  df = results[[id]]
  df.res = df$df.res %>% 
    filter(id %in% c(0,1,sample(2:max(df$df.res$id), 5)))
  value.max = max(df.res %>% filter(id %in% c(0, 1)) %>% .[["value"]],
                  na.rm = TRUE)
  value.min = min(df.res %>% filter(id %in% c(0, 1)) %>% .[["value"]],
                  na.rm = TRUE)
  value.110 = df.res %>% filter(id == 1) %>% .[[100, "value"]]
  if (i == 1) {
    legend_position = c(0.7, 0.7)
  }else{
    legend_position = "none"
  }
  figs_restrict[[id]] = df.res %>% 
    ggplot(aes(x = time, y = value, group = unit, color = colors)) +
    annotate("rect", xmin = 20, xmax = 120,
             ymin = -0.2*(value.max), ymax = 1.2*(value.max), alpha = .1) +
    geom_vline(xintercept = 20, linetype="dashed", col = "grey20") +
    geom_line(size = 1, data = df.res %>% filter(colors == "Target")) +
    geom_line(size = 1, data = df.res %>%
                filter(colors == "Synthetic Control")) +
    scale_color_manual(name = NULL,
                       labels = c("Synthetic Control" = "No Restriction",
                                  "Target" = "Observed"),
                       values = c("Synthetic Control" = "#fe4a49",
                                  "Target" = "#2ab7ca")) +
    scale_x_continuous(breaks=(1:7)*20-20) +
    coord_cartesian(ylim = c(value.min, value.max),
                    xlim = c(0, 120)) +
    ggtitle(paste0(time, " GMT")) +
    theme_bw() +
    theme(legend.position = legend_position,
          axis.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_rect(color = "grey20"),
          plot.title = element_text(size = 12, hjust = 0.5))
}

fig_restrict = ggpubr::ggarrange(plotlist = figs_restrict, ncol = 3, nrow = 5)

fig_restrict <- ggpubr::annotate_figure(
  fig_restrict,
  bottom = ggpubr::text_grob("Time", face = "bold", size = 12),
  left = ggpubr::text_grob("Tweet Count per Minute",
                           face = "bold", size = 12, rot = 90)
)

ggsave("./figures/sc_restrict.pdf",
       fig_restrict, width = 8, height = 10,
       units = "in", limitsize = FALSE)


## Figure: Treatment Effect ----------------------------------------------------

results = readRDS("./data/sc_results.Rds")

df_sc = NULL
for (item in results) {
  res = data.frame(id = item$target$trump_id,
                   time = 1:120,
                   value = item$value,
                   value.sc = item$value.sc,
                   story = item$target$story,
                   label = item$target$trump_label,
                   include = item$target$is_include,
                   n_mention = item$target$n_mention)
  df_sc = rbind(df_sc, res)
}

write_csv(df_sc, "./data/df_sc.csv")

filter_active = df_sc %>% 
  group_by(story, id) %>% 
  summarise(
    mean_value = mean(value)
  ) %>% 
  ungroup %>% 
  filter(mean_value > 100)

res = semi_join(df_sc, filter_active, by = c("story", "id"))

res = res %>% 
  group_by(story, id, label, include) %>% 
  summarise(
    treat_effect = mean((value[60:120] - value.sc[60:120])/value.sc[60:120]),
    mean.value = mean(value[60:120]),
    mean.value.sc = mean(value.sc[60:120]),
    ratio = mean(value[60:120]/value.sc[60:120]),
    log_ratio = log(mean(value[60:120])/mean(value.sc[60:120]))
  )

df_time = data %>% 
  select(trump_id, trump_time) %>% 
  distinct

res = left_join(res, df_time, by = c("id" = "trump_id"))
res = res %>% filter(trump_time >= as.POSIXct("2020-11-04 00:00:00", "GMT"))

res$story = forcats::fct_infreq(res$story)
res = res %>% arrange(desc(label))

# t test
ttest_result <- t.test(res$log_ratio)

# Create a data frame to hold the t-test results
result_df <- data.frame(
  Mean = as.character(round(ttest_result$estimate[1],4)),
  t = as.character(round(ttest_result$statistic,4)),
  df = as.character(round(ttest_result$parameter,0)),
  `p-value` = as.character(round(ttest_result$p.value,4))
)

# Convert the data frame to a LaTeX table
latex_table <- xtable::xtable(result_df)

# Print the LaTeX code for the table
print(latex_table, type = "latex", table.placement="t",
      caption.placement="top", include.rownames = FALSE)

fig = res %>% 
  ggplot(aes(x = trump_time, y = exp(log_ratio),
             shape = label, color = story)) +
  annotate("rect", xmin = as.POSIXct("2020-10-15", tz = "GMT"),
           xmax = as.POSIXct("2020-12-30", tz = "GMT"),
           ymin = 1, ymax = 60, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = as.POSIXct("2020-10-15", tz = "GMT"),
           xmax = as.POSIXct("2020-12-30", tz = "GMT"),
           ymin = 0, ymax = 1, alpha = 0.15, fill = "green") +
  geom_hline(yintercept = 1, linetype = "solid", color = "grey20") +
  geom_point(size = 4, stroke = 1) +
  scale_shape_manual(name = "Label", values = c(13, 19)) +
  ggsci::scale_color_d3(name = "Story", palette = "category20") +
  # geom_point(data = res %>% filter(include == 1), shape = 1,
  #            color = "red", size = 8, stroke = 1) +
  scale_y_log10(breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16), 
                labels = scales::percent) +
  scale_x_datetime(date_breaks="6 days",
                   date_labels="%b %d") +
  coord_cartesian(ylim = c(0.125,16), 
                  xlim = c(as.POSIXct("2020-11-01", tz = "GMT"),
                           as.POSIXct("2020-12-16", tz = "GMT"))) +
  xlab("Time") +
  ylab("Observed Tweets/SC Estimated Tweets") +
  theme_bw() +
  theme(legend.position = "top",#c(0.5,0.85),
        #legend.background = element_rect(colour = "grey20"),
        legend.box = "vertical") +
  guides(color = guide_legend(order = 2, ncol = 2),
         shape = guide_legend(order = 1, ncol = 2))
  
  
ggsave("./figures/treatment_effect_scatter_v2.pdf",
       fig, width = 7, height = 7,
       units = "in", limitsize = FALSE)

## Table count tweets ----------------------------------------------------------
latex_table = xtable::xtable(count_label %>% 
                 `colnames<-`(c("Story", "Restricted",
                                "Warned", "Unrestrict")))

# Print the LaTeX code for the table
print(latex_table, type = "latex", table.placement="t",
      caption.placement="top", include.rownames = FALSE)


## Table ratio -----------------------------------------------------------------
# Table: Ratio
count_restrict = hist(res %>% filter(label == "restricted") %>% .[["ratio"]],
                      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16))$counts
count_warn = hist(res %>% filter(label == "warned") %>% .[["ratio"]],
                  breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16))$counts

print(count_restrict/sum(count_restrict))
print(count_warn/sum(count_warn))


# res %>% 
#   ggplot(aes(x = ratio, fill = label)) +
#   geom_histogram(data = res %>% filter(label == "warned"),
#                  breaks = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
#                  color = "grey20") +
#   geom_histogram(data = res %>% filter(label == "restricted"),
#                  breaks = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
#                  color = "grey20") +
#   scale_fill_manual(name = NULL, values = c("#fe4a49","#2ab7ca")) +
#   scale_x_log10(breaks = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16), 
#                 labels = scales::percent) +
#   stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
#   geom_vline(aes(xintercept = 1), color = "black",
#              linetype = "dashed", size = 1.25) +
#   xlab("Ratio") +
#   ylab("Count") +
#   theme_bw() +
#   theme(legend.position = c(0.8, 0.8)) +
#   coord_flip() 






