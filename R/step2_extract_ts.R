## Setup -----------------------------------------------------------------------
library(tidyverse)
library(lubridate)


## Data ------------------------------------------------------------------------
df_trump = read.csv("./data/trump_tweets_final.csv",
                    colClasses = c("character", "character", "numeric",
                                   "character", "character", "numeric",
                                   "character", "numeric", "numeric"))

df_misinfo = read.csv("./data/ros_ids_complete.csv",
                      colClasses = c("character", "character", "character",
                                     "character", "character", "character",
                                     "character", "character"))

df_trump$date = as.POSIXct(df_trump$date, "GMT")
df_misinfo$created_at = as.POSIXct(df_misinfo$created_at, "GMT")

df_misinfo = df_misinfo %>% distinct

## Functions -------------------------------------------------------------------
# one tweet
one_tweet = function(data, target, target_time, label,
                     restricted_ids, n_mention = 10){
  ind_include = which(data$id == target)
  ind_retweet = which(data$retweeted_status_id == target)
  ind_quote = which(data$quoted_status_id == target)
  ind_reply = which(data$in_reply_to_status_id == target)
  if (length(c(ind_include, ind_retweet, ind_quote, ind_reply)) == 0) {
    return(NULL)
  }
  
  main_story = data$story[ind_include]
  mentioned = data[unique(c(ind_retweet, ind_quote, ind_reply)),]
  stories = data.frame(table(mentioned$story)) %>% 
    filter(Freq > n_mention) %>% .[["Var1"]] %>% as.character
  stories = unique(c(stories, main_story))
  
  second(target_time) = 0
  time_range = seq(target_time - 3600, length.out = 300, by = '1 min')
  
  if (length(stories) == 0) {
    return(NULL)
  }
  
  result = NULL
  for (item in stories) {
    window = data %>% 
      filter(created_at >= min(time_range) &
               created_at <= max(time_range) &
               !(retweeted_status_id %in% restricted_ids) &
               story == item)
    window$time = window$created_at
    second(window$time) = 0
    counts = window %>% 
      group_by(time) %>% 
      summarise(volume = n(),
                n_include = sum(id == target),
                n_retweet = sum(retweeted_status_id == target),
                n_quote = sum(quoted_status_id == target),
                n_reply = sum(in_reply_to_status_id == target),
                partisan_left = sum(partisan_lean == "left"),
                partisan_right = sum(partisan_lean == "right"),
                partisan_unknown = sum(partisan_lean == "unknown")) %>% 
      ungroup
    counts = full_join(counts, data.frame(time = time_range), by = "time")
    counts[is.na(counts)] = 0
    counts$story = item
    result = rbind(result, counts)
  }
  result = result %>% 
    mutate(trump_id = target,
           trump_time = target_time,
           trump_label = label)
  
  return(result)
}


## Run -------------------------------------------------------------------------
target = "1324004491612618752"
time = as.POSIXct("2020-11-04 15:04:04", "GMT")

restricted_ids = df_trump %>% filter(hard == 1) %>% .[["id"]]

results = NULL
for (i in 1:nrow(df_trump)) {
  target = df_trump$id[i]
  time = df_trump$date[i]
  label = ifelse(df_trump$hard[i] == 1, "restricted",
                 ifelse(df_trump$soft[i] == 1, "warned", "unrestricted"))
  results[[target]] = one_tweet(df_misinfo, target, time,
                                label, restricted_ids, n_mention = 10)
  print(paste0(i, ": ", time, "...", ifelse(is.null(results[[target]]),
                                            "No Match", "Match Found")))
}

data = results %>% 
  do.call("rbind", .) %>% 
  `rownames<-`(NULL)

saveRDS(data, "./data/extracted_ts_v2.Rds")
