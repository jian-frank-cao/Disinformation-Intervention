## Functions -------------------------------------------------------------------
# do synth
do.synth = function(df, dep.var, dependent.id, predictors,
                    special.predictors, time.predictors.prior,
                    time.optimize.ssr){
  # find v
  dataprep.out <-
    Synth::dataprep(
      foo = df,
      predictors    = eval(predictors),
      dependent     = dep.var,
      unit.variable = 1,
      time.variable = 3,
      special.predictors = eval(special.predictors),
      treatment.identifier = dependent.id,
      controls.identifier = setdiff(unique(df$id), dependent.id),
      time.predictors.prior = time.predictors.prior,
      time.optimize.ssr = time.optimize.ssr, 
      unit.names.variable = 2,
      time.plot = sort(unique(df$time))
    )
  
  # fit training model
  synth.out <- 
    Synth::synth(
      data.prep.obj=dataprep.out,
      Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
    )
  
  value = df %>% filter(id == dependent.id) %>% `$`(value_raw)
  average = df %>% filter(id != dependent.id) %>% group_by(time) %>% 
    summarise(average = mean(!!sym(dep.var), na.rm = TRUE)) %>% `$`(average)
  synthetic = dataprep.out$Y0%*%synth.out$solution.w %>% as.numeric
  
  return(list(value = value,
              average = average,
              synthetic = synthetic,
              weights = synth.out$solution.w))
}


sc.analysis = function(data, target.id, donor.id, treat.t = 20){
  # prepare data
  df = data %>% 
    filter(id %in% c(target.id, donor.id))
  df.rescale = df %>% 
    filter(time <= treat.t) %>% 
    group_by(unit) %>% 
    summarise(value.min = min(value),
              value.max = max(value)) %>% 
    ungroup()
  mean.diff = mean(df.rescale$value.max - df.rescale$value.min)
  df.rescale = df.rescale %>% 
    mutate(
      multiplier = mean.diff/(value.max - value.min)
    )
  df = left_join(df, df.rescale, by = "unit")
  target.min = df %>% filter(id == target.id) %>% .[1,"value.min"]
  target.max = df %>% filter(id == target.id) %>% .[1,"value.max"]
  target.multiplier = df %>% filter(id == target.id) %>% .[1,"multiplier"]
  df = df %>% 
    mutate(value_raw = value,
           value = (value - value.min)*multiplier)
  
  # synthetic control
  res = do.synth(df = df,
                 dep.var = "value", 
                 dependent.id = target.id, 
                 predictors = NULL,
                 special.predictors = list(list("value", 10:19, c("mean"))),
                 time.predictors.prior = 1:19,
                 time.optimize.ssr = 1:19)
  value = res$value
  value.sc = res$synthetic/target.multiplier + target.min
  
  effect = value[treat.t:(treat.t+60)] - value.sc[treat.t:(treat.t+60)]
  avg.effect = mean(effect/value.sc[treat.t:(treat.t+60)])
  
  df.res = rbind(df %>% 
                   select(c("id", "unit", "time", "value")) %>% 
                   mutate(value = value/target.multiplier + target.min),
                 data.frame(id = 0,
                            unit = "sc",
                            time = 1:length(value.sc),
                            value = value.sc))
  df.res = df.res %>%
    group_by(unit) %>%
    mutate(moving_avg3 = zoo::rollmean(value, k=10, fill=NA, align='left')) %>% 
    ungroup
  
  df.res = df.res %>% 
    mutate(colors = case_when(id == 0 ~ "Synthetic Control",
                              id == target.id ~ "Target",
                              TRUE ~ "Donors"))
  
  treat.a = df.res %>% filter(id == 0) %>% .[[75, "value"]]
  treat.b = df.res %>% filter(id == target.id) %>% .[[75, "value"]]
  
  # plot figure
  fig = df.res %>% 
    ggplot(aes(x = time, y = value, group = unit, color = colors)) +
    annotate("rect", xmin = 20, xmax = 80,
             ymin = -0.2*(target.max), ymax = 1.2*(target.max), alpha = .2) +
    geom_vline(xintercept = 20, linetype="dashed", col = "grey20") +
    geom_line(size = 1, data = df.res %>% filter(colors == "Donors")) +
    geom_line(size = 1, data = df.res %>% filter(colors == "Target")) +
    geom_line(size = 1, data = df.res %>% filter(colors == "Synthetic Control")) +
    scale_color_manual(name = NULL,
                       values = c("Synthetic Control" = "#fe4a49",
                                  "Target" = "#2ab7ca",
                                  "Donors" = "grey70")) +
    scale_x_continuous(breaks=(1:7)*20-20) +
    annotate("segment", x = 75,
             y = treat.a,
             xend = 75,
             yend = treat.b,
             arrow = arrow(ends = "both", length = unit(.2,"cm")),
             colour = "grey20", size = 0.8) +
    annotate("label", x = 75, y = (treat.a + treat.b)/2, size = 3,
             label = paste0("Estimated\nTreatment Effect\n", 
                            round(avg.effect*100,2), "%"), col = "grey20") +
    annotate("text", x = 23, y = 0.1*(target.max), label = "20 mins",
             color = "grey20", size = 4, angle = -90) +
    coord_cartesian(ylim = c(0, target.max + 50),
                    xlim = c(0, 120)) +
    xlab("Time") +
    ylab("Tweet Count per Minute") +
    theme_bw() +
    theme(legend.position = c(0.8,0.8))
  
  return(list(df.res = df.res,
              value = value,
              value.sc = value.sc,
              fig = fig))
}