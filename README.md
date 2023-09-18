# Disinformation Intervention

The scripts for the paper "The Effect of Disinformation Intervention: Evidence from Trump’s Tweets and the 2020 Election"

## Scripts

| Script Name | Description | Input | Output |
|-------------|-------------|-------|--------|
| `./python/find_tweets.ipynb` | Finds retweets of Trump's restricted 2020 general election Tweets | 2020 Election fraud tweets collected by our Twitter monitor | - `./data/df_count.csv`: Counts of Trump's retweets over time. |
| `./R/step0_setup.R` | Installs required R packages | None | None |
| `./R/step1_figure_restriction.R` | Plots Figure 4 | - `./data/df_count.csv`: Counts of Trump's retweets over time. | - `./figures/cumulative_retweets.pdf`: Figure 4 |
| `./R/step2_extract_ts.R` | Extracts misinformation time series for each Trump's labeled tweet. | - `./data/trump_tweets_final.csv`: Trump's tweets data from Trump Twitter Archive and Factba.se. <br> - `./data/ros_ids_complete.csv`: 2020 election misinformation tweets dataset by Kennedy et. al. | - `./data/extracted_ts_v2.Rds`: misinformation time series for each Trump's labeled tweet. |
| `./R/step3_run_sc_all.R` | Conducts synthetic control analysis and creates Table 1 & 2 and Figure 5 & 6 | - `./data/extracted_ts_v2.Rds`: misinformation time series for each Trump's labeled tweet. | - `./figures/sc_warn.pdf`: Figure 5a. <br> - `./figures/sc_restrict.pdf`: Figure 5b. <br> - `./figures/treatment_effect_scatter_v2.pdf`: Figure 6. |

