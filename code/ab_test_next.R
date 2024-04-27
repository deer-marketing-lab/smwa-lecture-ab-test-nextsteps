#' ab_test_next.R
#'
#' Student script for the lecture:
#'  "A/B Testing: Next Steps"
#'
#' Social Media and Web Analytics, 2024

# --- Libraries ---# 
library(readr)
library(dplyr)
library(infer)
library(ggplot2)
library(broom)
library(estimatr)

# --- CUPED --- #
# load data
df <- read_csv("data/ad_campaign.csv")

# Your Code below

# --- Standard Error Adjustments --- #
recommender <- read_csv("data/recommender_clustering.csv")

# Your Code below



