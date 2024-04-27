#' ab_test_next_instructor.R
#'
#' Instructor code for the lecture:
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
head(df)

# the usual approach
mod <- lm(post_spend ~ treatment_status,
          data = df)
tidy(mod)

# Get theta for CUPED
theta <-
    tidy(lm(post_spend ~ pre_spend, data = df)) %>%
    filter(term=="pre_spend") %>%
    select(estimate) %>%
    purrr::pluck('estimate')

print(theta)

# alternative way to get theta
cov(df$post_spend, df$pre_spend) / var(df$pre_spend)

# transform DV
df <-
    df %>%
    mutate(cuped_spend = post_spend -
               theta*(pre_spend - mean(df$pre_spend)
                      )
           )

# use CUPED Estimate
mod_cuped <- lm(cuped_spend ~ treatment_status,
                data = df)
tidy(mod_cuped)

# Alternative way to get CUPED estimate
df <-
    df %>%
    mutate(post_spend_tilde =
               resid(lm(post_spend ~ pre_spend,
                        data =df
                        )
                     ) +
               mean(df$post_spend)
           )

tidy(lm(post_spend_tilde ~ treatment_status, data = df))


# --- Standard Error Adjustments --- #
recommender <- read_csv("data/recommender_clustering.csv")

# What we've been doing
tidy(lm(log(revenue) ~ treatment_status, data = recommender))

# Heterosk Robust
summary(lm_robust(log(revenue) ~ treatment_status,
               data = recommender,
               se_type = "HC1")
     )

# Cluster Robust
summary(lm_robust(log(revenue) ~ treatment_status,
               data = recommender,
               cluster = user)
     )



