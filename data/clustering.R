# simulate a clustered data set
n_customer <- 2000

beta0 = 100
beta1 = 2.5

set.seed(12345)
d = rbinom(n_customer, 1, 0.5)
# user_shock
user_shock <- rnorm(n_customer, sd = 45)

df <- tibble::tibble(
    treatment_status = d,
    user_shock = user_shock,
) %>%
    tibble::rowid_to_column("user")

# some percentage buy twice
two_purchases   <- sample_n(df, 0.2*n_customer)
# some three times
three_purchases <- sample_n(df, 0.075*n_customer)
# aggregate df    
combi_df <- df %>%
    rbind(two_purchases, three_purchases)

combi_df <-
    combi_df %>%
    mutate(revenue = beta0 + beta1 * d + user_shock + rnorm(n(), mean = 0, sd = 35)
           ) %>%
    mutate(revenue = round(if_else(revenue < 0, 15, revenue), 2))

my_df <-
    combi_df %>%
    select(-user_shock)

tidy(lm(revenue ~ treatment_status, data = my_df))

library(estimatr)
options(digits = 3)

tidy(lm_robust(revenue ~ treatment_status, se_type = "HC1", data = my_df))

tidy(lm_robust(revenue ~ treatment_status, clusters = user, data = my_df))

write_csv(my_df, "data/recommender_clustering.csv")
