simulate_gst_data <- function(seed = 1){
    #set.seed(42)
    n_obs = 1000
    beta0 = 100
    beta1 = 2.5
    
    d = rbinom(n_obs, 1, 0.5)
    revenue = beta0 + beta1 * d + + rnorm(n_obs, mean = 0, sd = 35)
    df <-
        tibble::tibble(
            revenue = revenue,
            treatment_status = d,
        ) %>%
        tibble::rowid_to_column("user") %>%
        mutate(day_of_experiment = 1 + user %/% 100) %>%
        mutate(revenue = round(if_else(revenue <0, 0, revenue), 2)
        )
    
    return(df)
    #readr::write_csv(df, "data/peeking.csv")
}


# analyse single data set
out <-
    purrr::map_df(min(df$day_of_experiment): max(df$day_of_experiment),
              ~ df %>%
                  filter(between(day_of_experiment, min_date, .x)),
              .id = "n_peek"
    ) %>%
    tidyr::nest(data = -n_peek)
           
out2 <-
    out %>%
    mutate(mod = purrr::map(data, 
                            ~tidy(lm(revenue ~ treatment_status, data = .),
                                  conf.int = TRUE)
                            )
    ) %>%
    tidyr::unnest(mod) %>%
    filter(term == "treatment_status")

out2 %>%
    ggplot() +
    geom_point(aes(x = as.numeric(n_peek), y = estimate)) +
    geom_errorbar(aes(x = as.numeric(n_peek), ymin = conf.low, ymax = conf.high,
                      width=0.2)
                  ) +
    geom_hline(yintercept= 0, linetype="dashed", 
               color = "red", size=1) +
    scale_x_continuous(breaks = round(seq(min(out2$n_peek), max(out2$n_peek), by = 1))) +
    theme_bw()
