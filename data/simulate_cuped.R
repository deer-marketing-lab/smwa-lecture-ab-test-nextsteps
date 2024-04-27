library(dplyr)

simulate_cuped <- function(seed = 1){
    #set.seed(42)
    n_obs = 2000
    beta0 = 100
    beta1 = 1.05
    lambda = 0.65
    gamma = beta0*(1 - lambda)
    # this is interesting, 
    # CUPED can take some of this bias out
    pre_diff = 0 #0.1*beta0
    
    d = rbinom(n_obs, 1, 0.5)
    y0 = beta0 + pre_diff*d + rnorm(n_obs, mean = 0, sd = 40)
    y1 = (1-d)*lambda*y0 + d*beta1*lambda*y0 + gamma + rnorm(n_obs, mean = 0, sd = 40)
    #y1 = lambda * y0 + rnorm(n_obs, mean = (1-lambda-.07)*y0, sd = 10) + beta1 * d + rnorm(n_obs, mean = 0, sd = 40)
    
    df <-
        tibble::tibble(
           pre_spend = y0,
           post_spend = y1,
           treatment_status = d
        ) %>%
        tibble::rowid_to_column("id") %>%
        select(id, treatment_status, pre_spend, post_spend) %>%
        mutate(pre_spend = round(if_else(pre_spend <0, 0, pre_spend), 2),
               post_spend = round(if_else(post_spend <5, 0, post_spend),2)
               )
    
    return(df)
    #readr::write_csv(df, "data/ad_campaign.csv")
}


mod <- lm(post_spend ~ treatment_status, 
          data = df)
tidy(mod)

theta <- 
    tidy(lm(post_spend ~ pre_spend, data = df)) %>%
    filter(term=="pre_spend") %>%
    select(estimate) %>%
    purrr::pluck('estimate')

print(theta)

df <- 
    df %>%
    mutate(cuped_spend = post_spend - 
               theta*(pre_spend - mean(df$pre_spend)
               )
    )

mod_cuped <- lm(cuped_spend ~ treatment_status, 
                data = df)
tidy(mod_cuped)

df %>%
    ggplot() +
    geom_density(aes(x = post_spend, fill = as.factor(treatment_status))) + 
    theme_bw() + 
    theme(legend.position="none")


df %>% group_by(treatment_status) %>% count()
## Sim

set.seed(42)
#reps <- replicate(1000, simulate_cuped())
all_data <- tibble::enframe(replicate(n = 5000, 
                                      simulate_cuped() ,
                                      simplify = FALSE)
                            ) %>%
    tidyr::unnest(cols = c(value)) %>%
    tidyr::nest(-name)

get_cuped <- function(df){
    theta <- 
        tidy(lm(post_spend ~ pre_spend, data = df)) %>%
        filter(term=="pre_spend") %>%
        select(estimate) %>%
        purrr::pluck('estimate')

    df <- 
        df %>%
        mutate(cuped_spend = post_spend - 
                   theta*(pre_spend - mean(df$pre_spend)
                   )
        )

    mod_cuped <- lm(cuped_spend ~ treatment_status, 
                    data = df)
    out <- 
        tidy(mod_cuped) %>%
        select(term, estimate) %>%
        filter(term == "treatment_status")
    return(out)
    } 


all_data <-
    all_data %>% 
    mutate(no_cuped = purrr::map(data, ~lm(post_spend ~ treatment_status, data = .) %>%
                                        tidy %>%
                                   select(term, estimate) %>%
                                   filter(term == "treatment_status")
                               ),
           cuped = purrr::map(data, ~ get_cuped(.))
               )

all_data2 <-
    all_data %>%
    tidyr::unnest(no_cuped, names_sep = "_") %>%
    tidyr::unnest(cuped, names_sep = "_")

all_data2 %>%
    ggplot() +
    geom_histogram(aes(x = no_cuped_estimate), alpha = 0.2, fill = "steelblue") +
    geom_histogram(aes(x = cuped_estimate), alpha = 0.2, fill = "red") +
    geom_vline(xintercept = 5, color = "black", linetype = "dashed") +
    xlab("Coefficient Estimate") +
    theme_bw()

ggsave("figs/comparison_cuped.png")

var(all_data2$no_cuped_estimate)
var(all_data2$cuped_estimate)

mean(all_data2$no_cuped_estimate)
mean(all_data2$cuped_estimate)
