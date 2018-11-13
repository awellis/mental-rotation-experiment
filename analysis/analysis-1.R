library(fs)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(ggplot2)

## load data ----
# load(file = "././data/DATASET.Rda")
source("analysis/import-data.R")


## plot data ----
DATASET %>% ggplot(aes(x = rt, fill = rotation)) +
    # geom_histogram(aes(y = ..density..),
                   # bins = 60, alpha = 0.6, position = "identity") +
    geom_density(alpha = 0.6) +
    facet_grid(condition ~ subject) +
    scale_color_viridis_d() +
    theme_minimal()


## summarize data ----
se <- function(x) sd(x)/sqrt(length(x))

SUMMARY_RT <- DATASET %>%
    group_by(subject, condition, rotation, correct) %>%
    summarize_at(.funs = funs(mean, sd, median, se),
                 .vars = vars(rt))

SUMMARY_CORRECT <- DATASET %>%
    group_by(subject, condition, rotation) %>%
    summarize_at(.funs = funs(mean, sd, median, se),
                 .vars = vars(correct, rt))

SUMMARY_RT_aggregated <- DATASET %>%
    group_by(condition, rotation, correct) %>%
    summarize_at(.funs = funs(mean, sd, median, mean, se),
                 .vars = vars(rt))

SUMMARY_CORRECT_aggregated <- DATASET %>%
    group_by(condition, rotation) %>%
    summarize_at(.funs = funs(mean, sd, se1 = se, median, mean, se),
                 .vars = vars(correct, rt))

## plot summaries ----

SUBJECTS <- sample(unique(SUMMARY_RT$subject), 6)

SUMMARY_RT %>%
    filter(subject %in% SUBJECTS)  %>%
    mutate(correct = as.factor(correct)) %>%
    ggplot(aes(x = rotation, y = mean,
               color = correct,
               shape = correct)) +
    geom_point(size = 4) +
    geom_line(aes(group = correct)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
    facet_grid(subject ~ condition) +
    ylab("Mean response time (s)") +
    scale_color_viridis_d(alpha = 1.0, begin = 0, end = 0.8) +
    theme_minimal()


SUMMARY_CORRECT %>%
    # filter(subject %in% SUBJECTS)  %>%
    ggplot(aes(x = rotation, y = correct_mean,
                               color = condition, shape = condition))+
    geom_point(size = 4) +
    geom_line(aes(group = condition)) +
    geom_errorbar(aes(ymin= correct_mean-correct_se,
                      ymax=correct_mean+correct_se), width=.1) +
    facet_wrap(~subject) +
    ylab("Mean accuracy") +
    scale_y_continuous(limits = c(0.4, 1.0)) +
    scale_color_viridis_d(alpha = 1.0, begin = 0, end = 0.8) +
    theme_minimal()

SUMMARY_RT_aggregated %>%
    mutate(correct = as.factor(correct)) %>%
    ggplot(aes(x = rotation, y = mean,
               color = correct,
               shape = correct)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
    geom_line(aes(group = correct)) +
    facet_grid(~condition) +
    ylab("Mean response time (s)") +
    scale_color_viridis_d(alpha = 1.0, begin = 0, end = 0.8) +
    theme_minimal()

SUMMARY_CORRECT_aggregated %>%
    ggplot(aes(x = rotation, y = correct_mean,
               color = condition, shape = condition))+
    geom_point(size = 4) +
    geom_line(aes(group = condition)) +
    ylab("Mean accuracy") +
    scale_color_viridis_d(alpha = 1.0, begin = 0, end = 0.8) +
    theme_minimal()

## Analysis ----
library(brms)
# monotoonic effects in brms
# https://cran.r-project.org/web/packages/brms/vignettes/brms_monotonic.html

DATASET <- DATASET %>%
    mutate(angleF = ordered(angle ))


#
# formula_lognormal <- bf(rt ~ mo(angleF) + (mo(angleF) |p| subject),
#               sigma ~ mo(angleF) + (mo(angleF) |p| subject))
formula_lognormal <- bf(rt ~ mo(angleF) + (mo(angleF) |p| subject),
                        sigma ~ 1 + (1 |p| subject))

# prior <- c(set_prior("cauchy(0, 5)", class = "b"),
#            # set_prior("normal(1.5, 1)", class = "b", dpar = "sigma"),
#            set_prior("uniform(0, min_Y)", class = "Intercept", dpar = "ndt"))

fit1 <- brm(formula = formula_lognormal,
            family = lognormal(),
            data = filter(DATASET, correct == 1, condition == "self"),
            control = list(adapt_delta = 0.97))
            # prior = prior)
plot(marginal_effects(fit1))


formula_exgauss <- bf(rt ~ mo(angleF) + (1 |p| subject),
                sigma ~ 1 + (1 |p| subject),
                beta ~  mo(angleF) + (1 |p| subject))

fit2 <- brm(formula_exgauss,
            family = exgaussian(),
            data = filter(DATASET, correct == 1, condition == "self"),
            control = list(adapt_delta = 0.97))


