library(tidyverse)

set.seed(1)

theme_set(theme_bw())

p1 <- ggplot() +
  geom_function(fun = ~ dbeta(., 2, 3)) +
  scale_x_continuous(
    "Probability of graduation",
    labels = scales::label_percent(1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(NULL, breaks = NULL, labels = NULL)

ggsave(
  "~/Documents/causal-fairness/presentation/p1.png",
  plot = p1,
  width = 4.5,
  height = 3
)

m2 <- tibble(x = rbeta(1e7, 2, 3)) %>%
  with(mean(x))

p2 <- p1 +
  stat_function(fun = ~ dbeta(., 2, 3), geom = "area",fill='#e0666620') +
  stat_function(fun = ~ 1/2 * dbeta(., 2, 3), geom = "area",fill='#93c47d70') 

ggsave(
  "~/Documents/causal-fairness/presentation/p2.png",
  plot = p2,
  width = 4.5,
  height = 3
)

p2.5 <- p2 +
  geom_vline(xintercept = m2, linetype = "dashed", color = "red", size = 1.5)

ggsave(
  "~/Documents/causal-fairness/presentation/p2.5.png",
  plot = p2.5,
  width = 4.5,
  height = 3
)

m3 <- tibble(x = rbeta(1e7, 2, 3)) %>%
  filter(x > qbeta(1/2.5, 2.5, 3)) %>%
  with(mean(x))

p3 <- p1 +
  stat_function(fun = ~ dbeta(., 2, 3), geom = "area",fill='#e0666620') +
  stat_function(fun = ~ dbeta(., 2, 3), geom = "area", xlim = c(qbeta(1/2.5, 2.5, 3), 1),fill='#93c47d70')

ggsave(
  "~/Documents/causal-fairness/presentation/p3.png",
  plot = p3,
  width = 4.5,
  height = 3
)

p3.5 <- p3 +
  geom_vline(xintercept = m3, linetype = "dashed", color = "red", size = 2)

ggsave(
  "~/Documents/causal-fairness/presentation/p3.5.png",
  plot = p3.5,
  width = 4.5,
  height = 3
)

p4 <- ggplot() +
  geom_function(fun = ~ dbeta(., 2, 3), data = tibble(A = "Minority")) +
  geom_function(fun = ~ dbeta(., 3, 2), data = tibble(A = "Majority")) +
  scale_x_continuous(
    "Probability of graduation",
    labels = scales::label_percent(1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(NULL, breaks = NULL, labels = NULL) +
  facet_grid(cols = vars(A), labeller = "label_parsed") +
  theme(panel.spacing = unit(1, "cm"))

ggsave(
  "~/Documents/causal-fairness/presentation/p4.png",
  plot = p4,
  width = 4.5,
  height = 3
)

p5 <- p4 +
  stat_function(
    fun = ~ dbeta(., 3, 2),
    geom = "area",
    xlim = c(0, 1),
    data = tibble(A = "Majority"),
    fill='#e0666620'
  ) +
  stat_function(
    fun = ~ dbeta(., 2, 3),
    geom = "area",
    xlim = c(0, 1),
    data = tibble(A = "Minority"),
    fill='#e0666620'
   )+
  stat_function(
    fun = ~ dbeta(., 3, 2),
    geom = "area",
    xlim = c(1/2, 1),
    data = tibble(A = "Majority"),
    fill='#93c47d70'
  ) +
  stat_function(
    fun = ~ dbeta(., 2, 3),
    geom = "area",
    xlim = c(1/2, 1),
    data = tibble(A = "Minority"),
    fill='#93c47d70'
  ) 






ggsave(
  "~/Documents/causal-fairness/presentation/p5.png",
  plot = p5,
  width = 4.5,
  height = 3
)

p6 <- p4 +
  stat_function(
    fun = ~ dbeta(., 3, 2),
    geom = "area",
    xlim = c(0, 1),
    data = tibble(A = "Majority"),
    fill='#e0666620'
  ) +
  stat_function(
    fun = ~ dbeta(., 2, 3),
    geom = "area",
    xlim = c(0, 1),
    data = tibble(A = "Minority"),
    fill='#e0666620'
  )+
    stat_function(
      fun = ~ dbeta(., 3, 2),
      geom = "area",
      xlim = c(qbeta(5/8, 3, 2), 1),
      data = tibble(A = "Majority"),
      fill='#93c47d70'
    ) +
    stat_function(
      fun = ~ dbeta(., 2, 3),
      geom = "area",
      xlim = c(qbeta(3/8, 2, 3), 1),
      data = tibble(A = "Minority"),
      fill='#93c47d70'
    )

ggsave(
  "~/Documents/causal-fairness/presentation/p6.png",
  plot = p6,
  width = 4.5,
  height = 3
)

p7 <- p4 +
  stat_function(
    fun = ~ dbeta(., 2, 3),
    geom = "area",
    data = tibble(A = "Minority"),
    fill='#e0666620'
  ) +
  stat_function(
    fun = ~ dbeta(., 3, 2),
    geom = "area",
    data = tibble(A = "Majority"),
    fill='#e0666620'
  )+
  stat_function(
    fun = ~ 1/2 * dbeta(., 2, 3),
    geom = "area",
    data = tibble(A = "Minority"),
    fill='#93c47d70'
  ) +
  stat_function(
    fun = ~ 1/2 * dbeta(., 3, 2),
    geom = "area",
    data = tibble(A = "Majority"),
    fill='#93c47d70'
  )

ggsave(
  "~/Documents/causal-fairness/presentation/p7.png",
  plot = p7,
  width = 4.5,
  height = 3
)

p8 <- ggplot() +
  scale_x_continuous(breaks = c(0:4 / 4), labels = scales::label_percent(),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = NULL) +
  ylab(NULL) +
  xlab("Probability of graduating") +
  geom_function(fun = ~ dbeta(., 2.5, 2.5), data = tibble(a = "Majority", y_ = "Would graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = "Minority", y_ = "Would graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 4, 4), data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted")) +
  facet_grid(vars(y_), vars(a), switch = "y")

ggsave(
  "~/Documents/causal-fairness/presentation/p8.png",
  plot = p8,
  width = 4.5,
  height = 3
)

p9 <- p8 + 
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(0.0, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.0, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.0, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 4, 4), xlim = c(0.0, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(0.5, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_area(stat = "function", fun = ~ dbeta(., 4, 4), xlim = c(0.5, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  annotate("text", x = 2/3, y = 1/2, label = "50%", color = "#274e13", size = 5) 
  

ggsave(
  "~/Documents/causal-fairness/presentation/p9.png",
  plot = p9,
  width = 4.5,
  height = 3
)

p10 <- ggplot() +
  scale_x_continuous(breaks = c(0:4 / 4), labels = scales::label_percent(),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = NULL) +
  ylab(NULL) +
  xlab("Probability of graduating") +
  geom_function(fun = ~ dbeta(., 2.5, 2.5), data = tibble(a = "Majority", y_ = "Would graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 3.1, 2.9), data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = "Minority", y_ = "Would graduate\nif admitted")) +
  geom_function(fun = ~ dbeta(., 3.9, 4.1), data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted")) +
  facet_grid(vars(y_), vars(a), switch = "y")


  

p11 <- p10 +
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(0.0, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3.1, 2.9), xlim = c(0.0, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.0, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3.9, 4.1), xlim = c(0.0, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(0.5, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 2.5, 2.5), digits = 2), "%"),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "#274e13",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.1, 2.9), xlim = c(0.5, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3.1, 2.9), digits = 2), "%"),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "red",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3, 3), digits = 2), "%"),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "#274e13",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.9, 4.1), xlim = c(0.5, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3.9, 4.1), digits = 2), "%"),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "red",
            mapping = aes(x = x, y = y),
            size = 5)

ggsave(
  "~/Documents/causal-fairness/presentation/p11.png",
  plot = p11,
  width = 4.5,
  height = 3
)

t_maj <- qbeta(1/2, 3.1, 2.9)
t_min <- qbeta(1/2, 3.9, 4.1)


  

p12 <- p10 +
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(0, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3.1, 2.9), xlim = c(0, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 3.9, 4.1), xlim = c(0, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#e0666620') +
  geom_area(stat = "function", fun = ~ dbeta(., 2.5, 2.5), xlim = c(t_maj, 1),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(t_maj, 2.5, 2.5), digits = 2), "%"),
            data = tibble(a = "Majority", y_ = "Would graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "red",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.1, 2.9), xlim = c(t_maj, 1),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(t_maj, 3.1, 2.9), digits = 2), "%"),
            data = tibble(a = "Majority", y_ = "Would not graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "#274e13",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(t_min, 1),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(t_min, 3, 3), digits = 2), "%"),
            data = tibble(a = "Minority", y_ = "Would graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "red",
            mapping = aes(x = x, y = y),
            size = 5) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.9, 4.1), xlim = c(t_min, 1),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted"),fill='#93c47d70') +
  geom_text(label = str_c(format(100 - 100 * pbeta(t_min, 3.9, 4.1), digits = 2), "%"),
            data = tibble(a = "Minority", y_ = "Would not graduate\nif admitted",
                          x = 2/3, y = 1/2),
            color = "#274e13",
            mapping = aes(x = x, y = y),
            size = 5)

ggsave(
  "~/Documents/causal-fairness/presentation/p12.png",
  plot = p12,
  width = 4.5,
  height = 3
)

