Simulation
================
Hamed Nilforoshan
2/10/2021

## Overview

We construct a stylized example illustrating the sub-optimal outcomes of
Wang and Blei’s fairness definitions (Affirmative Action and Equal
Opportunity) on utility.

## Setup

A “book of life” is a draw of each potential outcome for a population of
a given size. We’ll define a function that generates a book of life for
a given set of parameters. T

``` r
gen_book_of_life <- function(
  pop_size,
  frac_black,
  lambda_1,
  lambda_2,
  lambda_3,
  beta_1,
  beta_2,
  beta_3,
  ...
) {
  
  # Define the structural equations.
  f_T <- function(r_i, m_i){
    return(round(pmax(0,pmin(lambda_1*r_i + lambda_2*m_i + rnorm(pop_size, mean = 0, sd = lambda_3) ,100)),digits=2))
  }
  
  f_A <- function(t_i, r_i){
    return(round(sigmoid(beta_1*t_i + beta_2*r_i + rnorm(pop_size, mean = 0, sd = beta_3) - 60.0), digits = 0))
  }
  

  # Generate the book of life.
  book_of_life <- tibble(
      # Generate the exogenous variables.
      R = as.integer(rbernoulli(pop_size, frac_black)), # Race (Black=1, White=0)
      M = runif(pop_size), # Medical Ability [0, 1]
      
      # Generate the endogenous variables.
      T_black = f_T(1,M), # Test Score black
      T_white = f_T(0,M), # Test Score black
      T = if_else(R==1,T_black,T_white),
      A = f_A(T, R), # Admissions Committee Decision 
      
      # Generate potential outcomes
      L_p = M * 100, # Lives saved if admitted
      D_p = R , # Added diversity if admitted  
      
      # Generate observed outcomes
      L_o = L_p * A, # Lives saved observed
      D_o = D_p * A  # Added diversity observed 
      
  )
}
```

## Initialize parameters and books of life

We initialize the training population size for the book of life, the
fraction of the population that is a minority (black), and the number of
counterfactual draws when calculating affirmative action decisions.

We then initial two books of life. One is for training our models, and
the second is for testing what happens when the models are applied to a
new, unseen dataset

``` r
POP_SIZE = 10000000
POP_SIZE_TEST = 500
POP_SIZE_TEST_ADMIT = 250
FRAC_BLACK = 0.2
COUNTER_FACTUAL_SAMPLE = 10000

df_train <-gen_book_of_life(POP_SIZE,FRAC_BLACK,-30,100,10,2,-25,5)
df_test <-gen_book_of_life(POP_SIZE_TEST,FRAC_BLACK,-30,100,10,2,-25,5)
```

## Use the biased admissions committee model as the baseline

``` r
ml_naive <- glm(A ~ T + R, data = df_train, family = "binomial", control = list(maxit = 1000))
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(ml_naive)
```

    ## 
    ## Call:
    ## glm(formula = A ~ T + R, family = "binomial", data = df_train, 
    ##     control = list(maxit = 1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.9650  -0.0001   0.0000   0.0001   3.8866  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -21.728561   0.032448  -669.6   <2e-16 ***
    ## T             0.724327   0.001077   672.7   <2e-16 ***
    ## R            -9.060099   0.015049  -602.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13330214  on 9999999  degrees of freedom
    ## Residual deviance:   907581  on 9999997  degrees of freedom
    ## AIC: 907587
    ## 
    ## Number of Fisher Scoring iterations: 11

``` r
df_test$ml_naive = plogis(predict(ml_naive,df_test))
df_test<-df_test[order(df_test$ml_naive,decreasing=TRUE),]

sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$L_p)
```

    ## [1] 18143.06

``` r
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$D_p)
```

    ## [1] 22

``` r
summary(ml_naive)
```

    ## 
    ## Call:
    ## glm(formula = A ~ T + R, family = "binomial", data = df_train, 
    ##     control = list(maxit = 1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.9650  -0.0001   0.0000   0.0001   3.8866  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -21.728561   0.032448  -669.6   <2e-16 ***
    ## T             0.724327   0.001077   672.7   <2e-16 ***
    ## R            -9.060099   0.015049  -602.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13330214  on 9999999  degrees of freedom
    ## Residual deviance:   907581  on 9999997  degrees of freedom
    ## AIC: 907587
    ## 
    ## Number of Fisher Scoring iterations: 11

## Use the outcomes model

``` r
df_train_ = filter(df_train, A == 1 )
ml_outcomes <- lm(L_p ~ T+R, data = df_train_)
summary(ml_outcomes)
```

    ## 
    ## Call:
    ## lm(formula = L_p ~ T + R, data = df_train_)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -48.854  -6.163   0.166   6.198  50.623 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.223e+00  1.293e-02   326.6   <2e-16 ***
    ## T           9.190e-01  1.884e-04  4877.6   <2e-16 ***
    ## R           2.686e+01  1.334e-02  2013.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.394 on 6148890 degrees of freedom
    ## Multiple R-squared:  0.8092, Adjusted R-squared:  0.8092 
    ## F-statistic: 1.304e+07 on 2 and 6148890 DF,  p-value: < 2.2e-16

``` r
df_test$ml_outcomes = predict(ml_outcomes,df_test)
df_test<-df_test[order(df_test$ml_outcomes,decreasing=TRUE),]
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$L_p)
```

    ## [1] 18925.21

``` r
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$D_p)
```

    ## [1] 56

``` r
summary(ml_outcomes)
```

    ## 
    ## Call:
    ## lm(formula = L_p ~ T + R, data = df_train_)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -48.854  -6.163   0.166   6.198  50.623 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.223e+00  1.293e-02   326.6   <2e-16 ***
    ## T           9.190e-01  1.884e-04  4877.6   <2e-16 ***
    ## R           2.686e+01  1.334e-02  2013.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.394 on 6148890 degrees of freedom
    ## Multiple R-squared:  0.8092, Adjusted R-squared:  0.8092 
    ## F-statistic: 1.304e+07 on 2 and 6148890 DF,  p-value: < 2.2e-16

## Calculate the EO-Fair decision

``` r
df_test_black = tibble(df_test)
df_test_black$R=1
df_test_white = tibble(df_test)
df_test_white$R=0
df_test$ml_eo_fair = FRAC_BLACK*plogis(predict(ml_naive,df_test_black)) + (1-FRAC_BLACK)*plogis(predict(ml_naive,df_test_white))
df_test<-df_test[order(df_test$ml_eo_fair,decreasing=TRUE),]


sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$L_p)
```

    ## [1] 18584.5

``` r
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$D_p)
```

    ## [1] 35

## Calculate the AA-Fair decision

``` r
#Setup counterfactual probability distributions
aa_probs_black <- df_train %>%
  count(R, T, T_black) %>%
  group_by(R, T) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()
  
aa_probs_white <- df_train %>%
  count(R, T, T_white) %>%
  group_by(R, T) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()


AA_FAIR_MODEL <- function(applicant_race, applicant_test){
  
# Get sampling space for black and white counterfactuals
aa_probs_black_sample <- aa_probs_black %>% 
filter(R == applicant_race, T==applicant_test)
aa_probs_white_sample <- aa_probs_white %>% 
filter(R == applicant_race, T==applicant_test)

#sample test scores, first for black counterfactual, then for white
sampled_test_scores = aa_probs_black_sample[sample.int(nrow(aa_probs_black_sample), COUNTER_FACTUAL_SAMPLE*FRAC_BLACK, replace = TRUE, prob = aa_probs_black_sample$p),]$T_black
sampled_test_scores = c(sampled_test_scores,aa_probs_white_sample[sample.int(nrow(aa_probs_white_sample), COUNTER_FACTUAL_SAMPLE*(1-FRAC_BLACK), replace = TRUE, prob = aa_probs_white_sample$p),]$T_white)


aa_input_black <- tibble(R=1, T=sampled_test_scores)
aa_input_white <- tibble(R=0, T=sampled_test_scores)

AA_DECISION = mean(FRAC_BLACK*plogis(predict(ml_naive,aa_input_black)) + (1-FRAC_BLACK)*plogis(predict(ml_naive,aa_input_white)))

return(AA_DECISION)
  }
```

``` r
df_test <- df_test %>% 
  rowwise() %>% 
  mutate(ml_aa_fair = AA_FAIR_MODEL(R,T))


df_test<-df_test[order(df_test$ml_aa_fair,decreasing=TRUE),]
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$L_p)
```

    ## [1] 18897.95

``` r
sum(head(df_test,n=POP_SIZE_TEST_ADMIT)$D_p)
```

    ## [1] 50
