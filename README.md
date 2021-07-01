
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survival.calib

<!-- badges: start -->

[![codecov](https://codecov.io/gh/bcjaeger/survival.calib/branch/master/graph/badge.svg?token=V522BP9QGS)](https://codecov.io/gh/bcjaeger/survival.calib)
[![R-CMD-check](https://github.com/bcjaeger/survival.calib/workflows/R-CMD-check/badge.svg)](https://github.com/bcjaeger/survival.calib/actions)
<!-- badges: end -->

The goal of `survival.calib` is to provide convenient access to tests
for mis-calibration in the survival setting. It provides functions to
help users apply these tests in a highly customized way (see
`gnd_test_manual`) or following a protocol that the developers recommend
(see `gnd_test_auto`).

## Installation

<!-- You can install the released version of survival.calib from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("survival.calib") -->

<!-- ``` -->

You can install `survival.calib` from GitHub with:

``` r
remotes::install_github("bcjaeger/survival.calib")
```

## Example

Let’s apply the Namwood-Green-D’Agostino (GND) test to assess
calibration of a proportional hazards model. First, we load packages and
curate some data for modeling.

``` r

library(survival.calib)
library(survival)
library(riskRegression)
#> riskRegression version 2020.12.08

model_vars <- c('futime', 'death', 'age', 'sex', 'flc.grp', 'lambda')
model_data <- survival::flchain[, model_vars]

head(model_data)
#>   futime death age sex flc.grp lambda
#> 1     85     1  97   F      10  4.860
#> 2   1281     1  92   F       1  0.683
#> 3     69     1  94   F      10  3.850
#> 4    115     1  92   F       9  2.220
#> 5   1039     1  93   F       6  1.690
#> 6   1355     1  90   F       9  1.860
```

Next, we

1.  designate a set of rows that will define the training set,
2.  fit a model to the training set,
3.  predict risk for mortality in a testing set.

<!-- end list -->

``` r

# step 1
set.seed(730)
train_index <- sample(x = nrow(model_data), size = 2000)

# step 2
risk_mdl <- coxph(
 formula = Surv(futime, death) ~ age + sex + flc.grp + lambda,
 data = flchain[train_index,],
 x = TRUE
)

# step 3
time_predict <- 4000
risk_pred <- predictRisk(risk_mdl,
                         newdata = flchain[-train_index, ],
                         times = time_predict)
```

Once we have predicted risk values, we can go in two directions:

1.  Create risk groups by hand and pass the risk group assignments into
    `gnd_test_manual`.
2.  Use `gnd_test_auto`, which creates risk groups for you.

We’ll show both approaches.

### GND test (manual approach)

The manual GND test requires a `group` input value that designates which
group each observation in the testing data belongs to. Creation of the
`group` input is up to the user, although there are functions that help,
e.g., `cut_percentiles` will create quantile groups for you.

``` r

# Create risk groups:
risk_groups <- cut_percentiles(risk_pred, g = 10)

# supply predicted risk, observed outcomes, 
# time of prediction, and group assignments
# to the manual GND test function.
gnd_result_manual <- 
 gnd_test_manual(predicted_risk = risk_pred,
                 event_time = flchain$futime[-train_index],
                 event_status = flchain$death[-train_index],
                 time_predict = time_predict,
                 group = risk_groups,
                 verbose = 0)

# peak at the results
gnd_result_manual
#> -------------------------------------------------------
#> 
#> - Greenwood-Nam-D'Agostino (GND) Mis-calibration Test
#> 
#> -- Chi-square test statistic: 13.473
#> -- degrees of freedom: 9
#> -- P-value for lack of fit: .14
#> -- Warnings: None
#> 
#> -----------------------------------------------------
#>  
#> - Events data by group (truncated):
#> 
#>  group_label group_n events_observed events_expected
#>            1     588              20            24.7
#>            2     587              27            34.6
#>            3     587              45            45.0
#>            4     588              51            59.4
#>            5     587              69            80.3
#> 
#> + 5 more rows
```

### GND test (automatic approach)

Demler et al. show that the GND test has high variability if there are
less than 5 events in any of the risk groups. When this occurs, the
authors recommend lumping groups with \< 5 events into the nearest risk
group. The `gnd_test_auto` automates this approach, allowing users to
forego manual creation of risk groups. For example, below we apply
`gnd_test_auto` with `group_count_init = 45`, which initiates the
following algorithm:

1.  Create 45 groups based on quantiles of predicted risk.
2.  Check the event counts in each group
3.  If any groups have \< 5 events, collapse the group with fewest
    events into its neighbor, and repeat step 2
4.  run `gnd_test_manual` with the (potentially modified) groups.

<!-- end list -->

``` r

gnd_result_auto <- 
 gnd_test_auto(predicted_risk = risk_pred,
               event_time = flchain$futime[-train_index],
               event_status = flchain$death[-train_index],
               time_predict = time_predict,
               group_count_init = 45,
               group_method = 'lump',
               verbose = 1)
#> Checking event counts using 45 risk groups...too few; trying again
#> Checking event counts using 44 risk groups...too few; trying again
#> Checking event counts using 43 risk groups...okay
#> Attempting GND test with 43 risk groups...Done

gnd_result_auto
#> -------------------------------------------------------
#> 
#> - Greenwood-Nam-D'Agostino (GND) Mis-calibration Test
#> 
#> -- Chi-square test statistic: 48.91
#> -- degrees of freedom: 42
#> -- P-value for lack of fit: .22
#> -- Warnings: None
#> 
#> -----------------------------------------------------
#>  
#> - Events data by group (truncated):
#> 
#>  group_label group_n events_observed events_expected
#>            1     131               7            4.39
#>            3     260               6           10.83
#>            4     131               6            6.20
#>            6     262               8           13.77
#>            7     130               6            7.55
#> 
#> + 38 more rows
```

You can see from the printed output that the p-value with 10 groups is
different from the p-value with 43 groups. Simulation studies have
focused on using 10 groups and this approach has shown good statistical
properties. So, don’t use 45 groups (the default value for
`group_count_init` is 10).

# References

Demler, O.V., Paynter, N.P. and Cook, N.R., 2015. Tests of calibration
and goodness‐of‐fit in the survival setting. *Statistics in medicine*,
34(10), pp.1659-1680. DOI: 10.1002/sim.6428
