
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survival.calib

<!-- badges: start -->

[![codecov](https://codecov.io/gh/bcjaeger/survival.calib/branch/master/graph/badge.svg?token=V522BP9QGS)](https://codecov.io/gh/bcjaeger/survival.calib)
[![R-CMD-check](https://github.com/bcjaeger/survival.calib/workflows/R-CMD-check/badge.svg)](https://github.com/bcjaeger/survival.calib/actions)
[![DOI](https://zenodo.org/badge/379621580.svg)](https://zenodo.org/badge/latestdoi/379621580)
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
#> riskRegression version 2021.10.10

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
pred_horizon <- 4000
pred_risk <- predictRisk(risk_mdl,
                         newdata = flchain[-train_index, ],
                         times = pred_horizon)
```

Once we have predicted risk values, we can create a `scalib` (survival
calibration) object:

``` r
sc <- scalib(pred_risk = list(cph = pred_risk),
             pred_horizon = pred_horizon,
             event_time = flchain$futime[-train_index],
             event_status = flchain$death[-train_index])

print(sc)
#> 
#> Survival calibration object with prediction horizon of 4000
#> 
#> -- Input data ----------------------
#> 
#>       event_time event_status    cph
#>            <int>        <num>  <num>
#>    1:       1281            1 0.8750
#>    2:         69            1 0.9990
#>    3:        115            1 0.9815
#>    4:       1355            1 0.9528
#>    5:       2851            1 0.8281
#>   ---                               
#> 5870:       4547            0 0.0455
#> 5871:       4788            0 0.0397
#> 5872:       3652            0 0.0350
#> 5873:       2507            0 0.0716
#> 5874:       3995            0 0.0370
```

Now we can go in two directions:

1.  Create risk groups by hand and pass the risk group assignments into
    `scalib_gnd_manual`.
2.  Use `scalib_gnd`, which creates risk groups for you.

We’ll show both approaches.

### GND test (manual approach)

The manual GND test requires a `group` input value that designates which
group each observation in the testing data belongs to. Creation of the
`group` input is up to the user.

``` r
# Create risk groups:
pctls <- quantile(pred_risk, probs = c(0:10) / 10)
risk_groups <- cut(pred_risk, breaks = pctls, include.lowest = TRUE)
risk_groups <- as.numeric(risk_groups)

# supply predicted risk, observed outcomes, 
# time of prediction, and group assignments
# to the manual GND test function.

gnd_result_manual <- scalib_gnd_manual(scalib_object = sc, 
                                       pred_risk_col = 'cph',
                                       group = risk_groups)

# print the updated scalib object
gnd_result_manual
#> 
#> Survival calibration object with prediction horizon of 4000
#> 
#> -- Input data ----------------------
#> 
#>       event_time event_status    cph
#>            <int>        <num>  <num>
#>    1:       1281            1 0.8750
#>    2:         69            1 0.9990
#>    3:        115            1 0.9815
#>    4:       1355            1 0.9528
#>    5:       2851            1 0.8281
#>   ---                               
#> 5870:       4547            0 0.0455
#> 5871:       4788            0 0.0397
#> 5872:       3652            0 0.0350
#> 5873:       2507            0 0.0716
#> 5874:       3995            0 0.0370
#> 
#> 
#> -- Output data ----------------------------------------------------------
#> 
#> Key: <._id_.>
#>    ._id_. gnd_df gnd_chisq gnd_pvalue           gnd_data gnd_group_method
#>    <char>  <num>     <num>      <num>             <list>           <char>
#> 1:    cph      9      13.5      0.142 <data.table[10x8]>           custom
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

``` r

gnd_result_auto <- scalib_gnd(scalib_object = sc,
                              group_method = 'lump', 
                              group_count_init = 45,
                              verbose = 1)
#> Checking event counts using 45 risk groups...too few; trying again
#> Checking event counts using 44 risk groups...too few; trying again
#> Checking event counts using 43 risk groups...okay

# print the updated scalib object
gnd_result_auto
#> 
#> Survival calibration object with prediction horizon of 4000
#> 
#> -- Input data ----------------------
#> 
#>       event_time event_status    cph
#>            <int>        <num>  <num>
#>    1:       1281            1 0.8750
#>    2:         69            1 0.9990
#>    3:        115            1 0.9815
#>    4:       1355            1 0.9528
#>    5:       2851            1 0.8281
#>   ---                               
#> 5870:       4547            0 0.0455
#> 5871:       4788            0 0.0397
#> 5872:       3652            0 0.0350
#> 5873:       2507            0 0.0716
#> 5874:       3995            0 0.0370
#> 
#> 
#> -- Output data ----------------------------------------------------------
#> 
#> Key: <._id_.>
#>    ._id_. gnd_df gnd_chisq gnd_pvalue           gnd_data gnd_group_method
#>    <fctr>  <num>     <num>      <num>             <list>           <char>
#> 1:    cph     42        51      0.162 <data.table[43x8]>             lump
```

You can see from the printed output that the p-value with 10 groups is
different from the p-value with 43 groups. Simulation studies have
focused on using 10 groups and this approach has shown good statistical
properties. So, use 10 groups (the default value for
`group_count_init`).

# References

Demler, O.V., Paynter, N.P. and Cook, N.R., 2015. Tests of calibration
and goodness‐of‐fit in the survival setting. *Statistics in medicine*,
34(10), pp.1659-1680. DOI: 10.1002/sim.6428
