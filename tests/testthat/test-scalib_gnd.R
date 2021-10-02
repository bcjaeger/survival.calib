

suppressPackageStartupMessages(expr = {
  library(survival)
  library(riskRegression)
  library(data.table)
})

# Original GND functions: ----
kmdec_orig = function(dec.num, dec.name, datain, adm.cens) {

  stopped = 0

  data.sub = datain[datain[, dec.name] == dec.num,]

  if (sum(data.sub$out) > 1) {

    avsurv = survfit(
      Surv(tvar, out) ~ 1,
      data = datain[datain[, dec.name] == dec.num,], error = "g"
    )

    avsurv.est = ifelse(
      min(avsurv$time) <= adm.cens,
      avsurv$surv[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      1
    )

    avsurv.stderr = ifelse(
      min(avsurv$time) <= adm.cens,
      avsurv$std.err[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      0
    )

    avsurv.stderr = avsurv.stderr * avsurv.est

    avsurv.num = ifelse(
      min(avsurv$time) <= adm.cens,
      avsurv$n.risk[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      0
    )

  } else {

    return(c(0, 0, 0, 0, stopped = -1))

  }

  if (sum(data.sub$out) < 5)
    stopped = 1

  c(avsurv.est, avsurv.stderr, avsurv.num, dec.num, stopped)

} #kmdec

GND.calib_orig = function(pred, tvar, out, cens.t, groups, adm.cens) {

  tvar.t = ifelse(tvar > adm.cens, adm.cens, tvar)
  out.t = ifelse(tvar > adm.cens, 0, out)

  datause = data.frame(
    pred = pred,
    tvar = tvar.t,
    out = out.t,
    count = 1,
    cens.t = cens.t,
    dec = groups
  )

  numcat = length(unique(datause$dec))
  groups = sort(unique(datause$dec))

  kmtab = matrix(
    unlist(lapply(
      groups, kmdec_orig, "dec", datain = datause, adm.cens
    )),
    ncol = 5,
    byrow = TRUE
  )

  if (any(kmtab[, 5] == -1))
    stop("Stopped because at least one of the groups contains <2 events.")

  else if (any(kmtab[, 5] == 1))
    warning("At least one of the groups contains < 5 events.")

  hltab = data.frame(
    group = kmtab[, 4],
    totaln = tapply(datause$count, datause$dec, sum),
    censn = tapply(datause$cens.t, datause$dec, sum),
    numevents = tapply(datause$out, datause$dec, sum),
    expected = tapply(datause$pred, datause$dec, sum),
    kmperc = 1 - kmtab[, 1],
    kmvar = kmtab[, 2] ^ 2,
    kmnrisk = kmtab[, 3],
    expectedperc = tapply(datause$pred, datause$dec, mean)
  )

  hltab$kmnum = hltab$kmperc * hltab$totaln
  hltab$GND_component = ifelse(
    hltab$kmvar == 0,
    0,
    (hltab$kmperc - hltab$expectedperc) ^ 2 / (hltab$kmvar))

  #print(hltab[c(1, 2, 3, 4, 10, 5, 6, 9, 7, 11)], digits = 4)

  c(
    df = numcat - 1,
    chi2gw = sum(hltab$GND_component),
    pvalgw = 1 - pchisq(sum(hltab$GND_component), numcat - 1)
  )

} # GND.calib


# Set up for tests -----------------------------------------------------------

# drop rows with missing values for simplicity
data_init <- na.omit(flchain)

data_init$chapter <- NULL

n_obs_total <- nrow(data_init)
n_obs_train <- round(n_obs_total * 2 / 3)

set.seed(32987)

train_index <- sample(n_obs_total, size = n_obs_train)

data_train <- data_init[train_index,]
data_test <- data_init[-train_index,]

model_1 <- coxph(Surv(futime, death) ~ .,
                 data = data_train,
                 x = TRUE)

model_2 <- update(model_1, . ~ . - flc.grp)

model_3 <- update(model_2, . ~ . - mgus)

pred_horizon <- 3000

pred_risk <- lapply(
  X = list(model_1, model_2, model_3),
  FUN = predictRisk,
  newdata = data_test,
  times = pred_horizon
)

pred_risk <- lapply(pred_risk, as.numeric)

#split into deciles
g <- 10

risk_groups <-
  lapply(pred_risk,
         function(x)
           ceiling((g) * frank(x) / (length(x)+1)))

# TEST REPRODUCIBILITY: do my results match Olga D's original code? ----------

orig <- vector(mode = 'list', length = length(pred_risk))

for (i in seq_along(pred_risk)) {

  orig[[i]] <- GND.calib_orig(
    pred = pred_risk[[i]],
    tvar = data_test$futime,
    out = data_test$death,
    groups = risk_groups[[i]],
    adm.cens = pred_horizon,
    cens.t = pred_horizon
  )

}

bcj <- scalib(
  pred_risk = pred_risk,
  pred_horizon = pred_horizon,
  event_status = data_test$death,
  event_time = data_test$futime
)

bcj <- scalib_gnd(bcj)

test_that(
  desc = "results match Olga Demler's original code",
  code = {
    for (i in seq_along(risk_times)) {
      expect_equal(as.numeric(bcj$data_outputs$gnd_chisq[i]),
                   as.numeric(orig[[i]]['chi2gw']),
                   ignore_attr = TRUE)
      expect_equal(as.numeric(bcj$data_outputs$gnd_pvalue[i]),
                   as.numeric(orig[[i]]['pvalgw']),
                   ignore_attr = TRUE)
    }
  })

# TEST ERROR CATCHING: invalid columns in inputs -----------------------------

.scalib <- scalib(pred_risk = risk_pred[, 1],
                  pred_horizon = risk_times[1],
                  event_status = data_test$death,
                  event_time = data_test$futime)

.scalib$data_inputs$group <- risk_groups[, 1]

test_that(
  desc = 'invalid column inputs trigger errors',
  code = {
    expect_error(
      scalib_gnd_manual(.scalib, pred_risk_col = 'not_right'),
      regexp = 'not_right is not a column'
    )
    expect_error(
      scalib_gnd_manual(.scalib,
                             pred_risk_col = 'pred_risk',
                             group_col = 'bad_group'),
      regexp = 'bad_group is not a column'
    )
  }
)

# TEST ERROR/WARNING CATCHING: not enough events -----------------------------

.scalib_bad <- .scalib

.scalib_bad$data_inputs[group == 1, event_status := 0]

test_that(
  desc = 'if a group has < group_min_events_stop, error is triggered',
  code = {
    expect_error(
      scalib_gnd_manual(.scalib_bad,
                             pred_risk_col = 'pred_risk',
                             group_min_events_stop = 3),
      regexp = 'at least 1 group contains < 3 events'
    )
  }
)

g1_status <- .scalib_bad$data_inputs[group == 1][['event_status']]
g1_status[1:5] <- 1

.scalib_bad$data_inputs[group == 1, event_status := g1_status]

test_that(
  desc = 'if a group has < group_min_events_warn, warning is triggered',
  code = {
    expect_warning(
      scalib_gnd_manual(.scalib_bad,
                             pred_risk_col = 'pred_risk',
                             group_min_events_warn = 6),
      regexp = 'at least 1 group contains < 6 events'
    )
  }
)



# TEST SCALIB_GND: binding outputs properly

.scalib <- scalib(
  pred_risk = lapply(list(model_1, model_2, model_3),
                     predictRisk,
                     newdata = data_test,
                     times = pred_horizon),
  pred_horizon = pred_horizon,
  event_status = data_test$death,
  event_time = data_test$futime
)

.scalib <- scalib_gnd(.scalib)

# TEST ERROR/WARNING CATCHING: auto (unfinished) -----------------------------

# test_that(
#   desc = "event count error is triggered",
#   code = {
#     expect_error(
#       calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         time_predict = risk_times,
#         group_count_init = 50,
#         group_count_min = 49,
#         verbose = 2
#       ),
#       regexp = 'too few events'
#     )
#   }
# )
#
#
# # check printed output
# test_that(
#   desc = 'printed output has not changed',
#   code = {
#     expect_snapshot(
#       calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         time_predict = risk_times,
#         group_count_init = 50,
#         verbose = 0
#       ),
#       cran = FALSE,
#       error = FALSE
#     )
#   }
# )
#
# # check on error messages
# # group_count_init (lwr bound)
# test_that(
#   desc = 'group_count_init min is caught',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         time_predict = risk_times,
#         group_count_init = 1,
#         verbose = 0
#       ),
#       regexp = 'group_count_init = 1 should be'
#     )
#   }
# )
#
# test_that(
#   desc = 'group_method incorrect entry is caught',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         group_method = 'clump',
#         time_predict = risk_times,
#         group_count_init = 1,
#         verbose = 0
#       ),
#       regexp = 'group_method should be'
#     )
#   }
# )
#
# # time_predict (length)
# test_that(
#   desc = 'time_predict length is caught',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         time_predict = c(1, 2),
#         group_count_init = 5,
#         verbose = 0
#       ),
#       regexp = 'time_predict should have length <1>'
#     )
#   }
# )
#
# # time_predict (type)
# test_that(
#   desc = 'time_predict length is caught',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = flchain$death[-train_index],
#         time_predict = "4000",
#         group_count_init = 5,
#         verbose = 0
#       ),
#       regexp = 'should have type <double or integer>'
#     )
#   }
# )
#
# # event_status is caught (part 1)
# test_that(
#   desc = 'status values are corrected, part 1',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = sample(c(0, 1, 2),
#                               size = length(risk_pred),
#                               replace = TRUE),
#         time_predict = risk_times,
#         group_count_init = 5,
#         verbose = 0
#       ),
#       regexp = 'event_status should contain values of'
#     )
#   }
# )
#
# # event_status is caught (part 2)
# test_that(
#   desc = 'less than 2 events hard stop',
#   code = {
#     expect_error(
#       object = calib_test_gnd(
#         predicted_risk = risk_pred,
#         event_time = flchain$futime[-train_index],
#         event_status = rep(0, length(risk_pred)),
#         time_predict = risk_times,
#         group_count_init = 5,
#         verbose = 0
#       ),
#       regexp = 'At least 1 group contains <'
#     )
#   }
# )


# BENCHMARKING COMPUTATION TIMES (leave this commented out) ------------------

# # drop rows with missing values for simplicity
# data_init <- na.omit(flchain)
#
# data_init$chapter <- NULL
#
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
# data_init <- rbind(data_init, data_init)
#
# n_obs_total <- nrow(data_init)
# n_obs_train <- round(n_obs_total * 2 / 3)
#
# set.seed(32987)
#
# train_index <- sample(n_obs_total, size = n_obs_train)
#
# data_train <- data_init[train_index,]
# data_test <- data_init[-train_index,]
#
# model <- coxph(Surv(futime, death) ~ .,
#                data = data_train,
#                x = TRUE)
#
# pred_horizon <- 1000
#
# risk_pred <- as.numeric(predictRisk(model,
#                                     newdata = data_test,
#                                     times = pred_horizon))
#
# risk_group <- predrisk_grp_prcnt(risk_pred, g = 10)
#
# .scalib <-
#   scalib(risk_pred, pred_horizon, data_test$death, data_test$futime)
#
# # # benchmark of computing times
# microbenchmark::microbenchmark(
#   orig = GND.calib_orig(
#     pred = risk_pred,
#     tvar = data_test$futime,
#     out = data_test$death,
#     groups = risk_group,
#     adm.cens = risk_times[i],
#     cens.t = pred_horizon
#   ),
#   bcj = scalib_gnd_manual(.scalib,
#                           pred_risk_col = 'pred_risk',
#                           group = risk_group,
#                           verbose = 0),
#   times = 5
# )
