


kmdec <- function(dec.num, dec.name, datain, adm.cens) {
  
  stopped <- 0
  data.sub <- datain[datain[, dec.name] == dec.num, ]
  
  if (sum(data.sub$out) > 1) {
    
    avsurv <- survfit(
      formula = Surv(tvar, out) ~ 1,
      data = datain[datain[, dec.name] == dec.num, ], error = "g"
    )
    
    avsurv.est = ifelse(
      test = min(avsurv$time) <= adm.cens,
      yes = avsurv$surv[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      no = 1
    )
    
    avsurv.stderr <- ifelse(
      test = min(avsurv$time) <= adm.cens,
      yes = avsurv$std.err[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      no = 0
    )
    
    avsurv.stderr = avsurv.stderr * avsurv.est
    
    avsurv.num = ifelse(
      test = min(avsurv$time) <= adm.cens,
      yes = avsurv$n.risk[avsurv$time == max(avsurv$time[avsurv$time <= adm.cens])],
      no = 0
    )
    
  } else {
    
    return(c(0, 0, 0, 0, stopped = -1))
    
  }
  
  if (sum(data.sub$out) < 5) stopped = 1
  
  c(avsurv.est, avsurv.stderr, avsurv.num, dec.num, stopped)
  
}

GND.calib = function(pred, tvar, out, cens.t,
                     groups, adm.cens, verbose = 0){
  
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
    data = unlist(lapply(groups, kmdec, "dec", datain = datause, adm.cens)),
    ncol = 5,
    byrow = TRUE
  )
  
  if (any(kmtab[, 5] == -1))
    stop(
      "Stopped because at least one of the groups contains <2 events.\
      Consider collapsing some groups.",
      call. = FALSE
    )
  else if (any(kmtab[, 5] == 1)) warning(
    "At least one of the groups contains < 5 events, and this may cause
    high variability in the GND test results (see Demler, Paynter, Cook 
    'Tests of Calibration and Goodness of Fit in the Survival Setting'
    DOI: 10.1002/sim.6428).",
    call. = FALSE
  )
  
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
    test = hltab$kmvar == 0,
    yes = 0,
    no = (hltab$kmperc - hltab$expectedperc) ^ 2 / (hltab$kmvar)
  )
  
  if(verbose > 0) print(hltab[c(1, 2, 3, 4, 10, 5, 6, 9, 7, 11)], digits = 4)
  
  c(
    df = numcat - 1,
    chi2gw = sum(hltab$GND_component),
    pvalgw = 1 - pchisq(sum(hltab$GND_component), numcat - 1)
  )
  
}

GND_test <- function(predicted_risk,
                     predict_horizon,
                     event_time,
                     event_status) {
  
  do_over <- TRUE
  too_few_groups <- FALSE
  group_count <- 10
  
  repeat {
    
    # cut2 imported from Hmisc
    groups = as.numeric(Hmisc::cut2(predicted_risk, g = group_count))
    
    status_before_horizon <- event_status
    status_before_horizon[event_time > predict_horizon] <- 0
    
    group_event_counts <-
      table(groups = groups, event_status = status_before_horizon) %>%
      as_tibble() %>%
      filter(event_status == 1)
    
    if(all(group_event_counts$n > 5)){
      do_over <- FALSE
      break
    }
    
    group_count <- group_count - 1
    
    if(group_count == 1){
      too_few_groups <- TRUE
      do_over <- FALSE
      break
    }
    
  }
  
  if(too_few_groups){
    
    GND_fail <- TRUE
    GND.result <- NULL
    
    
  } else {
    
    GND.result <- try(GND.calib(
      pred = predicted_risk,
      tvar = event_time,
      out = event_status,
      cens.t = predict_horizon,
      groups = groups,
      adm.cens = predict_horizon
    ), silent = TRUE)
    
    GND_fail <- inherits(GND.result, 'try-error')
    
  }
  
  if(GND_fail){
    
    GND.chisq <- NA_real_
    GND.pvalue <- NA_real_
    
  } else {
    
    GND.chisq <- GND.result['chi2gw']
    GND.pvalue <- GND.result['pvalgw']
    
  }
  
  tibble(
    GND_chisq = GND.chisq,
    GND_pvalue = GND.pvalue
  )
  
}

