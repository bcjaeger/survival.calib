# printed output has not changed

    Code
      gnd_test_auto(predicted_risk = risk_pred, event_time = flchain$futime[
        -train_index], event_status = flchain$death[-train_index], time_predict = risk_times,
      group_count_init = 50, verbose = 0)
    Output
      -----------------------------------------------------
      
      - Greenwood-Nam-D'Agostino (GND) Goodness-of-Fit Test
      
      -- Chi-square test statistic: 29.434
      -- degrees of freedom: 39
      -- P-value for lack of fit: .87
      -- Warnings: None
      
      -----------------------------------------------------
       
      - Events data by group (truncated):
      
       group_label group_n events_observed events_expected
                 1      98               6            2.29
                 4     390               9           11.88
                 6     195              10            7.06
                 8     292              12           11.76
                12     195               8            8.66
      
      + 35 more rows

