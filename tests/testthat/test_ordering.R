df_groups <- data.frame(model = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                   1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                                 .Label = c("Model 1", "Model 2"
                                   ), class = "factor"),
               lci = c(1.49244645034209, -2.27232133279528,
                       -10.195627730385, -3.42331519154288, -7.2582175791452, -7.50889239357948,
                       2.32237535995309, -4.19825291547723, -2.1826007270069, -5.10844894907579,
                       -11.5888131030758, -5.81932593436748, -8.75830801817056, -8.92207357491443,
                       -0.976845721084109, -6.91466204997361),
               median = c(12.0538956417144,
                          7.96345092213661, 5.71698433277685, 7.64736472460526, 4.41051067080669,
                          5.87879854408202, 11.3437990476339, 8.55655044857858, 10.0834729896051,
                          7.74899408061751, 6.51822612965758, 7.66026503225289, 5.39734923894349,
                          6.48592173159492, 9.92117124972224, 8.25325172804674),
               uci = c(32.3238417016679,
                       18.1337990881098, 16.9542547518478, 18.2395336974648, 12.888852854861,
                       15.5066516900956, 24.7161368326162, 23.9786617068908, 31.4414464940353,
                       20.6890927007892, 20.4901160590292, 21.2704782651472, 16.4223258478311,
                       19.253511225471, 26.6745696432128, 26.6504603019531),
               mean = c(13.3986089410617,
                        7.93405862829297, 5.08119204688555, 7.57043882924618, 3.92995775990658,
                        5.32341916055734, 11.9564451324529, 8.90636956584567, 11.3018489760002,
                        7.81969933958096, 6.13516239988522, 7.62217173014161, 4.94176882054541,
                        6.12443598080328, 10.7163239439104, 8.61227234090427),
               sd = c(7.95490057918441,
                      5.18319056129519, 6.80555932623735, 5.33667167496301, 5.20408424240478,
                      5.65313869362644, 5.82452373585739, 6.83595740954779, 8.34144526041772,
                      6.31490353754717, 7.54387142619333, 6.662859081148, 6.43136817110203,
                      7.02312528537963, 6.89097916553347, 8.05516202875161),
               group = c("School A",
                         "School B", "School C", "School D", "School E", "School F", "School G",
                         "School H", "School A", "School B", "School C", "School D", "School E",
                         "School F", "School G", "School H"))



test_that("baggr comparison has the correct ranking of models when order = T", {
  expect_equal(get_order(df_groups, hyper = TRUE),
               c("School A", "School G", "School H",
                 "School B", "School D",
                 "School F", "School C",
                 "School E", "Pooled Estimate"))
  expect_equal(get_order(df_groups, hyper = FALSE),
               c("School A", "School G", "School H",
                 "School B", "School D",
                 "School F", "School C",
                 "School E"))
})


