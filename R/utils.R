# Utils ----
# Label Hypothesis choices correctly based on chosen design ----
hypothesis_choices <- list("1" = 1, "2" = 2, "3" = 3,
                           "4" = 1, "5" = 2, "6" = 3,
                           "7" = 1, "8" = 2, "9" = 3)

names(hypothesis_choices) <- c(paste("\U2260", "test value"),
                               "> test value",
                               "< test value",
                               paste("measure 1", "\U2260", "measure 2"),
                               paste("measure 1", ">", "measure 2"),
                               paste("measure 1", "<", "measure 2"),
                               paste("group 1", "\U2260", "group 2"),
                               paste("group 1", ">", "group 2"),
                               paste("group 1", "<", "group 2"))
