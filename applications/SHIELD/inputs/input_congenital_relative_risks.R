# Source: See Syphilis Natural History document

calculate_cs_relative_risks <- function() {
    risk_congenital_syphilis_prenatal_care_first_trimester <- 0.104
    risk_congenital_syphilis_prenatal_care_second_trimester <- 0.176
    risk_congenital_syphilis_prenatal_care_third_trimester <- 0.406
    risk_congenital_syphilis_no_prenatal_care <- 0.36
    
    df <- data.frame(trimester = c(0.5, 1.5, 2.5, 3),
                     risk = c(risk_congenital_syphilis_prenatal_care_first_trimester,
                              risk_congenital_syphilis_prenatal_care_second_trimester,
                              risk_congenital_syphilis_prenatal_care_third_trimester,
                              risk_congenital_syphilis_no_prenatal_care),
                     n = c(416, 1359, 1454, 3240))
    
    fit <- suppressWarnings(glm(risk ~ trimester,
                                data = df,
                                family = binomial(link = "logit"),
                                weights = df$n))
    log_odds <- fit$coefficients["(Intercept)"] + fit$coefficients["trimester"]*df$trimester
    df$smooth_risk <- 1 / (1 + exp(-log_odds))
    df$relative_risk <- df$smooth_risk / df[4, "smooth_risk"]
    
    if (1==2) {
        browser()
        summary(fit)
        ggplot(data=df, mapping=aes(x = trimester)) +
            ylim(0,1) +
            geom_line(aes(y = risk), color = "blue") +
            geom_line(aes(y = smooth_risk), color = "red")
    }
    
    c(first = df[1, "relative_risk"],
      second = df[2, "relative_risk"],
      third = df[3, "relative_risk"],
      none = df[4, "relative_risk"])
}

cs_relative_risks <- calculate_cs_relative_risks()
