# log HR and SE for the following studies:
# 1. de la Taille, 2.25, (1.19–4.23)
# 2. Loeb. 1.57 (0.87–2.83)
# 3. DeLancey, 1.45 (1.09-1.92)
# 4. Our, 1.86 (CI: 1.11-3.1) or 1.70 (CI: 1.00-2.9)

# lowerCI = exp(logHR-1.96*(SE))
# SE = (logHR - log(lowerCI))/1.96
# or SE=(log(ul)-log(ll))/(1.96*2)
HR <- c(2.25, 1.57, 1.45, 1.86)
lower_CI95 <- c(1.19, 0.87, 1.09, 1.11)
logHR <- log(HR)
SE <- (logHR - log(lower_CI95))/1.96
logcombinded <- rma.uni(yi=logHR, sei=SE)

# Present results on HR scale
combined_hr <- exp(logcombinded$b)
combined_lb <- exp(logcombinded$ci.lb)
combined_ub <- exp(logcombinded$ci.ub)

HR_CI <- c(combined_hr, combined_lb, combined_ub)
HR_CI <- lapply(HR_CI, function(x){formatC(x, digits = 2, format = "f")})

paste0(HR_CI[1], " (", HR_CI[2], ", ", HR_CI[3], "), ", "<0.0001")
