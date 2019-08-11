# log HR and SE for the following studies:
# 1. de la Taille, 2.25, (1.19–4.23)
# 2. Loeb. 1.57 (0.87–2.83)
# 3. DeLancey, 1.45 (1.09-1.92)
# 4. Our, 1.86 (CI: 1.11-3.1) or 1.70 (CI: 1.00-2.9)

# lowerCI = exp(logHR-1.96*(SE)) so SE = (logHR - log(lowerCI))/1.96
study_labels <- c("de la Taille", "Loeb", "DeLancey", "Our")
HR <- c(2.25, 1.57, 1.45, 1.86)
lower_CI95 <- c(1.19, 0.87, 1.09, 1.11)
logHR <- log(HR)
SE <- (logHR - log(lower_CI95))/1.96
logcombinded <- rma.uni(yi = logHR, sei = SE)

# Present results on HR scale
combined <- predict(logcombinded, transf = exp)
HR_CI <- c(combined$pred, combined$ci.lb, combined$ci.ub)
HR_CI <- lapply(HR_CI, function(x){formatC(x, digits = 2, format = "f")})
paste0(HR_CI[1], " (", HR_CI[2], ", ", HR_CI[3], "), ", "<0.0001")

# Forest plot
png(filename="../output/meta_forest.png")
forest(logcombinded, slab = study_labels, atransf = exp,
       xlab = "Hazard Ratio")
text(-2.05, 6, "First Author",  pos = 4)
text(3.4, 6, "Harazrd Ratio [95% CI]", pos = 2)
dev.off()
