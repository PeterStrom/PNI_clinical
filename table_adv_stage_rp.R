DTA$adv_stage_rp <- DTA$pat_stage != 'T2'

# HR only biopsy markers
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "`Gleason score`")
adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("adv_stage_rp ~ ", adjust))
model <- glm(formula, family=binomial(link='logit'), data=DTA)

result_rp <- exp(cbind(OR = coef(model), confint.default(model)))
result_rp <- trimws(format(round(result_rp, 2), nsmall=2))
result_rp <- apply(result_rp, 1, function(x) paste0(x[1], ' (', x[2], '-', x[3], ')'))
result_rp <- result_rp[-1]
names = attr(result_rp, "names")
for (i in list(7, 6, 5, 2, 1, 0)){
  result_rp <- append(result_rp, "", after=i)
  names <- append(names, "", after=i)
}
for (i in list(13, 9, 5, 1)){
  result_rp <- append(result_rp, "1.0", after=i)
  names <- append(names, "", after=i)
}
  
tab_rp <- data.frame('names'=names, 'cases'='', 'control'='', 'estimate'=result_rp, stringsAsFactors = FALSE)

fun_cc <- function(x){
  a <- table(x, DTA$adv_stage_rp)
  b <- a
  b[, 1] <- a[, 2]
  b[, 2] <- a[, 1]
  return(b)
}

marginal_adv_stage_rp <- rev(table(DTA$adv_stage_rp))

tab_rp[2:3, 2:3] <- fun_cc(DTA$`Perineural invasion`)
tab_rp[5, 2:3] <- marginal_adv_stage_rp
tab_rp[7:10, 2:3] <- fun_cc(DTA$PSA)
tab_rp[12:13, 2:3] <- fun_cc(DTA$`Digital rectal examination`)
tab_rp[15, 2:3] <- marginal_adv_stage_rp
tab_rp[17:20, 2:3] <- fun_cc(DTA$`Gleason score`)

# write to file
write.xlsx(tab_rp, "../output/table_adv_stage_rp.xlsx", row.names = FALSE)
