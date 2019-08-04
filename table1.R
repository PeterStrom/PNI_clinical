###########################################################################
## Table 1. Descriptions of cohort.
###########################################################################
a <- table(DTA$`Perineural invasion`)
n_unexp <- a[[1]]
n_exp <- a[[2]]

table1fun <- function(col) {
  a <- table(col)
  b <- prop.table(a)*100
  df <- data.frame(var = levels(col),
                   n = paste0(a, " (", round(b), "%)"), stringsAsFactors = FALSE)
  return(rbind(c("", ""), df))
}

make_table_1 <- function(x){
  varList <- list(x$Age,
                  x$PSA,
                  x$`Digital rectal examination`,
                  x$Cancerlength_cat,
                  x$`Gleason score`,
                  x$PNI,
                  x$`Pathological stage`,
                  x$`Surgical margin`)
  l_t1 <- lapply(varList, function(y) table1fun(col = y))
  t1 <- as.data.frame(bind_rows(l_t1))
  return(t1)
}

## Exposed
tmp <- DTA[DTA$`Perineural invasion` == "Positive", ]
t1 <- make_table_1(x=tmp)
t1 <- rbind(t1, c("", ""))
t1 <- rbind(t1, c("Events", sum(tmp$event)))
t1 <- rbind(t1, c("Person Years", as.numeric(round(sum(tmp$time)))))

## Unexposed
tmp <- DTA[DTA$`Perineural invasion` == "Negative", ]
t2 <- make_table_1(x=tmp)
t2 <- rbind(t2, c("", ""))
t2 <- rbind(t2, c("Events", sum(tmp$event)))
t2 <- rbind(t2, c("Person Years", as.numeric(round(sum(tmp$time)))))

# Combine
tab1 <- data.frame(Exposed = t1, Unexposed = t2[[2]])

# write to file
write.xlsx(tab1, "../output/table1.xlsx", row.names = FALSE)
