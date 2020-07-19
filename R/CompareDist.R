tab_ngo = function(datay, datax, y, x) {
  ind = which(datax[,x]!=0)
  datax_n = datax[ind,]
  datay_n = datay[ind,]
  tab = table(unlist(datax_n[,x]),
              unlist(datay_n[,y]))
  rownames(tab) = paste(colnames(datax)[x],
                        rownames(tab),
                        sep = "_")
  tab_new = apply(tab, 2, function (i) {
    sapply(i, function (j) {
      paste(j, " (", round(j / sum(i) * 100, digits = 2), "%)", sep = "")
    })
  })
  return(tab_new)
}

serial_tab = function(datay, datax, y, spec_x) {
  for (i in 1:length(spec_x)) {
    assign(paste("tab", colnames(datax)[spec_x[i]],sep = ""),
           tab_ngo(datay, datax, y, spec_x[i]))
  }
  tab = eval(parse(text = paste("tab", colnames(datax)[spec_x[1]],sep = "")))
  for (i in 2:length(spec_x)) {
    tab2 = eval(parse(text = paste("tab", colnames(datax)[spec_x[i]],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(tab)
}

tab_norm = function(datay, datax, y, x) {
  ind = which(datax[,x]!=0)
  datax_n = datax[ind,]
  datay_n = datay[ind,]
  tab = table(unlist(datax_n[,x]),
              unlist(datay_n[,y]))
  rownames(tab) = paste(colnames(datax[,x]),
                        rownames(tab),
                        sep = "_")
  return(tab)
}

serial_fisher = function(datay, datax, y, spec_x) {
  mat = matrix(nrow = length(spec_x))
  for (i in 1:length(spec_x)) {
    ifelse(dim(tab_norm(datay, datax, y, spec_x[i]))[1]>=2,
           mat[i,] <- round(fisher.test(tab_norm(datay, datax, y, spec_x[i]))$p.value, digits = 3),
           mat[i,] <- NA)
  }
  rownames(mat) = colnames(datax[,spec_x])
  colnames(mat) = paste("p-value grouped by",
                        colnames(datay[,y]),
                        sep = " ")
  return(mat)
}


serial_chisq = function(datay, datax, y, spec_x) {
  mat = matrix(nrow = length(spec_x),
               ncol = 2)
  for (i in 1:length(spec_x)) {
    ifelse((dim(tab_norm(datay, datax, y, spec_x[i]))[1]>=2),
           {
             mat[i,2] <- round(chisq.test(tab_norm(datay, datax, y, spec_x[i]))$p.value, digits = 3)
             mat[i,1] <- round(chisq.test(tab_norm(datay, datax, y, spec_x[i]))$statistic, digits = 3)
           },
           mat[i,] <- NA)
  }
  rownames(mat) = colnames(datax[,spec_x])
  colnames(mat) = c("chi-squared",
                    paste("p-value grouped by",
                          colnames(datay[,y]),
                          sep = " "))
  return(mat)
}

tab_ngo_0 = function(datay, datax, y, x) {
  datax_n = datax
  datay_n = datay
  tab = table(unlist(datax_n[,x]),
              unlist(datay_n[,y]))
  rownames(tab) = paste(colnames(datax)[x],
                        rownames(tab),
                        sep = "_")
  tab_new = apply(tab, 2, function (i) {
    sapply(i, function (j) {
      paste(j, " (", round(j / sum(i) * 100, digits = 2), "%)", sep = "")
    })
  })
  return(tab_new)
}

tab_norm_0 = function(datay, datax, y, x) {
  datax_n = datax
  datay_n = datay
  tab = table(unlist(datax_n[,x]),
              unlist(datay_n[,y]))
  rownames(tab) = paste(colnames(datax[,x]),
                        rownames(tab),
                        sep = "_")
  return(tab)
}

serial_tab_0 = function(datay, datax, y, spec_x) {
  for (i in 1:length(spec_x)) {
    assign(paste("tab", colnames(datax)[spec_x[i]],sep = ""),
           tab_ngo_0(datay, datax, y, spec_x[i]))
  }
  tab = eval(parse(text = paste("tab", colnames(datax)[spec_x[1]],sep = "")))
  for (i in 2:length(spec_x)) {
    tab2 = eval(parse(text = paste("tab", colnames(datax)[spec_x[i]],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(tab)
}

serial_chisq_0 = function(datay, datax, y, spec_x) {
  mat = matrix(nrow = length(spec_x))
  for (i in 1:length(spec_x)) {
    ifelse(dim(tab_norm_0(datay, datax, y, spec_x[i]))[1]>=2,
           mat[i,] <- round(chisq.test(tab_norm(datay, datax, y, spec_x[i]))$p.value, digits = 3),
           mat[i,] <- NA)
  }
  rownames(mat) = colnames(datax[,spec_x])
  colnames(mat) = paste("p-value grouped by",
                        colnames(datay[,y]),
                        sep = " ")
  return(mat)
}

t_test_ngo = function (datay, datax, y, x) {
  ind = which(datay[,y] == 0)
  mat = matrix(nrow = 1,
               ncol = 8)
  datax = unlist(datax[,x])
  x1 = datax[ind]
  x2 = datax[-ind]
  ifelse((length(unique(x1))>2 | length(unique(x2))>2),
         {
           tt = t.test(x1, x2)
           mat[1,1] <- round(tt$estimate[1], digits = 2)
           mat[1,2] <- round(sd(x1), digits = 2)
           mat[1,3] <- round(tt$estimate[2], digits = 2)
           mat[1,4] <- round(sd(x2), digits = 2)
           mat[1,5] <- round(tt$statistic, digits = 2)
           mat[1,6:7] <- round(tt$conf.int, digits = 2)
           mat[1,8] <- round(tt$p.value, digits = 3)
         },
         mat[1,1:8] <- c(NA,NA,NA,NA,NA,NA,NA,NA))
  return(mat)
}

serial_t = function(datay, datax, y, spec_x) {
  mat = matrix(nrow = length(spec_x),
               ncol = 8)
  for (i in 1:length(spec_x)) {
    mat[i,] = t_test_ngo(datay, datax, y, spec_x[i])
  }
  colnames(mat) = c("mean0", "sd0", "mean1", "sd1",
                    "t-value", "LCI", "UCI", "p-value")
  rownames(mat) = colnames(datax)[spec_x]
  return(mat)
}

t.test02 <- function(n1, m1, sd1, n2, m2, sd2) {
  se <- sqrt((sd1^2/n1) + (sd2^2/n2))
  df <- ((sd1^2/n1 + sd2^2/n2)^2) / ((sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1))
  t <- (m1-m2)/se
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  names(dat) = c("mean_difference", "SE", "t", "p-value")
  return(dat)
}
