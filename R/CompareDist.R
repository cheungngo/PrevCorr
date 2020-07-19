#' Creating a table showing the distributions grouped by 2 variables
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable, e.g. sex
#' @param y : column number with respect to datay
#' @param x : column number with respect to datax
#' @return A table grouped by the 2 variables with percentages
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

#' Creating a series of tables showing the distributions
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variable
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A series of tables with percentages
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

#' Simple table without percentage
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable, e.g. sex
#' @param y : column number with respect to datay
#' @param x : column number with respect to datax
#' @return A table grouped by the 2 variables
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

#' Serial fisher tests
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A matrix with all the resulted p-values
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

#' Serial chi-square tests
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A matrix with all the resulted p-values and X-squared statistics
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

#' Creating a table showing the distributions grouped by 2 variables (Including zeroes in the second variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable, e.g. sex
#' @param y : column number with respect to datay
#' @param x : column number with respect to datax
#' @return A table grouped by the 2 variables with percentages
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

#' Simple table without percentage (Including zeroes in the second variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable, e.g. sex
#' @param y : column number with respect to datay
#' @param x : column number with respect to datax
#' @return A table grouped by the 2 variables
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

#' Creating a series of tables showing the distributions (Including zeroes in the second variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variable
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A series of tables with percentages
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

#' Serial chi-square tests (Including zeroes in the second variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A matrix with all the resulted p-values
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

#' Simple t test
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A matrix with the rounded t-test results
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

#' Serial t tests
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param y : column number with respect to datay
#' @param x : column numbers with respect to datax
#' @return A matrix with all the rounded t-test results
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

#' Simple t test by statistics
#'
#' @param n1 : number of subjects in group 1
#' @param m1 : mean of group 1
#' @param sd1 : sd of group 1
#' @param n2 : number of subjects in group 2
#' @param m2 : mean of group 2
#' @param sd2 : sd of group 2
#' @return results of t test
t.test02 <- function(n1, m1, sd1, n2, m2, sd2) {
  se <- sqrt((sd1^2/n1) + (sd2^2/n2))
  df <- ((sd1^2/n1 + sd2^2/n2)^2) / ((sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1))
  t <- (m1-m2)/se
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  names(dat) = c("mean_difference", "SE", "t", "p-value")
  return(dat)
}

