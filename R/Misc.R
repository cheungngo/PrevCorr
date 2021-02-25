#' Shapiro test
#'
#' @param data : the dataframe concerned
#' @param col : the column of the variable in the dataframe concerned
#' @return statistics and p-value of shapiro test
shapiro = function(data, col) {
  shap = shapiro.test(unlist(data[,col]))
  return(c(colnames(data)[col],
           round(shap$stat, digits = 3),
           shap$p.value))
}

#' A series of shapiro test
#'
#' @param data : the dataframe concerned
#' @param cols : the columns of the variable in the dataframe concerned
#' @return statistics and p-value of a series of shapiro tests
serial_shapiro = function(data,cols) {
  mat = matrix(nrow = length(cols), ncol = 3)
  n = 1
  for (i in cols) {
    mat[n,] <- unlist(shapiro(data,i))
    n = n + 1
  }
  return(mat)
}

#' Wilcoxon test
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @return Statistics and p-values of wilcoxon tests
wilcox = function (datay, datax, j, i) {
  dat = unlist(datax[,i])
  y = unlist(datay[,j])
  a = dat[which(y==0)]
  b = dat[which(y==1)]
  wil = wilcox.test(a,b)
  return(c(colnames(datay)[j],
           colnames(datax)[i],
           wil$stat,
           wil$p.value,
           paste(median(a)," (", IQR(a),") ", sep = ""),
           paste(median(b)," (", IQR(b),") ", sep = "")))
}

#' A series of wilcoxon test
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second to nth variables
#' @param j : column number with respect to datay
#' @param is : column numbers with respect to datax
#' @return Statistics and p-values of a series of wilcoxon tests
serial_wilcox = function(datay, datax, j, is) {
  mat = matrix(nrow = length(is), ncol = 6)
  n = 1
  for (x in is) {
    mat[n,] <- unlist(wilcox(datay, datax, j, x))
    n = n + 1
  }
  return(mat)
}

#' Frequencies of occurences
#'
#' @param data : the dataframe concerned
#' @param col : the column of the variable in the dataframe concerned
#' @return a table presenting the frequencies of occurences
foo = function(data, col) {
  dat = unlist(data[,col])
  cla = sort(unique(dat))
  mat = matrix(nrow = length(cla), ncol = 2)
  for (i in 1:length(cla)) {
    mat[i,] = c(paste(colnames(data)[col],cla[i],sep="_"),
                paste(length(which(dat==cla[i]))," (", round(length(which(dat==cla[i])) / length(dat) * 100, digits = 2), "%) ", sep = ""))
  }
  return(mat)
}

#' Frequencies of occurences for multiple variables
#'
#' @param data : the dataframe concerned
#' @param cols : the columns of the variable in the dataframe concerned
#' @return a table presenting the frequencies of occurences for multiple variables
serial_foo = function(data,cols) {
  for (i in cols) {
    assign(paste("tab", colnames(data)[i],sep = ""),
           foo(data,i))
  }
  tab = eval(parse(text = paste("tab", colnames(data)[cols[1]],sep = "")))
  for (i in cols[-1]) {
    tab2 = eval(parse(text = paste("tab", colnames(data)[i],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(tab)
}

#' Median & IQR
#'
#' @param data : the dataframe concerned
#' @param col : the column of the variable in the dataframe concerned
#' @return Median and IQR of the concerned variable
medianiqr = function (data, col) {
  dat = unlist(data[col])
  return(c(colnames(data)[col],
           paste(median(dat),
                 " (",
                 IQR(dat),
                 ")",
                 sep = "")))
}

#' A series of median & IQR
#'
#' @param data : the dataframe concerned
#' @param cols : the columns of the variable in the dataframe concerned
#' @return Median and IQR of the concerned variables
serial_miqr = function(data,cols) {
  mat = matrix(nrow = length(cols), ncol = 2)
  n = 1
  for (i in cols) {
    mat[n,] <- unlist(medianiqr(data,i))
    n = n + 1
  }
  return(mat)
}

