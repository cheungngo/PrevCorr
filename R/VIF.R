#' VIF for logistic regression with adjustments (categorical independent variable; specific referencing category)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param kref : specific referencing categories
#' @param adj : column numbers for adjustments
#' @return VIF for each variables
vif_adj = function(datay, datax, j, i, kref, adj) {
  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]
  coeff = matrix(nrow = length(table(var)),
                 ncol = length(adj)+1)
  ref = kref
  level = as.numeric(unlist(levels(as.factor(var))))
  level = level[-which(level==ref)]
  count = 2

  for (k in level) {
    ind2 = which(var==k | var==ref)
    yy = dep[ind2]
    xx = datax_n[ind2,]
    f = as.formula(paste("yy",
                         paste("xx$", var_names, sep = "", collapse = "+"),
                         sep = "~"))
    modglm = glm(f, family = "binomial")
    coeff[count,] <- try(round(unlist(car::vif(modglm)), digits = 3))
    count = count + 1
  }
  rownames(coeff) = paste(var_names[1], c(ref, level), sep = "")
  colnames(coeff) = c("var", var_names[-1])
  return(coeff)
}

#' VIF for a series of logistic regression with adjustments (categorical independent variable; specific referencing category)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param kref : specific referencing categories
#' @param adj : column numbers for adjustments
#' @return VIF for variables in a series of logistic regression
serial_vif = function (datay, datax, j, i, kref, adj) {

  for (x in i) {
    assign(paste("vif", colnames(datax)[x],sep = ""),
           vif_adj(datay, datax, j, x, kref, adj))
  }
  tab = eval(parse(text = paste("vif", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("vif", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(tab)
}

#' VIF for logistic regression with adjustments (continuous var)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param adj : column numbers for adjustments
#' @return VIF for each variables
vif_adj_cont = function(datay, datax, j, i, adj) {

  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) & !is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]

  f = as.formula(paste("dep",
                       paste("datax_n$", var_names, sep = "", collapse = "+"),
                       sep = "~"))

  modglm = glm(f, family = "binomial")
  return(unlist(try(round(unlist(car::vif(modglm)), digits = 3))))
}

#' VIF for a series of logistic regression with adjustments (continuous var)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param adj : column numbers for adjustments
#' @return VIF for variables in a series of logistic regression
serial_vif_cont = function (datay, datax, j, i, adj) {

  for (x in i) {
    assign(paste("vif", colnames(datax)[x],sep = ""),
           vif_adj_cont(datay, datax, j, x, adj))
  }
  tab = eval(parse(text = paste("vif", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("vif", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }

  return(tab)
}

#' VIF for logistic regression with adjustments (continuous var; including zero cont vars)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param adj : column numbers for adjustments
#' @return VIF for each variables
vif_adj_cont_0 = function(datay, datax, j, i, adj) {

  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which(!is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]

  f = as.formula(paste("dep",
                       paste("datax_n$", var_names, sep = "", collapse = "+"),
                       sep = "~"))

  modglm = glm(f, family = "binomial")
  return(unlist(try(round(unlist(car::vif(modglm)), digits = 3))))
}

#' VIF for a series of logistic regression with adjustments (continuous var; including zero cont vars)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param adj : column numbers for adjustments
#' @return VIF for variables in a series of logistic regression
serial_vif_cont_0 = function (datay, datax, j, i, adj) {


  for (x in i) {
    assign(paste("vif", colnames(datax)[x],sep = ""),
           vif_adj_cont_0(datay, datax, j, x, adj))
  }
  tab = eval(parse(text = paste("vif", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("vif", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(tab)
}
