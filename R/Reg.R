#' Function calculating the confidence interval from the coefficient obtained by logistic regression
#'
#' @param m : model calculated by glm()
#' return an array of odds ratio (LCI, OR, UCI)
OR_CI = function(m) {
  ifelse(is.na(coef(m)[2]),
         return(c(NA,NA,NA)),
         return(exp(summary(m)$coefficients[2,1] +
                      qnorm(c(0.025,0.5,0.975)) * summary(m)$coefficients[2,2])))
}


#' Logistic regression (categorical independent variable; referenced to the category with most subjects)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @return Odds ratio and CIs
log_reg_spss_max = function(datay, datax, j, i) {
  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  coeff = matrix(nrow = length(table(var)),
                 ncol = 4)
  coeff[1,] = c(NA,1,NA,NA)
  ref = as.numeric(unlist(names(which.max(table(var)))))
  level = as.numeric(unlist(levels(as.factor(var))))
  level = level[-which(level==ref)]
  count = 2
  for (k in level) {
    ind2 = which(var==k | var==ref)
    yy = dep[ind2]
    xx = var[ind2]
    modglm = glm(yy~xx, family = "binomial")
    coeff[count,1:3] = OR_CI(modglm)
    ifelse(is.na(coef(modglm)[2]),
           coeff[count,4] <- NA,
           coeff[count,4] <- as.numeric(unlist(summary(modglm)$coefficients[2,4])))
    count = count + 1
  }
  rownames(coeff) = c(ref, level)
  colnames(coeff) = c("LCI", "OR", "UCI", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}



#' Logistic regression (categorical independent variable; specific referencing category)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param kref : specific referencing categories
#' @return Odds ratio and CIs
log_reg_spss_kref = function(datay, datax, j, i, kref) {
  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  coeff = matrix(nrow = length(table(var)),
                 ncol = 4)
  coeff[1,] = c(NA,1,NA,NA)
  ref = kref
  level = as.numeric(unlist(levels(as.factor(var))))
  level = level[-which(level==ref)]
  count = 2
  for (k in level) {
    ind2 = which(var==k | var==ref)
    yy = dep[ind2]
    xx = var[ind2]
    modglm = glm(yy~xx, family = "binomial")
    coeff[count,1:3] = OR_CI(modglm)
    ifelse(is.na(coef(modglm)[2]),
           coeff[count,4] <- NA,
           coeff[count,4] <- as.numeric(unlist(summary(modglm)$coefficients[2,4])))
    count = count + 1
  }
  rownames(coeff) = c(ref, level)
  colnames(coeff) = c("LCI", "OR", "UCI", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}


#' Logistic regression (continuous independent variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @return Normal output of regression
log_reg_spss_cont = function(datay, datax, j, i) {
  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  coeff = matrix(ncol = 4)
  modglm = glm(dep~var, family = "binomial")
  coeff[1,] = summary(modglm)$coef[2,]
  rownames(coeff) = colnames(datax)[i]
  colnames(coeff) = c("Estimate", "SD", "z-value", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}


#' Logistic regression with adjustment (continuous independent variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param adj : column number(s) in datax for adjustments
#' @return Normal output of regression
log_reg_spss_cont_adj = function(datay, datax, j, i, adj) {

  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]

  coeff = matrix(ncol = 4)

  f = as.formula(paste("dep",
                       paste("datax_n$", var_names, sep = "", collapse = "+"),
                       sep = "~"))

  modglm = glm(f, family = "binomial")
  coeff[1,] = summary(modglm)$coef[2,]
  rownames(coeff) = colnames(datax)[i]
  colnames(coeff) = c("Estimate", "SD", "z-value", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}

#' A series of logistic regression with adjustment (continuous independent variable)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param adj : column number(s) in datax for adjustments
#' @return A beautified output for the series of regressions
serial_logregkacont = function (datay, datax, j, i, adj) {
  for (x in i) {
    assign(paste("mod", colnames(datax)[x],sep = ""),
           log_reg_spss_cont_adj(datay, datax, j, x, adj))
  }
  tab = eval(parse(text = paste("mod", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("mod", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(cont_beau(tab))
}


#' Logistic regression (continuous independent variable; including 0)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @return Normal output of regression
log_reg_spss_cont0 = function(datay, datax, j, i) {
  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which(!is.na(var))
  var = var[ind]
  dep = dep[ind]
  coeff = matrix(ncol = 4)
  modglm = glm(dep~var, family = "binomial")
  coeff[1,] = summary(modglm)$coef[2,]
  rownames(coeff) = colnames(datax)[i]
  colnames(coeff) = c("Estimate", "SD", "z-value", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}


#' Logistic regression with adjustment (continuous independent variable; including 0)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param adj : column number(s) in datax for adjustments
#' @return Normal output of regression
log_reg_spss_cont_adj0 = function(datay, datax, j, i, adj) {

  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which(!is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]

  coeff = matrix(ncol = 4)

  f = as.formula(paste("dep",
                       paste("datax_n$", var_names, sep = "", collapse = "+"),
                       sep = "~"))

  modglm = glm(f, family = "binomial")
  coeff[1,] = summary(modglm)$coef[2,]
  rownames(coeff) = colnames(datax)[i]
  colnames(coeff) = c("Estimate", "SD", "z-value", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}


#' A series of logistic regression with adjustment (continuous independent variable; including 0)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param adj : column number(s) in datax for adjustments
#' @return A beautified output for the series of regressions
serial_logregkacont0 = function (datay, datax, j, i, adj) {
  for (x in i) {
    assign(paste("mod", colnames(datax)[x],sep = ""),
           log_reg_spss_cont_adj0(datay, datax, j, x, adj))
  }
  tab = eval(parse(text = paste("mod", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("mod", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(cont_beau(tab))
}


#' Logistic regression with adjustments (categorical independent variable; specific referencing category)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @param kref : specific referencing categories
#' @param adj : column numbers for adjustments
#' @return Odds ratio and CIs
log_reg_spss_kref_adj = function(datay, datax, j, i, kref, adj) {
  var_names = c(colnames(datax)[i], colnames(datax)[adj])

  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var))
  var = var[ind]
  dep = dep[ind]
  datax_n = datax[ind,]
  coeff = matrix(nrow = length(table(var)),
                 ncol = 4)
  coeff[1,] = c(NA,1,NA,NA)
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
    coeff[count,1:3] = OR_CI(modglm)
    ifelse(is.na(coef(modglm)[2]),
           coeff[count,4] <- NA,
           coeff[count,4] <- as.numeric(unlist(summary(modglm)$coefficients[2,4])))
    count = count + 1
  }
  rownames(coeff) = paste(var_names[1], c(ref, level), sep = "_")
  colnames(coeff) = c("LCI", "OR", "UCI", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}

#' A Series of logistic regression with adjustments (categorical independent variable; specific referencing category)
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column numbers with respect to datax
#' @param kref : specific referencing categories
#' @param adj : column numbers for adjustments
#' @return A beautified output for the series of regressions
serial_logregka = function (datay, datax, j, i, kref, adj) {
  for (x in i) {
    assign(paste("mod", colnames(datax)[x],sep = ""),
           log_reg_spss_kref_adj(datay, datax, j, x, kref, adj))
  }
  tab = eval(parse(text = paste("mod", colnames(datax)[i[1]],sep = "")))
  for (x in i[-1]) {
    tab2 = eval(parse(text = paste("mod", colnames(datax)[x],sep = "")))
    tab = rbind(tab, tab2)
  }
  return(beautify_cate(tab))
}


#' linear regression
#'
#' @param datay : dataframe that contains the first variable, e.g. disease diagnosis status
#' @param datax : dataframe that contains the second variable
#' @param j : column number with respect to datay
#' @param i : column number with respect to datax
#' @return Normal output of a regression
lin_reg = function (datay, datax, j, i) {
  var = as.numeric(unlist(datax[,i]))
  dep = as.numeric(unlist(datay[,j]))
  ind = which((var!=0) | !is.na(var) | (dep!=0) | !is.na(dep))
  var = var[ind]
  dep = dep[ind]
  coeff = matrix(ncol = 4)
  mod = lm(dep~var)
  ifelse(is.na(coef(mod)[2]),
         coeff[1,] <- NA,
         coeff[1,] <- summary(mod)$coef[2,])
  rownames(coeff) = colnames(datax)[i]
  colnames(coeff) = c("Estimate", "SD", "z-value", "p-value")
  coeff = round(coeff, digits = 3)
  return(coeff)
}
