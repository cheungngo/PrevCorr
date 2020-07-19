#' Merging the results (of logistic regression)
#'
#' @param masterdata : masterdata
#' @param path : file path
#' @param number : file chapter number
#' @param key : identifier (e.g. ISSM / PEDT)
#' @param ind : desired variables for merging
#' @return Merged results
merge_coef = function(masterdata, path, number, key, ind) {
  ini = ind[1]
  initial = as.data.frame(read_csv(paste(path, sprintf("%02d", number), key, colnames(masterdata)[ini], ".csv", sep = "")))
  rownames(initial) = paste(colnames(masterdata)[ini], unlist(initial[,1]), sep = "_")
  initial = initial[,-1]
  if (length(ind) >=2) {
    for (i in ind[-1]) {
      appendix = as.data.frame(read_csv(paste(path, sprintf("%02d", number), key, colnames(masterdata)[i], ".csv", sep = "")))
      rownames(appendix) = paste(colnames(masterdata)[i], unlist(appendix[,1]), sep = "_")
      appendix = appendix[,-1]
      initial = rbind(initial, appendix)
    }
    return(initial)
  } else {
    return(initial)
  }
}


#' Beautifying results (OR)
#'
#' @param data : results of logistic regression (in the form of OR and CI)
#' @return Beautified results
beautify_cate = function (data) {
  data[,1:3] = apply(data[,1:3], 2, function(i) {
    round(i, digits = 1)
  })
  OR_b = apply(data, 1, function (i) {
    if (i[2] != 1) {
      return(paste(i[2], " (", i[1], "-", i[3], ")", sep = ""))
    } else {
      return(1)
    }
  })
  return(as.data.frame(cbind(OR_b, data[,4])))
}


#' Beautifying results of logistic regression (with normal regression output)
#'
#' @param cont : results of logistic regression with normal regression output
#' @return beautified results
cont_beau = function(cont) {
  OR = apply(cont, 1, function (i) {
    exp(i[1] + qnorm(c(0.025,0.5,0.975)) * i[2])
  })
  OR = t(OR)
  OR_b = apply(OR, 1, function (i) {
    if (i[2] != 1) {
      return(paste(round(i[2],2), " (", round(i[1],2), "-", round(i[3],2), ")", sep = ""))
    } else {
      return(1)
    }
  })
  result = as.data.frame(cbind(OR_b, cont[,4]))
  colnames(result) = c("OR","p")
  return(result)
}
