#' Function to expand variables from a vector of characters
#'
#' @param masterdata : masterdata; not much use, just to know the number of subjects
#' @param data : the dataframe containing the characters of variables
#' @param index : the index column in "data"
#' @param column : the column of characters in "data"
#' @return A matrix with columns for different characters, and rows for different subjects, with values of 1 and 2
expand_var = function(masterdata, data, index, column) {
  col = unlist(unique(data[,column]))
  index2 = unlist(data[,index])
  ncol = length(col)
  nrow = dim(masterdata)[1]
  mat = matrix(nrow = nrow,
               ncol = ncol)
  for (i in 1:ncol) {
    ind = sapply(unlist(data[,column]),
                 function(x){
                   ifelse(x == col[i],
                          1, 0)
                 })
    ind2 = which(ind == 1)
    ID = unique(index2[ind2])
    mat[ID,i] <- 2
    mat[-ID,i] <- 1
  }
  colnames(mat) = col
  return(mat)
}


#' A function to create dummy variables
#'
#' @param column : a column of vectors, e.g. with values 1:6
#' @return a matrix with number of columns equals number of vectors
create_dummy = function(column) {
  n = range(column)[2]
  len = dim(column)[1]
  data = matrix(nrow = len,
                ncol = range(column)[2]-range(column)[1]+1)
  count = 1
  for (i in range(column)[1]:range(column)[2]) {
    data[,count] = sapply(column, function(x) {
      ifelse(x==i, 2, 1)
    })
    count = count+1
  }
  c_name = colnames(column)
  colnames(data) = sapply(range(column)[1]:range(column)[2], function (i) {
    paste(c_name, i, sep = "_")
  })
  return(data)
}

#' To be used with create_dummy(); creating dummies for multiple variables
#'
#' @param masterdata : masterdata
#' @param ind : index of columns in masterdata that we want to create dummy variables
#' @return Matrix of a series of combination of dummy variables
cbind_dummy = function(masterdata, ind) {
  ini = ind[1]
  initial = create_dummy(masterdata[,ini])
  if (length(ind) >=2) {
    for (i in ind[-1]) {
      subsequent = create_dummy(masterdata[,i])
      initial = cbind(initial, subsequent)
    }
    return(initial)
  } else {
    return(initial)
  }
}
