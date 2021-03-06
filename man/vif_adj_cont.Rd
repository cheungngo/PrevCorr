% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VIF.R
\name{vif_adj_cont}
\alias{vif_adj_cont}
\title{VIF for logistic regression with adjustments (continuous var)}
\usage{
vif_adj_cont(datay, datax, j, i, adj)
}
\arguments{
\item{datay}{: dataframe that contains the first variable, e.g. disease diagnosis status}

\item{datax}{: dataframe that contains the second variable}

\item{j}{: column number with respect to datay}

\item{i}{: column number with respect to datax}

\item{adj}{: column numbers for adjustments}
}
\value{
VIF for each variables
}
\description{
VIF for logistic regression with adjustments (continuous var)
}
