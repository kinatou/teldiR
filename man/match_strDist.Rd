% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsequent_data_prep.R
\name{match_strDist}
\alias{match_strDist}
\title{Geocoding (Fuzzy matches two string fields)}
\usage{
match_strDist(dt1, dt2, col1, col2, xtr1, xtr2)
}
\arguments{
\item{dt1}{Data Table. Name of object in the global environment which corresponds
to the records of addresses that are to be geocoded.}

\item{dt2}{Data Table. Name of object in the global environment which corresponds
to the database of already geocoded addresses. The default used was the OS AddressBase
from the Ordnance Survey.}

\item{col1}{Character. Name of field in the data table \code{dt1} which contains the street
names of the addresses and which is to be used for geocoding.}

\item{col2}{Character. Name of field in the data table \code{dt2} which contains the street
names of the already geocoded addresses.}

\item{xtr1}{Vector of characters. Names of additional fields of information from \code{dt1} (other
than \code{col1}) which the user wishes to retain in the final output of the function.}

\item{xtr2}{Vector of characters. Names of additional fields of information from \code{dt2}(other
than \code{col2}) which the user wishes to retain in the final output of the function.}
}
\value{
The same object in the global environment (\code{dt1}) with additional fields appended
to it following the process of geocoding.
}
\description{
N.B. This currently works with \verb{\%do\%} in the package but not with \verb{\%dopar\%} (which only works
outside the package, if the function is loaded into global environment!)
This function is used primarily for geocoding. How it exactly works is by fuzzy
matching two columns/fields that each contain character strings (of address street names).
}
