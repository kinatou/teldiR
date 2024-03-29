% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_ocr_text.R
\name{prep_pgDT}
\alias{prep_pgDT}
\title{Prepares Table to Hold Information and Metadata from Single Text Files}
\usage{
prep_pgDT(DTname = "pgAPH", start_vCol = 3, end_vCol = 4, fl)
}
\arguments{
\item{DTname}{Character. Name of the R object (a table) on which the operation is to be run.
By default, the function \code{gen_fileLST()} creates this object by listing the files held in one folder.}

\item{start_vCol}{Numeric. Beginning from which number should 'V-Columns', i.e. 'V1', 'V2', 'V3'...,
be created? The default value is \verb{=3}, meaning \code{'V3'}, since by default, columns \code{'V1'} and \code{'V2'} would
have been initialised at the point of importing information from a text file using \code{readin_PG()}.}

\item{end_vCol}{Numeric. Similar to \code{start_vCol}, except this number indicates what the final 'V-Column'
should be. The default is \verb{=4}, meaning the last 'V-Column' to be created is \code{'V4'}}

\item{fl}{Numeric. Index/position of the name of the file to be processed, in the list \code{ls_aphpg}.}
}
\value{
An expanded 'data.table' and 'data.frame' object with more columns to hold different types of information.
}
\description{
This function expands the tables of subscriber information imported with the function \code{readin_PG()}
by adding empty columns (for information to be split up into) and adding columns that hold information
on the file of origin, such as directory name, page number and (if applicable) column number).
}
