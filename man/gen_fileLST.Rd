% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_ocr_text.R
\name{gen_fileLST}
\alias{gen_fileLST}
\title{Generates List of Text Files Containing Telephone Subscriber Information}
\usage{
gen_fileLST(
  dirYR,
  rootDR = "0.In.Archives/4.Out.Text",
  pgTYP = "Subscriber_Records",
  fl_pattern = "\\\\.txt$",
  sub_Pgs = -1
)
}
\arguments{
\item{dirYR}{Numeric. Year for which information from the telephone directories should be extracted.}

\item{rootDR}{Character. File path to the root folder that contains the .txt files resultant from
the Optical Character Recognition (OCR) process.}

\item{pgTYP}{Character. Name of category of page files to be processed, which should also be
a sub-folder within the file path \code{rootDR}.}

\item{fl_pattern}{Character. Regular Expression (RegEx) pattern used to filter for file names
that match the specified expression. By default, the function includes any files with the extension \code{.txt}.}

\item{sub_Pgs}{Numeric. Number of files to be randomly sampled for inclusion in the generated
list. The default is \verb{=-1}, which means no sampling occurs and that all available files are included.}
}
\value{
A list of names of files in the specified directory.
}
\description{
This function returns a list of the names of files within a certain directory, containing
data from a particular year that match a certain string query. The output can then be used
for importing the contents of those files through a loop.
}
