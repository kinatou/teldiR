#' Imports a Data Table of Telephone Subscriber Addresses
#'
#' This function loads a processed table of telephone subscriber addresses as an object of classes
#' 'data.table' and 'data.frame' from a given file path. It automatically determines the sub-path
#' to the file (i.e. the year of which it holds information) and allows for users to choose a random
#' subset of addresses to trial their analysis on.
#'
#' @param FLname Character. Name of the table to be imported, including the file extension.
#' This can either be a `.csv` or an `.xslx` file.
#' @param IMPORTname Character. Name of the object to be imported into the global environment.
#' @param sub_rows Integer. Number of rows of records to randomly subset from the original
#' list, which allows testing of code on a smaller number of records. The default value `=-1` 
#' and means no subsetting will take place.
#' @param xpSUBDIR Character. A sub-directory from which the table of addresses is to be taken.
#' It is nestled within the file path: "2.Intermediary/" and the default sub-directory
#' therein is "1.Addr.Tables".
#' @return An object that is both of class 'data.table' and 'data.frame'.
#' @export 
readin_adrDT <- function(FLname, IMPORTname="tld", sub_rows=-1, xpSUBDIR="1.Addr.Tables") {
  
  #reads in data from file path
  if (substr(FLname, 1, 14)=="2.Intermediary") { # , if already specified in full
    DT_x <- data.table::fread(FLname, data.table=TRUE)
  } else { #otherwise, constructs file path to that file using defaults
    fp_x <- file.path("2.Intermediary", xpSUBDIR, stringr::str_sub(FLname, 1, 4), FLname)
    
    if (stringr::str_sub(FLname, -3, -1)=="csv") { #read in .csv files as such
      DT_x <- data.table::fread(fp_x, data.table=TRUE)
    } else if (stringr::str_sub(FLname, -4, -1)=="xlsx") { 
      DT_x  <- data.table::data.table( openxlsx::read.xlsx(fp_x))
    } else { #in case no file extension is specified, by default, read .csv
      DT_x <- data.table::fread( paste0(fp_x, ".csv"), data.table=TRUE)
    }  #and, in some cases, 
    if (sub_rows != -1) { #subsets the rows of the newly imported data.table
      sampled_rows <- sample(1:nrow(DT_x), abs(sub_rows))
      DT_x <- DT_x[sampled_rows,]
    }
  }; Sys.time() #produces different back-halves of notification message dep. on
  if (xpSUBDIR == "1.Addr.Tables") { notif_end <- 
    paste0(" subscriber records across ", length(unique(DT_x[["DirName"]])),
           " unique telephone directories.")  
  } else { notif_end <- paste0("rows with columns such as: ", sample(names(DT_x), 1), ", ", 
                               sample(names(DT_x), 1), " and ", sample(names(DT_x), 1)) }
  print( paste0("The data table imported from ", FLname, " contains ", nrow(DT_x), 
                " ", notif_end))
  assign(IMPORTname, DT_x, envir = parent.frame())
}

#' View a Subset of the Data
#'
#' This function helps users troubleshoot possible errors or oversights in the text cleaning and
#' processing by displaying a subset of the data according to the condition defined by the user.
#' By default, all rows with `'NA'` values in the column `'ifCol1'` are displayed. If the `nchar_thresh`
#' argument is used, then instead, rows with strings of length equal to or lower than this threshold
#' are shown, allowing users to diagnose what the issue might be that led to unusually short character strings.
#'
#' @param DTname Character. Name of the object in the global environment to be exported.
#' @param ifCol1 Character. Name of the first column within the R object to check the condition specified
#' in `nchar_thresh` on.
#' @param ifCol2 Character - optional. Name of the second column within the R object to check the condition on.
#' @param nchar_thresh Numeric - optional. This value determines which rows of the table `DTname` will be shown
#' using `View()`, whereby only records with values in the columns `ifCol1` and `ifCol2` (where applicable)
#' with a string length equal to or lower than this value will be displayed. If this argument is not specified,
#' then any rows with `'NA'` values in the chosen columns will be displayed.
#' @return A view of the same object (a table) but only including rows that fulfil the condition.
#' @export 
view_DTif <- function(DTname, ifCol1, ifCol2=NA, nchar_thresh=-1) {
  DT_x  <- get(DTname)
  
  if (!is.na(ifCol2) & nchar_thresh==-1) { # checks for NA values in 2 columns
    DT_ifCon <- DT_x[is.na(DT_x[[ifCol1]]) & is.na(DT_x[[ifCol2]]), ]
  } else if (is.na(ifCol2) & nchar_thresh==-1) { # checks for NA values in 1 column
    DT_ifCon <- DT_x[is.na(DT_x[[ifCol1]]), ] 
  } else if (!is.na(ifCol2) & nchar_thresh>0) { # checks for `nchar` of 2 columns
    DT_ifCon <- DT_x[ nchar(DT_x[[ifCol1]]) <= nchar_thresh & 
                        nchar(DT_x[[ifCol2]]) < nchar_thresh, ] 
  } else if (is.na(ifCol2) & nchar_thresh>0) { # checks for `nchar` of 1 column
    DT_ifCon <- DT_x[ nchar(DT_x[[ifCol1]]) <= nchar_thresh, ]
  } 
  View(DT_ifCon)
}




#' Exports a Data Table
#'
#' This function loads a processed table of telephone subscriber addresses as an object of classes
#' 'data.table' and 'data.frame' from a given file path. It automatically determines the sub-path
#' to the file (i.e. the year of which it holds information) and allows for users to choose a random
#' subset of addresses to trial their analysis on.
#'
#' @param FLname Character. Name of the table to be imported, including the file extension.
#' This can either be a `.csv` or an `.xslx` file.
#' @param DTname Character. Name of the object in the global environment to be exported.
#' @param dirYR Numeric. Year about which the table to be exported holds information. This argument will
#' be used to place exports automatically in a sub-folder corresponding to the year. The default is
#' `=NULL`, in which case the year information will be automatically taken from the table.
#' @param xpNAME Character. Forms part of the name of the outputted file(s). This will be 
#' combined with a timestamp of when the file was exported.
#' @param csv Logical. If `=1`, the object will be exported as a `.csv` file. Default is `=1`.
#' @param xlsx Logical. If `=1`, the object will be exported as a `.xlsx` file. Default is `=0`.
#' @param xpSUBDIR Character. A sub-directory to which the object will be exported.
#' It is nestled within the file path: "2.Intermediary/" and the default sub-directory
#' therein is "1.Addr.Tables".
#' @return An object that is both of class 'data.table' and 'data.frame'.
#' @export 
export_fullDT <- function(DTname="dtAPH", dirYR=NULL, xpNAME="Ls.Adr", csv=1, xlsx=0,
                          xpSUBDIR="1.Addr.Tables") {
  if (is.null(dirYR)) { #default - extracts year from `DirName` variable
    dirYR <- get(DTname)[1, "DirName"] %>% gsub("^.*_(?=1[89])","", ., perl=T) %>% stringr::str_sub(1, 4)
    print( paste("Automatically getting directory year from variable `DirName`:", dirYR))  
  }
  xpTIME <- Sys.time() %>%  stringr::str_replace_all("[[:punct:]]","") %>% 
    stringr::str_replace_all(" ","_") %>% stringr::str_sub(., 3, -3)
  print("Preparing to export data table of directory entries as an MS Excel file...")
  if (xlsx==1) { #exports as an .xlsx file
    xpDIR <- file.path("2.Intermediary", xpSUBDIR, dirYR, 
                       paste0(dirYR, ".", xpNAME, ".", xpTIME, ".xlsx"))
    write.xlsx(get(DTname), xpDIR); print("The processed file has been outputted as an .xlsx file.")
  } 
  if (csv==1) { #exports as a .csv file
    xpDIR <- file.path("2.Intermediary", xpSUBDIR, dirYR, 
                       paste0(dirYR, ".", xpNAME, ".", xpTIME, ".csv"))
    data.table::fwrite(get(DTname), xpDIR, row.names=F)
    print("The processed file has been outputted as an .csv file.")
  } #N.B. exporting .xlsx file takes a while with large volumes of records
  Sys.time()
} 
