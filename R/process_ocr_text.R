#' Creates Empty Object to Store All Output Text from Telephone Directories
#'
#' This function initialises an empty object in the global environment to store ALL the processed
#' data from the outputs of Optical Character Recognition (OCR) from one directory. This is opposed to
#' some other functions in the package which deal specifically with the outputs from a single .txt. file.
#' @param masterDTname Character. Name of the empty object to be created in the global environment.
#' @param num_vCol Numeric. Number of "V-" Columns to create in the new data frame/table object.
#' These will be used to store information such as telephone number, subscriber name and address.
#' @param pg_split Logical. Did one page of the telephone directories concerned comprise
#' more than one column of data? If yes, an additional column `PgCol` will be made. Default `=TRUE`.
#' @return An empty table of both types 'data.table' and 'data.frame', to which information can be appended.
#' @export 
create_emptyDT <- function(masterDTname="dtAPH", num_vCol=3, pg_split=TRUE) {
  # initialises the list of columns for data.table to contain, depending on whether Page Splitting had to be done
  if (pg_split==TRUE) { ls_vCol <- c("DirName", "OriPg", "PgCol");  
  } else { ls_vCol <- c("DirName", "OriPg") }
  # appends the names of `V-` columns to the list of column names, i.e. columns to hold "broken-down addresses"
  for (i in 1:num_vCol) { ls_vCol <- append(ls_vCol, paste0("V", i)) }
  # outputs the data.table of appropriate length and column names as
  assign(x = masterDTname, envir = parent.frame(),
         value = setNames(data.table::data.table(matrix(nrow = 0, ncol = length(ls_vCol))), ls_vCol) )
}


#' Generates List of Text Files Containing Telephone Subscriber Information
#'
#' This function returns a list of the names of files within a certain directory, containing
#' data from a particular year that match a certain string query. The output can then be used
#' for importing the contents of those files through a loop.
#'
#' @param dirYR Numeric. Year for which information from the telephone directories should be extracted.
#' @param rootDR Character. File path to the root folder that contains the .txt files resultant from
#' the Optical Character Recognition (OCR) process.
#' @param pgTYP Character. Name of category of page files to be processed, which should also be
#' a sub-folder within the file path `rootDR`.
#' @param fl_pattern Character. Regular Expression (RegEx) pattern used to filter for file names
#' that match the specified expression. By default, the function includes any files with the extension `.txt`.
#' @param sub_Pgs Numeric. Number of files to be randomly sampled for inclusion in the generated
#' list. The default is `=-1`, which means no sampling occurs and that all available files are included.
#' @return A list of names of files in the specified directory.
#' @export 
gen_fileLST <- function(dirYR, rootDR="0.In.Archives/4.Out.Text", pgTYP="Subscriber_Records", 
                        fl_pattern="\\.txt$", sub_Pgs=-1) {
  pathYR         <- file.path(rootDR, dirYR, pgTYP)
  fl_aphpg       <- list.files(path = pathYR, pattern = fl_pattern) #creates list of file names
  #appends list w/ full path directories to .txt files that are outputted from OCR
  ls_aphpg       <<- paste0(pathYR, "/", fl_aphpg)
  #randomly samples `sub_Pgs` no. of pages, if specified by user
  if (sub_Pgs>0) { ls_aphpg <<- sample(ls_aphpg, sub_Pgs) }
  print("The names of files detected have been stored in a list called `ls_aphpg`.")
}


#' Imports Text from a Single Text File
#'
#' This function imports the information from a text file, usually the outputs of an OCR
#' algorithm and structures it into a table in preparation for subsequent operations.
#'
#' @importFrom data.table ":="
#' @param fl Numeric. Index or position of the name of file to be imported, in the object
#' `ls_aphpg` that was created using the function `gen_fileLST()`.
#' @param sep Character. Single-byte character that is treated as the marker of separation
#' of fields within a single line (row). The default value is `"+"` which is assumed
#' to be a character that will not appear in any of the input text files, and this is
#' desired so that there will not yet be any separation of information into fields in the output.
#' @return A table of two columns, containing information from a single .txt OCR output file.
#' @export 
readin_PG <- function(fl, sep="+"){ #`fl` refers to iteration of loop in the list of all pages
  pgAPH         <<- data.table::as.data.table(
    read.table( file = as.character(ls_aphpg[fl]), header=F, blank.lines.skip=T,
                #delinates columns by '|'; strips white space around separator;
                #fills in blanks with implicit rows (assuming rows of unequal length)
                sep = sep, strip.white=T, fill=T, col.names = c('V1', 'V2'), quote="",
                stringsAsFactors=F #prevents strings from being read as factors
    )) %>% dplyr::select(V1) %>% .[, V2 := V1] #duplicates V1; keeps V1 as original string
} # N.B. using data.table syntax (rather than dplyr) to clone V1 as V2 was crucial to create a "deep copy"!


#' Prepares Table to Hold Information and Metadata from Single Text Files
#'
#' This function expands the tables of subscriber information imported with the function `readin_PG()`
#' by adding empty columns (for information to be split up into) and adding columns that hold information
#' on the file of origin, such as directory name, page number and (if applicable) column number).
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' By default, the function `gen_fileLST()` creates this object by listing the files held in one folder.
#' @param start_vCol Numeric. Beginning from which number should 'V-Columns', i.e. 'V1', 'V2', 'V3'...,
#' be created? The default value is `=3`, meaning `'V3'`, since by default, columns `'V1'` and `'V2'` would
#' have been initialised at the point of importing information from a text file using `readin_PG()`.
#' @param end_vCol Numeric. Similar to `start_vCol`, except this number indicates what the final 'V-Column'
#' should be. The default is `=4`, meaning the last 'V-Column' to be created is `'V4'`
#' @param fl Numeric. Index/position of the name of the file to be processed, in the list `ls_aphpg`.
#' @return An expanded 'data.table' and 'data.frame' object with more columns to hold different types of information.
#' @export 
prep_pgDT <- function(DTname="pgAPH", start_vCol=3, end_vCol=4, fl){
  DT_x <- get(DTname)
  
  # creates new columns for separating strings and breaking down addresses
  for (i in start_vCol:end_vCol) { DT_x[[ eval(paste0("V", i)) ]] <- as.character( rep(NA, nrow(DT_x))) }
  
  # tests, from file name, whether pages from that year's directories comprise multiple columns
  if (regexpr("\\_[1|2|3]\\.?(tif|j2k|png)?\\.txt$", ls_aphpg[fl], perl=T)>0) { # if YES contains columns,
    # extracts, from file name: (f)ile (p)age number, (col)umn number and dir(ectory) n(a)me
    DT_x[["OriPg"]]    <- ls_aphpg[fl] %>% sub("\\_[1|2|3]\\.?(tif|j2k|png)?\\.txt$", "", .) %>% stringr::str_sub(., -5,-1) %>% as.numeric()
    DT_x[["PgCol"]]    <- ls_aphpg[fl] %>% sub("\\.?(tif|j2k|png)?\\.txt$", "", .) %>% stringr::str_sub(., -1) %>% as.numeric()
  } else {
    DT_x[["OriPg"]]    <- ls_aphpg[fl] %>% sub("\\.?(tif|j2k|png)?\\.txt$", "", .) %>% stringr::str_sub(., -5,-1) %>% as.numeric()
  }; DT_x[["DirName"]]  <- ls_aphpg[fl] %>% gsub("^.*(?=bt_[0-9])","", ., perl=T) %>% gsub("\\-\\d{2,}.*(\\.txt)?$","", .) %>% as.character()
  ### N.B. `stringr::str_sub()` from `stringr` package allows you to subset from the string's end
  # appends these values to the `pgAPH` data.table
  assign(DTname, DT_x, envir = .GlobalEnv)
}
