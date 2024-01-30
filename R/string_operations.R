#' Merge Multiple Rows that Contain Data on the Same Subscriber
#'
#' This function combines consecutive rows that contain information on the same telephone subscriber
#' by using up to three different Regular Expressions (RegEx) string pattern queries. Based on whether
#' the patterns in these queries are detected (indicating a split-up record), every row of data
#' will be given a score and merged into the row above it if their score is higher than that of the other row.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#' @param Regex_str1 Character. Regular Expression (RegEx) pattern to be searched for within
#' character strings and, following that, to apply the operation onto.
#' @param weight1 Numeric - optional. Weighting of the RegEx pattern `Regex_str1` in determining
#' whether or not to collapse a row of records to the row above. The higher this number, the more
#' important `Regex_str1` will be in comparison to the other two RegEx patterns. Default value `=1`.
#' @param Regex_str2 Character - optional. See `Regex_str1`.
#' @param Regex_str3 Character - optional. See `Regex_str1`.
#' @param nchar_from_back Numeric - optional. Number of characters from the back of
#' the character string to restrict the string search operation to. If this argument is used,
#' then the argument `nchar_from_front` should not be used.
#' @param nchar_from_front Numeric - optional. Number of characters from the front of
#' the character string to restrict the string search operation to. If this argument is used,
#' then the argument `nchar_from_back` should not be used.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
collapse_RowsUpIf <- function(DTname, vCol, Regex_str1, weight1=1,Regex_str2=NA, Regex_str3=NA,
                              nchar_from_front=-1, nchar_from_back=-1) {
  DT_x <- get(DTname)

  #subsets string in relevant `vCol` and converts it into a vector of strings
  if (nchar_from_front>0) { vSub <- stringr::str_sub(DT_x[[vCol]], 1, nchar_from_front)
  } else if (nchar_from_back>0) { vSub <- stringr::str_sub(DT_x[[vCol]], -nchar_from_back, -1)
  } else { vSub <- DT_x[[vCol]] }

  #detects presence/absence of up to 3 string patterns (to restrict front/back, use `^` and `$`)
  posX <- as.integer(stringr::str_detect(vSub, Regex_str1) * weight1)
  if (!is.na(Regex_str2)) { posY <- as.integer(stringr::str_detect(vSub, Regex_str2)); posX <- posX + posY }
  if (!is.na(Regex_str3)) { posZ <- as.integer(stringr::str_detect(vSub, Regex_str3)); posX <- posX + posZ }


  for ( i in 2:nrow(DT_x) ) if ( !is.na(posX[i]) & !is.na(posX[i-1]) & posX[i]>posX[i-1] &
                                 stringr::str_sub(DT_x[[vCol]][i], 1, 3) != stringr::str_sub(DT_x[[vCol]][i-1], 1, 3) ) {
    DT_x[(i-1), eval(vCol) := paste(DT_x[[vCol]][i-1], DT_x[[vCol]][i])]
    DT_x[    i, eval(vCol) := ""]
  }
  assign(DTname, DT_x, envir = parent.frame())
}

#' Remove Character Strings of Unwanted Characters
#'
#' This function trims unwanted characters of strings in a particular column at two ends - either
#' the back (by default) or the front -, using a Regular Expressions (RegEx) string pattern query.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#' @param Regex_str Character. Regular Expression (RegEx) pattern to be searched for within
#' character strings and, following that, to apply the operation onto.
#' @param nchar_from_back Numeric - optional. Number of characters from the back of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_front` should not be used.
#' @param nchar_from_front Numeric - optional. Number of characters from the front of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_back` should not be used.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
trim_string <- function(DTname="pgAPH", vCol, Regex_str, nchar_from_back=-1, nchar_from_front=-1){
  DT_x <- get(DTname) ### Each stage below works differently based on user settings

  # Stage One - Subset string first (if necessary), then detect `Regex_str` pattern therein
  if (nchar_from_back>0) { # subsets the last X characters of string
    oriAPH <- stringr::str_sub(DT_x[[vCol]], -(nchar_from_back), -1) %>%
      regexpr(Regex_str, ., perl=T)
  } else if (nchar_from_front>0) { # subsets the first X characters of string
    oriAPH <- regexpr(Regex_str, substr(DT_x[[vCol]], 1, nchar_from_front), perl=T)
  } else { oriAPH <- regexpr(Regex_str, DT_x[[vCol]], perl=T) } # or don't subset

  # Stage Two - Trim strings' front or back portions based on the Regex positions found above
  for ( i in 1:nrow(DT_x) ) if ( !is.na(oriAPH[i]) & oriAPH[i]>1 ) { # for every row,
    # print(DT_x[i, ..vCol] %>% as.character() ) #prints the original string
    if (nchar_from_back>0) { #trim from the back of string; keep the front
      DT_x[i, eval(vCol) := stringr::str_sub(DT_x[[vCol]][i], 1, -(nchar_from_back - oriAPH[i] + 2)) ]
    } else if (nchar_from_front>0){ # or trim from the front of string; keep the back
      DT_x[i, eval(vCol) := stringr::str_sub(DT_x[[vCol]][i], oriAPH[i], -1)]
    } else { # DEFAULT OPTION - trim the back of the string; keep the front
      DT_x[i, eval(vCol) := stringr::str_sub(DT_x[[vCol]][i], 1, oriAPH[i])]
    }
    # print(DT_x[i, ..vCol] %>% as.character() ) #prints the altered string
  }
  assign(DTname, DT_x, envir = parent.frame())
}


#' Split Character Strings into Two Columns
#'
#' This function separates the contents of a character string into two columns using
#' a Regular Expressions (RegEx) string pattern query.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param fromCol Character. Name of the column from which the string to be split is taken.
#' @param toCol Character. Name of the column to which the split-off string will be allocated.
#' @param Regex_str Character. Regular Expression (RegEx) pattern to be searched for within
#' character strings and, following that, to apply the operation onto.
#' @param nchar_from_back Numeric - optional. Number of characters from the back of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_front` should not be used.
#' @param nchar_from_front Numeric - optional. Number of characters from the front of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_back` should not be used.
#' @param rvrs_frontback Binary - optional. If `=1`, then the splitting operation is reversed such that the
#' string segment following the matched RegEx pattern in `Regex_str` is retained in the column
#' `fromCol` while the string segment preceding that gets moved to the column `toCol`. If this
#' argument is used, then `extract_front_only` and `extract_back_only` should not be used.
#' @param extract_front_only Binary - optional. If `=1`, then only string segment preceding the matched
#' RegEx pattern in `Regex_str` is copied to the column `toCol`, while the column `fromCol`. If this
#' argument is used, then `rvrs_frontback` and `extract_back_only` should not be used.
#' remains untouched.
#' @param extract_back_only Binary - optional. If `=1`, then only string segment following the matched
#' RegEx pattern in `Regex_str` is copied to the column `toCol`, while the column `fromCol`. If this
#' argument is used, then `rvrs_frontback` and `extract_front_only` should not be used.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
split_strings <- function(DTname, fromCol, toCol, Regex_str, nchar_from_front=-1, nchar_from_back=-1,
                          #when =1, extract from, but do not modify `fromCol`
                          rvrs_frontback=-1, extract_front_only=0, extract_back_only=0) {
  DT_x <- get(DTname)

  # Stage One - Detect `Regex_str` pattern differently, based on user-selected option
  if (nchar_from_front>0) { posX <- regexpr(Regex_str, stringr::str_sub(DT_x[[fromCol]], 1, nchar_from_front), perl=T)
  } else if (nchar_from_back>0) { posX <- regexpr(Regex_str, stringr::str_sub(DT_x[[fromCol]], -nchar_from_back, -1), perl=T)
  # uses RegEx to find position of pattern detected in the string
  } else { posX <- regexpr(Regex_str, DT_x[[fromCol]], perl=T) }

  # Stage Two - Split the strings based on the Regex positions found
  if (nchar_from_back>0) {
    #and truncates the `fromCol` value where the Regex pattern is; move these digits to `toCol`
    for ( i in 1:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 ) {
      if (extract_front_only==1) { #exception 1 - extract front of string from, but leave `fromCol` untouched
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, -(nchar_from_back - posX[i] + 3))]
      } else if (extract_back_only==1) { #exception 2 - extract back of string from
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], -(nchar_from_back - posX[i] + 1), -1)]
      } else { #normal circumstances - split strings
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], -(nchar_from_back - posX[i] + 1), -1)]
        DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, -(nchar_from_back - posX[i] + 3))]
      }
    }
  } else { #default case: string splitting from the front of string
    for ( i in 1:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 ) {
      if (extract_front_only==1) { #exception 1 - extract front of string from `fromCol`, but leave `fromCol` untouched
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, posX[i]-1)]
      } else if (extract_back_only==1) { #exception 2 - extract back of string from
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], posX[i], -1)]
      } else if (rvrs_frontback==1) { #SPECIAL CASE;
        #truncates the `fromCol` values, in reverse, retains the back and moves the front to `toCol`
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, posX[i])]
        DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], posX[i]+1, -1)]
      } else { #normal circumstances - split strings
        DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], posX[i], -1)]
        DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, posX[i]-1)]
      }
    }
  }
  assign(DTname, DT_x, envir = parent.frame())
}


#' Split Character Strings if Record is Missing a Value
#'
#' This function, like `split_strings()`, separates the contents of a character string
#' into two columns using a Regular Expressions (RegEx) string pattern query. The difference
#' is that `split_strIfNA()` will do this only on select rows of data/records, conditional on
#' the missingness (`'NA'` value) in another specified column.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param ifNACol Character. Name of the column which should be checked for `'NA'` values. The splitting
#' of string will only happen conditional on the value in this column for a record being `'NA'`.
#' @param fromCol Character. Name of the column from which the string to be split is taken.
#' @param toCol Character. Name of the column to which the split-off string will be allocated.
#' @param Regex_str Character. Regular Expression (RegEx) pattern to be searched for within
#' character strings and, following that, to apply the operation onto.
#' @param rvrs_frontback Binary - optional. If `=1`, then the splitting operation is reversed such that the
#' string segment following the matched RegEx pattern in `Regex_str` is retained in the column
#' `fromCol` while the string segment preceding that gets moved to the column `toCol`.
#' @param nchar_from_back Numeric - optional. Number of characters from the back of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_front` should not be used.
#' @param nchar_from_front Numeric - optional. Number of characters from the front of
#' the character string to restrict the string operation to. If this argument is used,
#' then the argument `nchar_from_back` should not be used.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
#'
split_strIfNA <- function(DTname, ifNACol, fromCol, toCol, Regex_str, rvrs_frontback=-1,
                          nchar_from_front=-1, nchar_from_back=-1) {
  DT_x  <- get(DTname)
  ifCon <- paste0("(is.na(", DTname, "[i, ", ifNACol, "]))")

  if (nchar_from_front>0) { posX <- regexpr(Regex_str, stringr::str_sub(DT_x[[fromCol]], 1, nchar_from_front), perl=T)
  } else if (nchar_from_back>0) { posX <- regexpr(Regex_str, stringr::str_sub(DT_x[[fromCol]], -nchar_from_back, -1), perl=T)
  # uses RegEx to find position of pattern detected in the string
  } else { posX <- regexpr(Regex_str, DT_x[[fromCol]], perl=T) }

  if (nchar_from_back>0) {
    #and truncates the `fromCol` value where the digits start; move these digits to `toCol`
    for ( i in 1:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 & eval(parse(text=ifCon))==TRUE) {
      DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], -(nchar_from_back - posX[i] + 2), -1)]
      DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, -(nchar_from_back - posX[i] + 3))]
    }
  } else if (rvrs_frontback==1) { #SPECIAL CASE;
    #truncates the `fromCol` values, in reverse, retains the back and moves the front to `toCol`
    for ( i in 1:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 & eval(parse(text=ifCon))==TRUE) {
      DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, posX[i])]
      DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], posX[i]+1, -1)]
    }
  } else { #NORMAL CASE
    #and truncates the `fromCol` value where Regex is detected; move these digits to `toCol`
    for ( i in 1:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 & eval(parse(text=ifCon))==TRUE) {
      DT_x[i, eval(toCol) := stringr::str_sub(DT_x[[fromCol]][i], posX[i], -1)]
      DT_x[i, eval(fromCol) := stringr::str_sub(DT_x[[fromCol]][i], 1, posX[i]-1)]
    }
  }
  assign(DTname, DT_x, envir = parent.frame())
}


#' Replace or Remove String Pattern in Column(s) of Data
#'
#' This function either replaces all instances of, or completely removes (the default), a given
#' Regular Expressions (RegEx) string pattern query in one or two selected columns.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#' @param Regex_str Character. Regular Expression (RegEx) pattern to be searched for within
#' character strings and, following that, to apply the operation onto.
#' @param replOrPurge_str Character - optional. The string with which any instances of matches of
#' the RegEx pattern specified in `Regex_str` should be replaced. The default value is blank,
#' meaning that all instances of matches should be deleted.
#' @param vCol2 Character - optional. If desired, name of the second column within the R object
#' to be operated on in the same way as `vCol`.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
#'
replOrPurge_Col <- function(DTname, vCol1, Regex_str, replOrPurge_str="", vCol2=NA) {
  DT_x  <- get(DTname)

  DT_x[, eval(vCol1) := gsub(Regex_str, replOrPurge_str,  DT_x[[vCol1]], perl=T)]
  if (!is.na(vCol2)) { # repeats the process if a second column was inputted
    DT_x[, eval(vCol1) := gsub(Regex_str, replOrPurge_str,  DT_x[[vCol1]], perl=T)]
  }
  assign(DTname, DT_x, envir = parent.frame())
}

#' Conditionally Fill Cells with 'NA' Values
#'
#' This function replaces values within a column that are likely to not contain useful information:
#' they fulfil a condition relating to the length of string, i.e. number of characters, as specified
#' by the user. By default, any string values of length `nchar_thres` or _lower_ get replaced with `'NA'`
#' values. However, if `nchar_thres` is a negative number, any strings of length _above_ the absolute
#' value of `nchar_thres` will be replaced with `'NA'` instead.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vColRange Character or Numeric Range. If character, this refers to the name of the column
#' within the R object to be operated on. If a numeric range, this refers to the range of 'V-Columns'
#' that will be operated on, e.g. `2:5` refers to the columns `c("V2", "V3", "V4", "V5")`.
#' @param nchar_thres Numeric. If the number of characters in a string in the specified column range
#' is less than or equal to this number, the values inside them will be replaced with `'NA'`s. If
#' instead this value is negative, then `'NA'`s will replace cells whose character lengths exceed
#' the absolute value of this number. Default value is `=3`.
#' @param Remove_str Character - optional. Regular Expressions (RegEx) string pattern to search
#' for and remove before counting the number of characters in each string in the chosen column. This
#' value is blank by default, meaning no character in any string will be disregarded.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
#'
fillCells_withNA <- function(DTname, vColRange, nchar_thres=3, Remove_str=NA) {
  DT_x  <- get(DTname)

  #deals accordingly with the type of input for `vColRange`:
  #converts character into a list, if the `vColRange` is a character string
  if (is.character(vColRange)) { lsvCol <- c(vColRange)
  } else {#combines the list of concerned `V-rows` into a vector of column names
    lsvCol <- paste0("V", as.character(vColRange)) %>% c()
  }

  for(j in lsvCol) { #(if desired) remove chars. before counting nchar()
    if (!is.na(Remove_str)) {
      ls_j <- gsub(Remove_str, "",  DT_x[[j]], perl=T)
    } else ls_j <- DT_x[[j]]

    if (nchar_thres>=0) { #fill with NA if values are below a certain length
      data.table::set(DT_x, i = which(nchar(ls_j) <= nchar_thres), j=j, value=NA)
    } else if (nchar_thres<0) { #fill with NA if values are above a certain length
      data.table::set(DT_x, i = which(nchar(ls_j) >= -nchar_thres), j=j, value=NA)
    } }
  assign(DTname, DT_x, envir = parent.frame())
}


#' Delete Rows and/or Collapse Information across Rows using 'NA's
#'
#' This function either deletes (by default) or collapses rows into the rows above them,
#' conditional on the number of `'NA'` values that each row has among the columns specified in
#' `vColRange`. In the case of deletion, rows must have a number of columns not holding `'NA'` values
#' corresponding to the value of `export_thres` to not be deleted. In the case of collapsing of rows,
#'
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vColRange Character or Numeric Range. If character, this refers to the name of the column
#' within the R object to be operated on. If a numeric range, this refers to the range of 'V-Columns'
#' @param origCol Character - optional. If desired, the name of the column from which any character
#' string remaining in a row slated for deletion is to be transferred to the row above.
#' @param addtoCol Character - optional. If desired, the name of the column to which any character
#' string remaining in a row slated for deletion is to be transferred from the row below.
#' @param export_thres Integer. Number of permissible `'NA'` values per row of subscriber records,
#' out of the cells in the columns listed in `vColRange`. Default value `=1`.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
del_orCollapsebyNA <- function(DTname, vColRange, origCol=NA, addtoCol=NA, export_thres=1) {
  DT_x  <- get(DTname)

  #combines the list of concerned `V-rows` into a vector of column names
  lsvCol <- paste0("V", as.character(vColRange)) %>% c()
  lenV <- length(lsvCol)
  # Criteria :: counts the number of NAs in every row
  numNAs   <- rowSums(is.na(DT_x[, ..lsvCol]))

  # IF user specifies to do not only delete but also merge/collapsing rows,
  if (!is.na(origCol) & !is.na(addtoCol)){
    #reverse order implemented for "upwards merging" of strings
    for ( i in (nrow(DT_x)):2 ) if ( numNAs[i]==lenV & numNAs[i-1]<lenV ) {
      #checks if the row below numberless row records an occurrence of consecutive digits
      DT_x[i-1, eval(addtoCol) := paste0(DT_x[[addtoCol]][i-1], " ", DT_x[[origCol]][i])]
      #strangely, `paste` statt `paste0` generates a random "NA" string in bet. the strings
    }
  } #returns to parent frame (`env`) only rows with a permissible no. of NAs (`thres`)
  assign(DTname, DT_x[numNAs <= (lenV-export_thres), ], envir = parent.frame())
}


#' Replaces 'ditto' Strings and Equivalents
#'
#' This function searches for instances of 'ditto' or equivalent character strings in a given column
#' and replaces them with a surname from the most proximal row above the row with a 'ditto' string.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#' @param nchar_from_front Numeric - optional. Number of characters from the front of
#' the character string to restrict the string operation to. By default the value is `=6`.
#' @param Surname_str Character. First Regular Expression (RegEx) pattern to be searched for within
#' the chosen column `vCol`. This RegEx pattern should correspond to how the directory records
#' the given surname (or forename) of subscribers. By default the string is `"^.{0,2}[A-Z]{3,}(?=\\s)"`.
#' @param Ditto_str Character. Second Regular Expression (RegEx) pattern to be searched for within
#' the chosen column `vCol`. This RegEx pattern should correspond to the character strings that
#' possibly indicate the use of 'ditto'. By default the string is `"d[o\\.\\,]{2,4}|(d[o|\\.]\\s)"`.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column.
#' @export
repl_Dittos <- function(DTname, vCol, nchar_from_front=6, Surname_str="^.{0,2}[A-Z]{3,}(?=\\s)",
                        Ditto_str="d[o\\.\\,]{2,4}|(d[o|\\.]\\s)") {
  DT_x <- get(DTname)# by default, only from the first 6 chars.

  # uses RegEx (pre-defined) to find position of `do.,` or equiv. detected in the string
  posX <- regexpr(Ditto_str, stringr::str_sub(DT_x[[vCol]], 1, nchar_from_front), perl=T)
  # detects if strings in the given `vCol` contains >= 3 consecutive capitalised letters
  posSRNM <- regexpr(Surname_str, DT_x[[vCol]], perl=T)

  #
  for ( i in 2:nrow(DT_x) ) if ( !is.na(posX[i]) & posX[i]>0 ) {
    for ( j in (i-1):1 ) if ( !is.na(posSRNM[j]) & posSRNM[j] > 0 ) {
      surNAME <- stringr::str_sub(DT_x[[vCol]][j], posSRNM[j],
                                  posSRNM[j]-1+attr(posSRNM, "match.length")[j])
      DT_x[i, eval(vCol) := gsub(Ditto_str, surNAME, DT_x[[vCol]][i], perl=T)]
      # print(paste0("Replaced `do.,` in row ", i, " with ", surNAME," from row ", j))
      break
    }
  }
  assign(DTname, DT_x, envir = parent.frame())
}

