#' Creates One or More Empty Columns
#'
#' This function creates one or more empty columns in a given data table or data frame.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param newcols Vector of character strings. Names of empty column(s) to be created, of type 'character'.
#'
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
create_newCols <- function(DTname="tld", newcols=c("tld_num", "tld_street")){
  DT_x  <- get(DTname)
  for (colT in newcols) { DT_x[, colT] <- character() }
  #returns to parent frame (`env`)
  assign(DTname, DT_x, envir = parent.frame())
}


#' Distinguish Business and Residential Subscribers
#'
#' This function creates a new column `isBusiness`, if not already existing. It then uses a list
#' of pre-defined strings stored in the package to infer if a particular telephone directory record
#' belongs to a commercial (business) or personal (residential) subscriber.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#'
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column(s).
#' @export
detect_isBusy <- function(DTname="tld", vCol) {
  DT_x  <- get(DTname) #creates new column
  #creates the `isBusiness` column, if needed
  if (!"isBusiness" %in% colnames(DT_x)) { DT_x[, isBusiness := integer()]
    print("A new column which was needed and absent, `isBusiness`, was created.")}
  #assign a binary value to indicate `isBusiness`
  DT_x[, isBusiness :=ifelse( stringr::str_detect(DT_x[[vCol]], lstBZNME), 1, 0)]
  assign(DTname, DT_x, envir = parent.frame())
}


#' Replace Abbreviations of Throughfare Names with their Long-forms
#'
#' This function searches for abbreviated forms of throughfare names in a column containing street
#' addresses (e.g. lane, street, road...) and replaces them with their long-forms. By default this
#' function searches for them at the end of character strings, with an optional allowance of a RegEx
#' string, `Regex_allowance`, that the user can choose.
#'
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param vCol Character. Name of the column within the R object to be operated on.
#' @param Regex_allowance Character - optional. Regular Expression (RegEx) pattern allowance for
#' character strings that are allowed to exist after the abbreviated throughfare names, but before
#' the end of the string. By default, the allowance is a single space.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column.
#' @export
repl_streetAbbrevs <- function(DTname="tld", vCol, Regex_allowance="\\s?") {
  # N.B. `Regex_allowance`is how lenient you want to be with the string detection @ the end of string

  DT_x  <- get(DTname)
  abbrVAT_frt <- paste0("^", dtABREV[["abbrev_rgx"]]) # at the beginning of strings, or
  abbrVAT_bck <- paste0(dtABREV[["abbrev_rgx"]], paste0(Regex_allowance, "$")) # at their ends

  for ( i in 1:length(abbrVAT_frt) ) { #replaces abbreviations listed above with full forms
    DT_x[, eval(vCol) := gsub(abbrVAT_frt[i], dtABREV[i, "full_name"], DT_x[[vCol]], perl=T)]
    DT_x[, eval(vCol) := gsub(abbrVAT_bck[i], dtABREV[i, "full_name"], DT_x[[vCol]], perl=T)]
  } #returns to parent frame (`env`)
  assign(DTname, DT_x, envir = parent.frame())
}


#' Checks and Corrects List of Surnames
#'
#' N.B. This currently works with `%do%` in the package but not with `%dopar%` (which only works
#' outside the package, if the function is loaded into global environment!)
#' This function matches uses fuzzy string matching surnames contained in  one column of a data frame 
#' or data table against another list of (standardised) surnames that the user provides. If a surname
#' from the former data table is found to closely resemble one from the latter table (as defined by 
#' `maxDist`), the value from the latter is appended to the former.
#'
#' @import parallel
#' @import doParallel
#' @importFrom foreach foreach %do% %dopar%
#' @param DTname Character. Name of the R object (a table) on which the operation is to be run.
#' @param DTrefName Character. Name of the R object (a table) which serves as the reference table,
#' against which surnames from `DTname` are to be fuzzy matched.
#' @param maxDist Numeric - optional. Maximum string distance between two character strings (of surnames)
#' for them to be considered a match to one another.
#' @return The same object in the global environment (a table) after the string operation
#' has been applied to the relevant column.
#' @export
proofcheck_Surnames <- function(DTname="tld", DTrefName="srn", maxDist=1) {
  DT_x  <- get(DTname) #OR eval(DTname) if `DTname` is entered without parentheses
  srn   <- get(DTrefName)
  
  # Safeguards: make sure `sbc_surname1` exists, and is of type `string`
  if (!"sbc_surname1" %in% colnames(DT_x)) { DT_x[, sbc_surname1 := character()] }
  else if (class(DT_x[["sbc_surname1"]]) != "character") { DT_x[, sbc_surname1:=NULL]
    DT_x[, sbc_surname1 := character()] }
  
  # #filters out the census surnames that start with a matching first alphabet to entries
  # cen_s <- srn[stringr::str_sub(sname_clean_stand, 1, 1) %in% unique(stringr::str_sub(DT_x[["sbc_surname"]], 1, 1)) |
  #                stringr::str_sub(sname_clean_stand, 2, 2) %in% unique(stringr::str_sub(DT_x[["sbc_surname"]], 1, 1)) |
  #                stringr::str_sub(sname_clean_stand, 3, 3) %in% unique(stringr::str_sub(DT_x[["sbc_surname"]], 1, 1)) ,]
  
  ### Searches for similar pairs bet. census and TelDir. subscriber surnames
  foreach (sur = setdiff(unique(DT_x[["sbc_surname"]]), NA), .combine=rbind,
           .packages = c("data.table", "stringdist")) %do% { #  --> not needed if using `::`s
             #creates matching condition; binary indicator of whether inter-string dist. <= 1
             sur_mch <- stringdist::amatch(sur, srn[["sname_clean_stand"]], method="osa", maxDist=maxDist)
             if ( !is.na(sur_mch) ) { data.table::data.table(ori_surname = sur, cor_surname = as.character(srn[sur_mch]))
             } else { data.table::data.table(ori_surname = character(), cor_surname = character()) }
             # N.B. Old method below doesn't work because each %dopar% instance is separate,
             # and so the iterations cannot all overwrite the same `DT_x`!
             # DT_x[!is.na(sur_mch) & sbc_surname==sur, sbc_surname1 := srn[sur_mch, "sname_clean_stand"]]
           } -> DT_mch
  
  for (i in 1:nrow(DT_mch)) { #use `DT_mch` created above to fill `sbc_surname1`
    DT_x[sbc_surname == DT_mch$ori_surname[i], sbc_surname1 := DT_mch$cor_surname[i]]
  } # N.B. `for` > `foreach %do%` here as every iteration is changing the same single `DT_x`
  assign(DTname, DT_x, envir = parent.frame()) #returns to parent frame (`env`)
}
