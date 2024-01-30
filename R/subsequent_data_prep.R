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


#' Ch
#'
#' N.B. Th
#'
#' @import parallel
#' @importFrom stringr str_sub
#' @param DTname Cha ract
#' @export
serialise_recIDs <- function(DTname=NA, DTlocation="NONE", dirNM=NA) {
  # prints notification messages corresponding to chosen operation
  if (sum(is.na(DTname), is.na(dirNM)) %in% c(0,2)) { 
    print("Exactly ONE of the following arguments should be defined: `DTname` or `dirNM`")
  } else if (!is.na(DTname)) { # I: operates on object in R environment
    #extracts directory year and location for creation of record IDs
    dirYR <- get(DTname) %>% .[1, "DirName"] %>% gsub("^.*_(?=1[89])", "", ., perl = T) %>% 
      str_sub(2, 4) 
    
    DTlocation %>% { if (. == "ALL") 0 
      else if (. == "LDN") 1
      else if (. == "GLA") 2 
      else if (. == "MAN") 3 #adds a leading zero if needed
      else 77                } %>% sprintf("%02d", .) -> dirCDE
    
    #creates unique ID column { (YYYY Year) (LL Location Code) (IIIIIII ID) }
    get(DTname) %>% .[, ID := as.numeric(paste0(dirYR, dirCDE, sprintf("%07d", .I)))]
    print( summary( get(DTname)[, "ID"])) #confirmation of correct attribution of IDs
    
    # assign(DTname, DT_x, envir = parent.frame())
  } else if (!is.na(dirNM)) { # II: operates on file stored as .csv/.xlsx
    readin_adrDT(dirNM, "tld") #imports chosen telephone directory
    tld <<- tld #exports the table of records as global variable in R env.
    
    #extracts directory year and location for creation of record IDs
    dirYR <- tld[1, "DirName"] %>% gsub("^.*_(?=1[89])", "", ., perl = T) %>% 
      str_sub(2, 4)
    #derives component of record ID to use based on `dirNM`
    dirNM %>% gsub("^.*[\\_\\.](?=[A-Z]{3})", "", ., perl = T) %>%
      #creates code based on directory location
      str_sub(1, 3) %>% { if (. == "ALL") 0 
        else if (. == "LDN") 1
        else if (. == "GLA") 2 
        else if (. == "MAN") 3 #adds a leading zero if needed
        else 77                } %>% sprintf("%02d", .) -> dirCDE
    
    #creates unique ID column { (YYYY Year) (LL Location Code) (IIIIIII ID) }
    tld[, ID := as.numeric(paste0(dirYR, dirCDE, sprintf("%07d", .I)))]
    
    #re-exports data table; keeps same file name as the original file
    dirNM %>% gsub("^.*\\.(?=[0-9]\\_[A-Z]{3})", "", ., perl = T) %>%
      gsub("(?<=[A-Za-z]{2})\\.[0-9]{6}\\_[0-9]{4}(\\.xlsx|\\.csv)?$", "", ., perl = T) -> yx
      
    print(yx)
    export_fullDT("tld", xpNAME=yx)
    
    print( summary(tld$ID)) #confirmation of correct attribution of IDs
  }
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



#' Geocoding (Fuzzy matches two string fields)
#'
#' N.B. This currently works with `%do%` in the package but not with `%dopar%` (which only works
#' outside the package, if the function is loaded into global environment!)
#' This function is used primarily for geocoding. How it exactly works is by fuzzy
#' matching two columns/fields that each contain character strings (of address street names).
#' 
#' @import parallel
#' @import doParallel
#' @importFrom foreach foreach %do% %dopar%
#' @param dt1 Data Table. Name of object in the global environment which corresponds
#' to the records of addresses that are to be geocoded.
#' @param dt2 Data Table. Name of object in the global environment which corresponds
#' to the database of already geocoded addresses. The default used was the OS AddressBase
#' from the Ordnance Survey.
#' @param col1 Character. Name of field in the data table `dt1` which contains the street
#' names of the addresses and which is to be used for geocoding.
#' @param col2 Character. Name of field in the data table `dt2` which contains the street
#' names of the already geocoded addresses.
#' @param xtr1 Vector of characters. Names of additional fields of information from `dt1` (other
#' than `col1`) which the user wishes to retain in the final output of the function.
#' @param xtr2 Vector of characters. Names of additional fields of information from `dt2`(other
#' than `col2`) which the user wishes to retain in the final output of the function.
#' @return The same object in the global environment (`dt1`) with additional fields appended
#' to it following the process of geocoding.
#' @export
match_strDist <- function(dt1, dt2, col1, col2, xtr1, xtr2) {
  strWGHT <- c(d = 1, i = 1, s = 1, t = 1) #del, ins, sub, trp.
  .GlobalEnv$col1 <- col1; .GlobalEnv$col2 <- col2
  
  #combines the optional arguments (xtr1,2) with required arg. (if applicable)
  if (!is.null(xtr2)) { lst2 <- append(append(col2, xtr2), "regdist") 
  } else {lst2 <- append(col2, "regdist")}
  # DT1 :: Rows to be matched; creates unique row identifier column (ID) in this data.table
  dt1[, ID := .I] #equivalent of dt1 %>% mutate(id = row_number())
  # DT2 :: Reference data table of addresses - from OS AddressBase; Extracts variable(s) to be used
  if (!is.null(xtr2)) { ref <- dt2[, ..lst2] } else { ref <- dt2[, ..col2]}
  
  
  ### PHASE II: Geo-codes Addresses without a defined Telephone Exchange Area OR bad phase I results
  #DT2 - no need to subset 'ref' data.table because we will use all entries
  
  #iterates string matching of addresses from dt1, row by row - to save memory (?)
  foreach (i=1:nrow(dt1), .export=c("ref", "col1", "col2", "strWGHT"), 
           .combine='rbind', .packages = c("data.table", "stringdist")) %do% {
             #replicate the same addr. string to be matched, as many times as the address ref. database
             mch <- rep(dt1[i, ..col1], times = nrow(ref))
             ref$dst <- #calculates string distance (of chosen distance measure)
               stringdist( as.character(mch), as.character(ref[[col2]]), 
                           method="osa", weight = strWGHT) 
             # #extracts the first row with lowest string dist., then append row ID to it
             # row <- ref[ , .SD[which.min(dst)]][, ID := dt1$ID[i]][, mch_flag := 2]
             
             # subsets to keep only entries with minimum string distance; checks for matching street_num
             ref_t <- ref[ ref[, .I[dst == min(dst)]],][, mch_flag := 20] #street-name match
             ref_t$mch_flag[ref_t$adb_num==dt1[i, tld_num] & ref_t$dst<4] <- 21 #str. name + num match
             #somehow #if (!is.na(dt1$tld_num[i])) doesn't work!!!!
             row <- ref_t[ , .SD[which.max(mch_flag)]][, ID := dt1$ID[i]]
             
             # and finally, appends these individual rows to the `xpt` rows from Phase I
           } -> xpt # %>% rbind(xpt, .) 
  
  #reassigns the mch_flag to `=99` if the address returns no matches (`NA`)
  xpt[is.na(dst) | dst>10, mch_flag := 99]
  
  ### Phase Final: Export Phase
  #defines columns to be exported: string dist. ("dst") and the original vars.
  xcol <- append(c("dst", "mch_flag"), lst2)
  #left join - merges on dt1, using the index 'ID' from xpt to search for rows
  #carries over all columns defined in 'xcol' from xpt to dt1
  dt1[xpt, on = 'ID', (xcol) := mget(paste0("i.", xcol))]
  return(dt1)
} 
