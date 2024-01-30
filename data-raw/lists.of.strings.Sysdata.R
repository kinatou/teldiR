## README: https://r-pkgs.org/data.html#sec-data-sysdata

#loads necessary R packages
lapply( c("data.table", "dplyr"),
        require, character.only=TRUE)

# [LDN] list of postcodes - for extraction into own `V` column or deletion
ldnPCDE  <- c("E\\.C", "H\\.C", "B\\.C",
              "W\\.C",
              "N\\.W",
              "S\\.W", "8\\.W", "S\\. W",
              "S\\.E", "8\\.E",
              " E", " W", " N", " S" #N.B. \\s denotes a space - alternatively, "\\sE"
              #N.B. You have to escape the \ since it is itself an escape character.
              #N.B. You HAVE to escape '.' because it otherwise means 'any character'
) %>% paste0(., collapse="\\.|") %>% paste0(., "\\.(?!C\\.|W\\.|N\\.|S\\.|E\\.)") #length-sensitive variant #matches starting character
#N.B. negative look-behind needed to prevent matching of e.g. "E." in "E.C."

# [LDN] list of Telephone Exchange names (correct for 1911) for removal when carried over
ldnTELX <- c("Avenue", "Balham", "Battersea", "Be?c?ke?nha?m", "Brixton", "Central", "Croydon",
             "P\\.O\\..*ydon",  "Dalston", "Ealing", "East", "P\\.O\\..*chle?y", "Fulham", "Gerrard",
             "P\\.O\\..*std", "Holborn", "Hop", "London.*all", "Kensington", "Kingston", "Le?wi?sha?m",
             "Mayfair", "Paddington", "Putney", "Richmond", "Sydenham", "Vic.*ria", "Wa?nste?a?d", "Wa?ndswo?r?t?h"
             # .* : "0 or more of anything" - used for contractions/misspellings
) %>% paste0(., collapse=".{0,3}$|\\s*") %>% paste0("\\s*", ., ".{0,3}$")

# imports list of words which indicate a subscription is commercial in nature
dtBZNME <- data.table ( #../../../
  openxlsx::read.xlsx("1.Text.Procs/3.Post.Proc/common/0.isBiz.Names.Detector.xlsx", colNames=T)
) #and strings vector elements together into list
lstBZNME <- dtBZNME %>% pull(., "keyword_rgx") %>% paste0(., collapse="|")
### N.B. due to how `pull()` works, escapes `\` written in .xlsx should not be doubled

# imports table of list of throughfare abbreviations and their long-forms
dtABREV <- data.table ( #../../../
  openxlsx::read.xlsx("1.Text.Procs/3.Post.Proc/common/0.Throughway.Names.Abbrevs.xlsx", colNames=T) 
  )[, c("full_name", "abbrev_rgx")]

#strings vector elements together into list
lstABRV <- dtABREV %>% pull(., "abbrev_rgx") %>% paste0(., collapse="|")
#for detecting street names left ..?
lstTOPO <- dtABREV %>% pull(., "full_name") %>% paste0(., collapse="|")
#for detecting position of space before the topoNYM elements --> resolution of 'Ditto's
lfwTOPO  <- dtABREV %>% pull(., "full_name") %>% paste0(., collapse="|[^\\s]+-") %>%
  paste0("[^\\s]+-", .)



setwd("1.Text.Procs/0.R.Packages/teldiR") #navigates to root directory of teldiR pkg
#exports/ writes the objects needed for the functions of the package into `sysdata.rda`
usethis::use_data(ldnPCDE, ldnTELX, dtBZNME, lstBZNME, dtABREV, lstABRV, lfwTOPO,
                  #objects are for internal (i.e. developer) use; not shown to pkg users
                  overwrite=T, internal=T)

#exports/ writes objects for EXTERNAL use that users can also see (not only devs.)
usethis::use_data(lstABRV, lstTOPO, overwrite=T, internal=F)
setwd("../../..") #returns the working directory back to the root folder



