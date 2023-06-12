# Telephone Directories Processor

## Overview
`teldiR` was specifically designed to help process unstructured text into tables of information that are easily analysed. With some customisation, it could potentially be applied to datasets other than Optical Character Recognition (OCR) scans of telephone directories.

## Installing teldiR
#### Installation through GitHub
teldiR is not distributed through CRAN, so you will have to install it directly from Github using the `devtools` package in R. If you do not have the devtools package installed, you will have to run the first line in the code below as well.

```{R}
# install.packages('devtools') #needed only if not already installed
devtools::install_github('kinatou/teldiR')
```
#### Local Installation
If the above steps do not work, the alternative is to do a local installation. 
1. Download the files of the entire repository, placing them in a master folder called 'teldiR'
2. In R, install `teldiR` from your local directory with the code shown below.

```{R}
# install.packages('devtools') #needed only if not already installed
devtools::install("file-path/to-folder-of/teldiR")
```

## Other Notes
Many functions in `teldiR` are equipped with default settings that reference a specific folder structure. Many of these file path references can be changed, but should it be deemed more convenient, the relevant folder structure can be seen in the [BTPhoneDirectories](https://github.com/ESRC-CDRC/BTPhoneDirectories) repository.
