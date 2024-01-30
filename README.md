# Telephone Directories Processor

## Overview
`teldiR` was specifically designed to help process unstructured text into tables of information that are easily analysed. It had been primarily designed to process the archive of historical telephone directory scans from British Telecommunications; the functions contained therein were used mainly on a series of .txt text files that were the outputs of Optical Character Recognition (OCR) done prior. With some customisation, it could potentially be applied to datasets, either historical and/or less structured, other than these scans of telephone directories.

## Installing teldiR
There are two options for installing the `teldiR` package to your local installation of R.

#### 1. Installation through GitHub
teldiR is not distributed through CRAN, so you will have to install it directly from Github using the `devtools` package in R. If you do not have `devtools`  installed, you will have to run the first line in the code below as well.

```{r}
# install.packages('devtools') #if not already installed
devtools::install_github('kinatou/teldiR')
```
#### 2. Local Installation
If the above steps do not work, the alternative is to do a local installation. 
1. Download the files of the entire repository, placing them in a master folder anywhere on your computer and calling this folder 'teldiR'
2. In R, install `teldiR` from your local directory with the code shown below.

```{r}
# install.packages('devtools') #if not already installed
devtools::install("file-path/to-folder-of/teldiR")
```

## Other Notes
<<<<<<< HEAD
Many functions in `teldiR` are equipped with default settings that reference a specific folder structure. Many of these file path references can be changed, but should it be deemed more convenient, the relevant folder structure can be seen in the [BTPhoneDirectories](https://github.com/ESRC-CDRC/BTPhoneDirectories) repository.
# testerrR
=======
The R package installation has documentation built-in. For every function contained in `teldiR`, you may type `?name_of_function` in the console to pull out its corresponding documentation and description of how to use it.

Many functions in `teldiR` are equipped with default settings that reference a specific folder structure. Many of these file path references can be changed by specifying certain arguments, but should it be deemed more convenient, the relevant folder structure can be seen in the [BTPhoneDirectories](https://github.com/ESRC-CDRC/BTPhoneDirectories) repository.
>>>>>>> b5df26fc8eecfacd0c9325d7b40857aee36cc03e
# teldiR
