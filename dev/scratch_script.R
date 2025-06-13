# ---- header ----
# 
# author: sheila saia
# date created: 2025-05-10
# email: sheila.saia@tetratech.com
#
# script name: scratch_script.R
# 
# 
# script description: 
# 
#
# ---- notes: ----
#
#
#
# ---- to do: ----
#
#
#
# ---- load libraries ----
library(tidyverse)
library(here)
library(fs)
library(golem)
library(usethis)


# ---- golem project setup ----

# 1. cloned empty repo from git

# 2. run git init in command line

# 3. added gitignore and scratch folder (ignored scratch folder for now)

# 4. run the code below
proj_path <- fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/Documents/github/TADAShinyAnalyze")
golem::create_golem(proj_path, overwrite = TRUE)

# 5. add readme, contributing, and license files
# used command line to do this but you can add them with usethis::use_readme_rmd(open = FALSE)
# there several other usethis functions to make license files, etc.

# 6. set options in description file and other setup files
golem::set_golem_options()
fs::dir_tree()

# 