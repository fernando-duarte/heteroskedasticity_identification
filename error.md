New submission
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘hetid’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... NOTE
unable to verify current time
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... NOTE
Non-standard files/directories found at top level:
  ‘CI_CD_SUCCESS_SUMMARY.md’ ‘FINAL_DOCKER_FIXES.md’
  ‘PR_FINAL_FIXES.md’ ‘PR_REVIEW_FIXES.md’ ‘WORKFLOW_FIXES_SUMMARY.md’
  ‘WORKFLOW_STATUS_SUMMARY.md’ ‘test_workflows.sh’ ‘tmux.md’
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking code files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
 WARNING
‘qpdf’ is needed for checks on size reduction of PDFs
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’ [61s/30s]
 [61s/30s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* checking PDF version of manual ... WARNING
LaTeX errors when creating PDF version.
This typically indicates Rd problems.
* checking PDF version of manual without index ... ERROR
Re-running with no redirection of stdout/stderr.
Hmm ... looks like a package
Converting parsed Rd's to LaTeX ....
Creating pdf output from LaTeX ...
Warning in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
  texi2dvi script/program not available, using emulation
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
  pdflatex is not available
Warning in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
  texi2dvi script/program not available, using emulation
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
  pdflatex is not available
Error in running tools::texi2pdf()
You may want to clean up by 'rm -Rf /tmp/RtmpgejkIS/Rd2pdf146148f21c2'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ‘hetid-manual.tex’
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 ERROR, 2 WARNINGs, 5 NOTEs
See
  ‘/workspace/hetid.Rcheck/00check.log’
for details.

Error: Process completed with exit code 1.
