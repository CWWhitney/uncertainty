devtools::release()

## Test environments
* local OS Monterey 12.3.1, R.Version R 4.2.0 

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs
We ran devtools::document() 
and 
checked the package with devtools::check()
All is well

We checked spelling with devtools::spell_check()
All is fine

We checked R-hub with devtools::check_rhub()
All is great

checked win-builder with devtools::check_win_devel()
All is ok

## Downstream dependencies
We used the devtools::install_github("r-lib/revdepcheck") to run R CMD check with revdepcheck::revdep_check() on downstream dependencies 
All packages passed 
