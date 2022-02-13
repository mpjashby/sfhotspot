## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is a resubmission after being asked to change \dontrun{} in some of the
  examples to \donttest{}, which I have done. I have also added references to
  papers discussing the techniques underlying functions in this package.
* The three possibly misspelled words in the DESCRIPTION file that are 
  highlighted by R CMD check are names of the authors of papers.
* When running R CMD check on Windows via R-hub (but on no other system) I 
  intermittently get a note of a detritus file/directory 'lastMiKTeXException'. 
  I have ignored this as recommended in Uwe Ligges's post on R-pkg-devel at 
  https://www.mail-archive.com/r-package-devel@r-project.org/msg07564.html
