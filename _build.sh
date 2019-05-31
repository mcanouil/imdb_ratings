#!/bin/sh

Rscript -e 'rmarkdown::render("README.Rmd",  encoding = "UTF-8");'

git add --all
git -c "user.name=Mickaël Canouil" -c "mickael.canouil@cnrs.fr" commit -m "build update"
