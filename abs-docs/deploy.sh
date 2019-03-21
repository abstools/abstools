#!/bin/bash
set -e

# Build and deploy the site to github -- see
# https://gohugo.io/hosting-and-deployment/hosting-on-github/

# link to repository added via `git submodule add -b master
# git@github.com:abstools/docs.abs-models.org.git docs.abs-models.org`

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

# Build the project.
../gradlew asciidoc

# Go To Public folder
cd site-deploy

# transfer build
rm -r *
cp -r ../build/asciidoc/html5/* .

# Add changes to git.
git add .

# Commit changes.
msg="rebuilding site `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

# Push source and build repos.
git push origin master

# Come Back up to the Project Root
cd ..
