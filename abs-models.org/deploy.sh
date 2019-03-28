#!/bin/bash
set -e
# Build and deploy the site to github -- see
# https://gohugo.io/hosting-and-deployment/hosting-on-github/

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

# Pull from upstream, just in case we’re behind
(cd public ; rm -rf * ; git checkout . ; git pull)

# Build the project.
hugo

# Go To Public folder
cd public
# Add changes to git.
git add .

# Commit changes.
msg="rebuilding site `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

# Push into deploy repository
git push origin master

# Come Back up to the Project Root
cd ..
