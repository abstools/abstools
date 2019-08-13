#!/bin/bash
set -e

# https://stackoverflow.com/questions/5143795/how-can-i-check-in-a-bash-script-if-my-local-git-repository-has-changes
echo -ne "\033[0;32mChecking for local changes...\033[0m"
# if necessary, change to `git status --porcelain --untracked-files=no`
if [[ `git status --porcelain` ]]; then
    echo
    git status
    echo -e "\033[0;32mUncommitted changes found, aborting $0\033[0m"
    exit 1
else
    echo "all good"
fi


# https://stackoverflow.com/questions/3258243/check-if-pull-needed-in-git
echo -ne "\033[0;32mChecking for unmerged upstream changes...\033[0m"

git fetch

LOCAL=$(git rev-parse '@')
REMOTE=$(git rev-parse '@{u}')
BASE=$(git merge-base '@' '@{u}')
if [ $LOCAL = $REMOTE ]; then
    echo "local and upstream are the same, all good"
elif [ $LOCAL = $BASE ]; then
    echo "please merge upstream changes"
    exit 1
elif [ $REMOTE = $BASE ]; then
    echo "local is ahead of upstream, will push"
else
    echo "local and upstream diverged, please resolve"
    exit 1
fi


# Build and deploy the site to github -- see
# https://gohugo.io/hosting-and-deployment/hosting-on-github/

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

# For submodule information, see
# https://git-scm.com/book/en/v2/Git-Tools-Submodules

# Pull from upstream, just in case we’re behind
git submodule update --rebase --remote public

# Build the project.
hugo

# Commit generated html
cd public
git add .
msg="rebuilding site `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"
cd ..

git add .
git commit -m "$msg"

git push --recurse-submodules=on-demand

