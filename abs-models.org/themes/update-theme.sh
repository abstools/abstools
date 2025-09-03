#!/bin/sh

rm -rf hugo-theme-relearn
git clone --depth=1 https://github.com/McShelby/hugo-theme-relearn
rm -rf hugo-theme-relearn/.git \
   hugo-theme-relearn/.githooks \
   hugo-theme-relearn/.github \
   hugo-theme-relearn/.vscode \
   hugo-theme-relearn/exampleSite \
   hugo-theme-relearn/vscode-frontmatter \
   hugo-theme-relearn/docs
