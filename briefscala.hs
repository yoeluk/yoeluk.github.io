#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do

  shell "git submodule init" empty
  shell "git submodule update" empty
  shell "cd _site/" empty
  shell "git checkout master" empty
  shell "cd ../" empty

  shell "git status" empty
  shell "git add --all" empty
  shell "git commit -m \"Update _site (`date '+%F %T %Z'`) [ci skip]\"" empty

  shell "cabal run build" empty

  shell "git push origin hakyll" empty

  shell "cd _site/" empty
  shell "git status" empty
  shell "git add --all" empty
  shell "git commit -m \"Update (`date '+%F %T %Z'`) [ci skip]\"" empty
  shell "git push origin master" empty
