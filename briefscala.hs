#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
  _ <- shell "git checkout hakyll" empty
  _ <- shell "git submodule init" empty
  _ <- shell "git submodule update" empty
  _ <- shell "cd _site/" empty
  _ <- shell "git checkout master" empty
  _ <- shell "cd ../" empty
  _ <- shell "cabal run build" empty

  _ <- shell "cd _site/" empty
  _ <- shell "git status" empty
  _ <- shell "git add --all" empty
  _ <- shell "git commit -m \"Update (`date '+%F %T %Z'`) [ci skip]\"" empty
  _ <- shell "git push origin master" empty

  _ <- shell "cd ../" empty
  _ <- shell "git status" empty
  _ <- shell "git add _site" empty
  _ <- shell "git commit -m \"Update _site (`date '+%F %T %Z'`) [ci skip]\"" empty
  final <- shell "git push origin hakyll" empty
  case final of
      ExitSuccess   -> return ()
      ExitFailure n -> die ("updateSite " <> " failed with exit code: " <> repr n)
