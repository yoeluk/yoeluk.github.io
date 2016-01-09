#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
  _ <- shell "git submodule update --init" empty
  _ <- shell "cd _site/ && git checkout master" empty
  _ <- shell "cabal run build" empty

  _ <- shell "cd _site/ && git status" empty
  _ <- shell "cd _site/ && git add --all" empty
  _ <- shell "cd _site/ && git commit -m \"Update (`date '+%F %T %Z'`) [ci skip]\"" empty
  _ <- shell "cd _site/ && git push origin master" empty

  _ <- shell "git status" empty
  _ <- shell "git add _site" empty
  _ <- shell "git commit -m \"Update _site (`date '+%F %T %Z'`) [ci skip]\"" empty
  final <- shell "git push origin hakyll" empty
  case final of
      ExitSuccess   -> return ()
      ExitFailure n -> die ("updateSite" <> " failed with exit code: " <> repr n)
