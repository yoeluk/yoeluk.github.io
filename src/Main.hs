--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Pandoc.Options (writerHtml5)
import           BriefScala.Config


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

  let engineConf = defaultEngineConfiguration

  let writerOptions = defaultHakyllWriterOptions { writerHtml5 = True }

  let pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions

  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocHtml5Compiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls


  match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Home"                `mappend`
                  defaultContext

          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  match "lib/Font-Awesome/fonts/*" $ do
      route $ customRoute (combine "fonts" . takeFileName . toFilePath)
      compile copyFileCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
