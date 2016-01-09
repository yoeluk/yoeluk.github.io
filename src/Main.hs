--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Pandoc.Options (writerHtml5)
import           System.FilePath (combine, splitExtension, takeFileName)
import           BriefScala.Config


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConf $ do

  let engineConf = defaultEngineConfiguration

  let writerOptions = defaultHakyllWriterOptions { writerHtml5 = True }

  let pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions

  tags <- buildTags "posts/*" (fromCapture "tags/*/index.html")

  let postTagsCtx = postCtx tags

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

  match "extra/*" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList $ vendorScriptFiles engineConf) $ do
    route $ customRoute (combine "js/vendor" . takeFileName . toFilePath)
    compile copyFileCompiler

  match (fromList $ lessFiles engineConf) $ do
    route $ setExtension "css"
    compile $ getResourceString
      >>= withItemBody
        (unixFilter (lessCommand engineConf) $ "-" : (lessOptions engineConf))


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

  match "lib/FontAwesome/fonts/*" $ do
      route $ customRoute (combine "fonts" . takeFileName . toFilePath)
      compile copyFileCompiler

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route idRoute
    compile $ do
      list <- postList tags (\t -> recentFirst t >>= filterM (fmap (elem tag) . getTags . itemIdentifier))
      let ctx =
            constField "tag" tag `mappend`
            constField "posts" list `mappend`
            constField "feedTitle" title `mappend`
            constField "title" title `mappend`
            constField "feedUrl" ("/tags/" ++ tag ++ "/index.xml") `mappend`
            siteCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

    version "rss" $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      route $ setExtension "xml"
      compile $ loadAllSnapshots pattern "content"
        >>= fmap (take 10) . recentFirst
        >>= renderAtom (feedConf title) feedCtx


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
