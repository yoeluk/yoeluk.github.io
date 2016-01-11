--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Control.Monad (filterM)
import           Data.List (intersperse, isSuffixOf)
import           Data.List.Split (splitOn)
import           BriefScala.Tags
import           Text.Pandoc.Options (writerHtml5)
import           System.FilePath (combine, splitExtension, takeFileName)
import           BriefScala.Config


--------------------------------------------------------------------------------
siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "https://briefscala.com"
  , siteGaId = "NA-NA"
  }

feedConf :: String -> FeedConfiguration
feedConf title = FeedConfiguration
  { feedTitle = "briefscala.com: " ++ title
  , feedDescription = "A Scala blog"
  , feedAuthorName = "Yoel Garcia"
  , feedAuthorEmail = "yoeluk@gmail.com"
  , feedRoot = "https://briefscala.com"
  }

main :: IO ()
main = hakyll $ do
  let engineConf = defaultEngineConfiguration
  let writerOptions = defaultHakyllWriterOptions { writerHtml5 = True }

  let pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions

  tags <- buildTags "content/posts/*" (fromCapture "tags/*/index.html")

  let postTagsCtx = postCtx tags

  match "content/tmp/*" $ do
    route stripContent
    compile copyFileCompiler

  match "images/*.png" $ do
    route idRoute
    compile copyFileCompiler

  match "libs/FontAwesome/fonts/*" $ do
    route $ customRoute (combine "fonts" . takeFileName . toFilePath)
    compile copyFileCompiler

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

  match "content/about/index.md" $ do
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/about.html"  siteCtx
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls
      >>= deIndexUrls

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

  match "content/posts/*" $ do
    route $ directorizeDate `composeRoutes` stripContent `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocHtml5Compiler
      full <- loadAndApplyTemplate "templates/post.html" postTagsCtx compiled
      teaser <- loadAndApplyTemplate "templates/post-teaser.html" postTagsCtx $ dropMore compiled
      _ <- saveSnapshot "content" full
      _ <- saveSnapshot "teaser" teaser
      loadAndApplyTemplate "templates/default.html" (postCtx tags) full
        >>= relativizeUrls
        >>= deIndexUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList tags recentFirst) `mappend`
            constField "title" "Archives" `mappend` siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "content/index.html" $ do
    route stripContent
    compile $ do
      tpl <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody
      loadAllSnapshots "content/posts/*" "teaser"
        >>= fmap (take 100) . recentFirst
        >>= applyTemplateList tpl (postCtx tags)
        >>= makeItem
        >>= applyTemplate body (siteCtx `mappend` bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      posts <- mapM deIndexUrls =<< fmap (take 10) . recentFirst =<<
        loadAllSnapshots "content/posts/*" "content"
      renderAtom (feedConf "blog") feedCtx (posts)

  match "templates/*" $ compile templateCompiler
  match "templates/*/*" $ compile templateCompiler

siteCtx :: Context String
siteCtx =
  deIndexedUrlField "url" `mappend`
  constField "root" (siteRoot siteConf) `mappend`
  constField "gaId" (siteGaId siteConf) `mappend`
  constField "feedTitle" "Posts" `mappend`
  constField "feedUrl" "/atom.xml" `mappend`
  constField "gMapsApiScript" "" `mappend`
  defaultContext

postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%e %B %Y" `mappend`
  dateField "datetime" "%Y-%m-%d" `mappend`
  (tagsFieldWith' getTags) "tags" tags `mappend`
  siteCtx

postList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags sortFilter = do
  posts <- sortFilter =<< loadAll "content/posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl (postCtx tags) posts
  return list

stripContent :: Routes
stripContent = gsubRoute "content/" $ const ""

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = dirs ++ "/index" ++ ext
      where
        (dirs, ext) = splitExtension $ concat $
          (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        (date, rest) = splitAt 3 $ splitOn "-" path

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) ("/." :: String)
  then take (length url - 10) url else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key
  $ fmap (stripIndex . maybe mempty toUrl) . getRoute . itemIdentifier

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)
