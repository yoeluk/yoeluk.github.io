{-# LANGUAGE OverloadedStrings #-}
module BriefScala.Tags where
import Data.List (intercalate, intersperse)
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Hakyll
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderSimpleTagList :: Tags -> Compiler String
renderSimpleTagList = renderTags makeLink (intercalate " | ")
  where
    makeLink tag url _ _ _ = renderHtml $
      H.a ! A.href (toValue url) $ toHtml tag

tagsFieldWith' :: (Identifier -> Compiler [String])
  -> String
  -> Tags
  -> Context a
tagsFieldWith' getTags' key tags = field key $ \item -> do
  tags' <- getTags' $ itemIdentifier item
  links <- forM tags' $ \tag -> renderLink tag <$> getRoute (tagsMakeId tags tag)
  return . renderHtml . mconcat . intersperse " | " . catMaybes $ links
  where
    renderLink tag rout = pathToUrl <$> rout
      where
        pathToUrl path = H.a ! A.href (toValue . toUrl $ path) $ toHtml tag
