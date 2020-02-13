--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "images/categories/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "README.md" $ do
        route   idRoute
        compile copyFileCompiler

--    match (fromList ["about.rst", "contact.markdown"]) $ do
    match (fromList ["about.rst"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate defaultHtml defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate defaultHtml postCtx
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
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate defaultHtml archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            teasers <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" ""                `mappend`
                    listField "teaser" (bodyField "content" `mappend` postCtx) (return teasers) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate defaultHtml indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "OB's Blog - " ++ title
    , feedDescription = "THUS SPAKE OBELISK"
    , feedAuthorName  = "obeliskgolem"
    , feedAuthorEmail = "obelisk.golem@gmail.com"
    , feedRoot        = "https://obeliskgolem.github.io"
    }

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]
--------------------------------------------------------------------------------
defaultHtml :: Identifier 
defaultHtml = fromFilePath "templates/pure_default.html"
{-
syntaxPandocCompiler :: Compiler (Item String)
syntaxPandocCompiler = pandocCompilerWith syntaxHakyllReaderOptions defaultHakyllWriterOptions

syntaxHakyllReaderOptions :: ReaderOptions
syntaxHakyllReaderOptions = def
        {
      readerExtensions = enableExtension Ext_smart pandocExtensions
}
-}
