--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Debug.Trace


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["aldersgruppe.markdown", "sted.markdown", "varighed.markdown", "rekvisitter.markdown","deltager-antal.markdown", "isbryderne.html", "isbjørn.html", "danbjørn.html"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match ("loever/*" .||. "rekvisitter/*" .||. "entreprenoerer/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/item.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["rekvisitter.html"] $ do
        route idRoute
        compile $ do
            rekvisitter <- loadAll "rekvisitter/*"

            let ctx = listField "rekvisitter" defaultContext (return rekvisitter) `mappend`
                    constField "title" "Rekvisitter" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/rekvisitter.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["roller.html"] $ do
        route idRoute
        compile $ do
            loever <- loadAll "loever/*"
            entreprenoerer <- loadAll "entreprenoerer/*"

            let ctx = listField "loever" defaultContext (return loever) `mappend`
                    listField "entreprenoerer" defaultContext (return entreprenoerer) `mappend`
                    constField "title" "Roller" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/roller.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
