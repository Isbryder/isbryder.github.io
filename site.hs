--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Images ( loadImage
                     , compressJpgCompiler
                     , resizeImageCompiler
                     )

import System.FilePath


cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where
        createIndexRoute "index.html" = "index.html"
        createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
        pattern = "/index.html"
        replacement = const "/"

main :: IO ()
main = hakyll $ do
    match "files/*" $ do
        route $ gsubRoute "files/" (const "")
        compile copyFileCompiler

    match "images/**.jpg" $ do
        route idRoute
        compile $ loadImage 
            >>= resizeImageCompiler 720 480
            >>= compressJpgCompiler 50

    match "style/*.hs" $ do
        route (setExtension "css")
        compile (getResourceString >>= withItemBody (unixFilter "runghc" []))

    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` cleanRoute `composeRoutes` setExtension "html"
        compile $ getResourceBody
            >>= applyAsTemplate defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexHtmls

    match "404.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match ("loever/*" .||. "rekvisitter/*" .||. "entreprenoerer/*") $ do
        route $ cleanRoute `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/item.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["rekvisitter.html"] $ do
        route cleanRoute
        compile $ do
            rekvisitter <- loadAll "rekvisitter/*"

            let ctx = listField "rekvisitter" defaultContext (return rekvisitter) `mappend`
                    constField "title" "Rekvisitter" `mappend`
                    constField "description" "Rekvisitter og idéer." `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/rekvisitter.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["roller.html"] $ do
        route cleanRoute
        compile $ do
            loever <- loadAll "loever/*"
            entreprenoerer <- loadAll "entreprenoerer/*"

            let ctx = listField "loever" defaultContext (return loever) `mappend`
                    listField "entreprenoerer" defaultContext (return entreprenoerer) `mappend`
                    constField "description" "Lad dig inspirere. Ikke politisk korrekt." `mappend`
                    constField "title" "Roller" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/roller.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls


    match "templates/*" $ compile templateBodyCompiler
