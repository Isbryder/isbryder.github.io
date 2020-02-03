{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay
import Data.Monoid

import qualified Data.Text.Lazy.IO as T
import qualified Clay.Media as Media

main :: IO ()
main = T.putStr $ renderWith compact [] site

darkPrimary :: Color
darkPrimary = rgb 2 136 209

lightPrimary :: Color
lightPrimary = rgb 179 229 252

primary :: Color
primary = rgb 3 169 244

iconText :: Color
iconText = rgb 255 255 255

accent :: Color
accent = rgb 68 138 255

primaryText :: Color
primaryText = rgb 33 33 33

secondaryText :: Color
secondaryText = rgb 117 117 117

divider :: Color
divider = rgb 189 189 189


roboto :: Css
roboto = fontFamily ["Roboto", "Open Sans"] [sansSerif]
-------------------------------------------------------------------------------



site :: Css
site = html <> body ? do
    backgroundColor lightPrimary
    margin (px 20) (px 20) (px 20) (px 20)
    display flex
    alignItems center
    justifyContent center
    img ? do
        border solid (px 2) darkPrimary
        maxWidth (pct 100)
    main_ ? do
        width (px 720)
        border solid (px 2) darkPrimary
        backgroundColor iconText
        padding (px 40) (px 40)  (px 40) (px 40)
        p ? do
            padding (px 2) (px 2)  (px 2) (px 2)
        h1 ? do
            padding (px 2) (px 2)  (px 2) (px 2)
        h2 ? do
            padding (px 2) (px 2)  (px 2) (px 2)
        h3 ? do
            padding (px 2) (px 2)  (px 2) (px 2)
        table ? do
            padding (px 2) (px 2)  (px 2) (px 2)
    roboto
    h1 ? do
        fontColor primary
        fontSize (px 24)
    h2 ? do
        fontColor primary
        fontSize (px 17)
    h3 ? do
        fontColor primary
        fontSize (px 14)

    {-
menu :: Css
menu = nav ?
  do uiFont
     alignCenter
     marginTop   u1
     paddingLeft u1
     lineHeight  u2
     a ? marginRight (px 20)

contents :: Css
contents = ".content" ?
  do backgroundColor (setA 200 white)
     bgBorder        5
     padding         u1 u1 u2 u1

theFooter :: Css
theFooter = footer ?
  do uiFont
     alignCenter
     color     (setA 70 txtC)
     fontSize  (px 14)
     margin    u1 nil u4 nil

-------------------------------------------------------------------------------

overview :: Css
overview =
  do ".date" ?
       do smallFont
          float floatRight
     ".read-more" ?
       do marginTop (unit (-1))
          fontSize  (pct 85)

theArticle :: Css
theArticle = article ?
  do contentFont
     marginBottom u1
     Main.meta

     star ?
       do sym padding nil
          sym margin  nil

     h1 <> h2 ?
       do sym margin   nil
          lineHeight   u2
          marginTop    u1
          color emC

     h1 ?
       do fontSize (px 24)
          marginBottom u1

     h2 ?
       do fontSize (px 17)

     p  ? marginBottom u1
     ul ? paddingLeft  u2

     a ?
       do animate
          textDecoration none
          color          linkC
          hover & backgroundColor bgC

     "em" ?
       do fontWeight bold
          fontStyle  normal
          color emC

     strong ?
       do fontWeight normal
          fontStyle  italic
          fontSize   (px 15)
          color emC

     p ? code ?
       do codeBlocks
          sym2 padding 0 (px 4)

     p ? a ? code ?  color linkC

     pre ?
       do box
          codeBlocks
          margin       nil (unit (0)) u1 (unit (0))
          sym padding  (half 1)
          narrow $ fontSize  (px 14)
          wide   $ fontSize  (px 16)
          overflowX    auto

     img ?
       do marginLeft  auto
          marginRight auto
          display     block

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     floatRight
     marginTop (unit (-1))
     Clay.span ?
       do display block
          smallFont

-------------------------------------------------------------------------------

centered :: Css
centered =
  do box
     wide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     narrow $
       do width       (pct 100)

contentFont :: Css
contentFont =
  do merriWeather
     fontSize   (px 16)
     lineHeight u1
     color      txtC

uiFont :: Css
uiFont =
  do openSans
     fontSize      (px 20)
     lineHeight    u1
     textTransform uppercase
     a ?
       do color          linkC
          textDecoration none
          animate
          hover &
            do color      black
               background white

smallFont :: Css
smallFont =
  do openSans
     fontSize (pct 85)
     color    (setA 120 txtC)

codeBlocks :: Css
codeBlocks = 
  do bgBorder 20
     fontFamily ["Courier"] [monospace]
     syntax

syntax :: Css
syntax =
  do color (rgb 0 60 100)
     ".kw" ? fontWeight bold
     ".kw" ? color (rgb   0   0   0)
     ".dt" ? color (rgb  20  60 180)
     ".dv" ? color (rgb 100   0 200)
     ".st" ? color (rgb 200  80 100)
     ".ot" ? color (rgb 160  80  80)
     ".fu" ? color (rgb   0 160 120)
     ".co" ? color (rgb 160 160 160)


bgBorder :: Float -> Css
bgBorder o = outline solid (px 1) (setA o black)

box :: Css
box = boxSizing borderBox

animate :: Css
animate =
  transitions
    [ ("background-color" , sec 0.5, ease, sec 0)
    , ("color"            , sec 0.2, ease, sec 0)
    ]

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)


unit, half :: Double -> Size LengthUnit
unit = px . (* 24)
half = px . (* 12)

pageWidth :: Size LengthUnit
pageWidth = unit 25

u1, u2, u3, u4 :: Size LengthUnit
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

openSans :: Css
openSans = fontFamily ["Open Sans", "Helvetixa", "Arial"] [sansSerif]

merriWeather :: Css
merriWeather = fontFamily ["Merriweather", "Georgia", "Times"] [serif]

narrow :: Css -> Css
narrow = query all [Media.maxWidth pageWidth]

wide :: Css -> Css
wide = query all [Media.minWidth pageWidth]
-}
