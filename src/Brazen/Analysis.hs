{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Brazen.Analysis where

import Brazen
import Brazen.FieldNames
import Brazen.Gnuplot
import Clay (Css)
import qualified Clay as C
import Control.Lens
import Data.Fixed
import Data.HKD
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text as TS
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf

class (Storable a) => QuantileSort a where
  quantileSort :: Ptr a -> CInt -> IO ()

foreign import ccall unsafe fsort :: Ptr Float -> CInt -> IO ()

instance QuantileSort Float where
  quantileSort = fsort

foreign import ccall unsafe dsort :: Ptr Double -> CInt -> IO ()

instance QuantileSort Double where
  quantileSort = dsort

newtype Quantile a = Quantile {getQuantile :: VS.Vector a}

precomputeQuantile :: (Storable a, QuantileSort a) => VS.Vector a -> IO (Quantile a)
precomputeQuantile v = do
  v' <- VS.thaw v
  VM.unsafeWith v' $ \vp -> quantileSort vp (fromIntegral $ VS.length v)
  Quantile <$> VS.unsafeFreeze v'

quantile :: (Storable a, Num a, RealFrac a) => Quantile a -> a -> a
quantile (Quantile v) q' = (1 - alpha) * v VS.! Prelude.max 0 (k - 1) + alpha * v VS.! Prelude.min (n - 1) k
  where
    n = VS.length v
    r = fromIntegral n * q'
    alpha = mod' r 1
    k = floor r

data Quantiles a = Quantiles
  { _p01 :: a,
    _p025 :: a,
    _p5 :: a,
    _p10 :: a,
    _p25 :: a,
    _p50 :: a,
    _p75 :: a,
    _p90 :: a,
    _p95 :: a,
    _p975 :: a,
    _p99 :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

$(makeLenses ''Quantiles)

quantileVals :: (Fractional a) => Quantiles a
quantileVals =
  Quantiles
    { _p01 = 0.01,
      _p025 = 0.025,
      _p5 = 0.05,
      _p10 = 0.1,
      _p25 = 0.25,
      _p50 = 0.5,
      _p75 = 0.75,
      _p90 = 0.9,
      _p95 = 0.95,
      _p975 = 0.975,
      _p99 = 0.99
    }

quantileAnalysis :: (QuantileSort a, Fractional a, RealFrac a) => VS.Vector a -> IO (Quantiles a)
quantileAnalysis v = do
  qs <- precomputeQuantile v
  pure $ fmap (quantile qs) quantileVals

quantileTableHeader :: Html
quantileTableHeader =
  H.thead . H.tr $
    toHtml $
      fmap
        H.th
        [ "Parameter Name",
          "Expected Value",
          "1% Quantile",
          "2.5% Quantile", "5% Quantile",
          "10% Quantile",
          "25% Quantile",
          "50% Quantile",
          "75% Quantile",
          "90% Quantile",
          "95% Quantile",
          "97.5% Quantile",
          "99% Quantile"
        ]

displayNumber :: (Ord a, PrintfArg a, Floating a, RealFrac a) => a -> String
displayNumber a = if b < 1e-4 || b > 1e4 then printf "%.4e" a else printf ("%." <> show decimals <> "f") a
  where
    b = abs a
    ndigits = ceiling (logBase 10 (abs a)) :: Int
    decimals = 4 - ndigits

quantileTableRow :: (Ord a, PrintfArg a, Floating a, RealFrac a) => String -> a -> Quantiles a -> Html
quantileTableRow name ev qs = H.tr $
  H.td (toHtml name)
    <> H.td (toHtml (displayNumber ev))
    <> fromMaybe "" (foldMap (Just . H.td . toHtml . displayNumber) qs)

readColumn :: String -> IO (VS.Vector Float)
readColumn name = VS.fromList . fmap (read . TL.unpack) . drop 1 . TL.lines <$> TIO.readFile name

quantileTable :: (FTraversable f) => f (FieldName Identity) -> IO Html
quantileTable f = do
  f' <- ffor f $ \case
    FieldVar name -> do
      let csvFile = name <> ".csv"
          csvQuantileFile = name <> "_quantile.csv"
          quantileHeader = "ev,y,p5,p95\n"
      c <- readColumn csvFile
      q <- quantileAnalysis c
      let mean x = VS.sum x / fromIntegral (VS.length x)
          cbar = mean c
          quantileStr = show cbar <> ",0," <> show (q ^. p5) <> "," <> show (q ^. p95)
      writeFile csvQuantileFile $ quantileHeader <> quantileStr
      pure $ Const (quantileTableRow name cbar q)
  pure $ ffoldMapDefault (\(Const x) -> x) f'


writeQuantileTable :: FilePath -> FieldName Identity a -> IO (Const Html a)
writeQuantileTable reportDir (FieldVar name) = do
  let csvPath = name <> ".csv"
      csvQuantilePath = name <> "_quantile.csv"
      imgName = "hist_" <> name <> ".png"
  runGnuplot (hdiPlot name csvPath csvQuantilePath) reportDir
  pure $ Const $ H.img H.! A.src (H.stringValue imgName)

writeHtmlReport :: (FTraversable f, FTraversable h) => FilePath -> f (FieldName Identity) -> h (FieldName Identity) -> IO ()
writeHtmlReport reportDir f h = do
  createDirectoryIfMissing False reportDir
  fqt <- quantileTable f
  hqt <- quantileTable h
  fhdis <- ffor f (writeQuantileTable reportDir)
  hhdis <- ffor h (writeQuantileTable reportDir)
  let report = H.docTypeHtml . H.html $ do
        H.head $ do
          H.meta H.! A.charset "utf-8"
          H.title "MCMC Analysis"
          H.style $ toHtml $ C.renderWith C.compact [] combinedCss
        H.body $ H.div H.! A.id "main" $ do
          H.h1 "MCMC Analysis"
          H.h2 "Parameter Estimates"
          H.table (quantileTableHeader <> fqt)
          H.h2 "Generated Quantity Estimates"
          H.table (quantileTableHeader <> hqt)
          H.h2 "Parameter HDI plots"
          ffoldMap (\(Const x) -> x) fhdis
          H.h2 "Generated Quantity HDI plots"
          ffoldMap (\(Const x) -> x) hhdis
  TIO.writeFile (reportDir </> "index.html") (renderHtml report)

bodyCss :: Css
bodyCss = C.root C.body $ do
  C.fontFamily ["Seravek", "Gill Sans Nova", "Ubuntu", "Calibri", "DejaVu Sans", "sourc-sans-pro"] [C.sansSerif]
  C.fontSize (C.pt 12)

headerCss :: Css
headerCss = C.root (foldl1 (<>) [C.h1, C.h2, C.h3, C.h4, C.h5, C.h6]) $ do
  C.textAlign C.center

mainDivCss :: Css
mainDivCss = C.root (C.element (TS.pack "#main")) $ do
  C.width (C.pct 80)
  C.margin C.auto C.auto C.auto C.auto

tableCss :: Css
tableCss = C.root C.table $ do
  C.marginLeft C.auto
  C.marginRight C.auto
  C.borderCollapse C.collapse

tableHeaderCss :: Css
tableHeaderCss = C.root C.th $ do
  C.borderBottom (C.px 1) C.solid C.black

tableElementCss :: Css
tableElementCss = C.root (C.td <> C.th <> C.thead) $ do
  C.paddingLeft (C.px 10)
  C.paddingRight (C.px 10)
  C.textAlign (C.alignSide C.sideLeft)

firstTableColumnCss :: Css
firstTableColumnCss = C.root (C.th <> C.td) $ C.nthChild (TS.pack "1") C.& do
  C.borderRight (C.px 1) C.solid C.black

oddTableColumnCss :: Css
oddTableColumnCss = do
  let grey = 0xef
  C.root C.thead $ C.nthChild (TS.pack "odd") C.& do
    C.backgroundColor (C.rgb grey grey grey)
  C.root C.tr $ C.nthChild (TS.pack "odd") C.& do
    C.backgroundColor (C.rgb grey grey grey)

evenTableColumnCss :: Css
evenTableColumnCss = do
  let grey = 0xdf
  C.root C.thead $ C.nthChild (TS.pack "even") C.& do
    C.backgroundColor (C.rgb grey grey grey)
  C.root C.tr $ C.nthChild (TS.pack "even") C.& do
    C.backgroundColor (C.rgb grey grey grey)

oddTableColumnHoverCss :: Css
oddTableColumnHoverCss = do
  let grey = 0xcf
  C.root (C.thead C.# C.hover) $ C.nthChild (TS.pack "odd") C.& do
    C.backgroundColor (C.rgb grey grey grey)
  C.root (C.tr C.# C.hover) $ C.nthChild (TS.pack "odd") C.& do
    C.backgroundColor (C.rgb grey grey grey)

evenTableColumnHoverCss :: Css
evenTableColumnHoverCss = do
  let grey = 0xbf
  C.root (C.thead C.# C.hover) $ C.nthChild (TS.pack "even") C.& do
    C.backgroundColor (C.rgb grey grey grey)
  C.root (C.tr C.# C.hover) $ C.nthChild (TS.pack "even") C.& do
    C.backgroundColor (C.rgb grey grey grey)

codePreCss :: Css
codePreCss = C.root (C.code <> C.pre) $ do
  C.fontFamily [TS.pack "Noto Sans Mono"] [C.monospace]
  C.fontSize (C.pt 10)

preCss :: Css
preCss = C.root C.pre $ do
  C.backgroundColor (C.rgb 0xef 0xef 0xef)
  C.borderRadius (C.px 10) (C.px 10) (C.px 10) (C.px 10)
  C.padding (C.px 10) (C.px 10) (C.px 10) (C.px 10)

imgCss :: Css
imgCss = C.root C.img $ do
  C.display C.block
  C.marginLeft C.auto
  C.marginRight C.auto
  C.width (C.pct 80)

combinedCss :: Css
combinedCss = do
  bodyCss
  headerCss
  mainDivCss
  tableCss
  tableHeaderCss
  tableElementCss
  firstTableColumnCss
  oddTableColumnCss
  evenTableColumnCss
  oddTableColumnHoverCss
  evenTableColumnHoverCss
  codePreCss
  preCss
  imgCss
