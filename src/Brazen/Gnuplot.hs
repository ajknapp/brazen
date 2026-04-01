{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brazen.Gnuplot where

import Control.Exception
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

newtype Gnuplot = Gnuplot {getGnuplot :: FilePath -> TB.Builder}
  deriving newtype (Semigroup, Monoid)

data GnuplotException = GnuplotException Int String String
  deriving (Eq, Ord, Show, Exception)

runGnuplot :: Gnuplot -> FilePath -> IO ()
runGnuplot gscript dest = withSystemTempFile "brazen_gnuplot.plt" $ \file h -> do
  dest' <- makeAbsolute dest
  createDirectoryIfMissing True (takeDirectory dest')
  TL.hPutStrLn h (TB.toLazyText $ getGnuplot gscript dest')
  hClose h
  (e, gstdout, gstderr) <- readProcessWithExitCode "gnuplot" [file] []
  case e of
    ExitSuccess -> pure ()
    ExitFailure e' -> throwIO $ GnuplotException e' gstdout gstderr

hdiPlot :: String -> FilePath -> FilePath -> Gnuplot
hdiPlot name spath qpath =
  Gnuplot $ \output ->
    foldl1 (<>) $ fmap (<> "\n")
        [ "set datafile separator ','",
          "set terminal unknown",
          "set palette viridis",
          "set style fill solid",
          "unset colorbox",
          "set linetype 1 lc palette frac 0.5",
          "set linetype 2 lc rgb 'black'",
          "set title 'Posterior marginal for " <> TB.fromString name <> "'",
          "plot '" <> TB.fromString spath <> "' using 1 skip 1 bins=100 with boxes title 'count'",
          "yy = -(GPVAL_Y_MAX-GPVAL_Y_MIN)/20",
          "set terminal pngcairo size 1920,1080 font 'OpenSymbol'",
          "set output '" <> TB.fromString output <> "/hist_" <> TB.fromString name <>  ".png'",
          "set errorbars 2.5",
          "replot '" <> TB.fromString qpath <> "' u 1:(yy):3:4 w xerrorbars ps 2.5 lw 2 pt 7 t '90% HDI'"
        ]
