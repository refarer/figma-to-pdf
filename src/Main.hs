{-# LANGUAGE OverloadedStrings #-}

module Main
where

import           Network.Wreq
import           Control.Lens
import           System.Environment
import           Data.List
import           Data.List.Utils
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LS
import           Graphics.PDF
import qualified Data.Text                     as T
import           Control.Monad

opts :: String -> Options
opts token = defaults & header "X-FIGMA-TOKEN" .~ [BS.pack token]

imagePage :: JpegFile -> PDF ()
imagePage jpeg = do
    let (w, h) = jpegBounds jpeg
    page <- addPage $ Just $ PDFRect 0 0 w h
    ref  <- createPDFJpeg jpeg
    drawWithPage page $ drawXObject ref

pdfSize :: JpegFile -> PDFRect
pdfSize f = PDFRect 0 0 w h where (w, h) = jpegBounds f

jpegsToPDF :: [FilePath] -> FilePath -> IO ()
jpegsToPDF jpegs pdf = do
    print jpegs
    js <- sequence <$> mapM readJpegFile jpegs
    case js of
        Left err -> print err
        Right files ->
            runPdf pdf standardDocInfo (pdfSize $ head files)
                $ forM_ files imagePage

getURL :: String -> IO (String, Response LS.ByteString)
getURL url = do
    res <- get url
    return (url, res)

getFinalPart :: String -> String
getFinalPart = reverse . takeWhile (/= '/') . reverse

downloadFiles :: [T.Text] -> IO [String]
downloadFiles files = do
    results <- mapM (getURL . T.unpack) files
    forM results $ \(url, res) -> do
        putStrLn $ url ++ "; OK"
        let content = res ^. responseBody
            fpath   = "./export/images/" ++ getFinalPart url ++ ".jpg"
        LS.writeFile fpath content
        return fpath

convertIds :: [String] -> String
convertIds s = replace ":" "%3A" $ intercalate "%2C" s

getImages :: String -> String -> String -> IO [T.Text]
getImages ids token file = do
    images <- getWith
        (opts token)
        (  "https://api.figma.com/v1/images/"
        ++ file
        ++ "?ids="
        ++ ids
        ++ "&format=jpg"
        )

    return $ reverse (images ^.. responseBody . key "images" . members . _String)

getNodeIds :: String -> String -> IO (T.Text, [T.Text], [T.Text])
getNodeIds token file = do
    document <- getWith (opts token) ("https://api.figma.com/v1/files/" ++ file)
    let ids =
            document
                ^.. responseBody
                .   key "document"
                .   key "children"
                .   nth 0
                .   key "children"
                .   values
                .   key "id"
                .   _String
        fileName =
            document
                ^. responseBody
                .  key "document"
                .  key "children"
                .  nth 0
                .  key "name"
                .  _String

        names =
            document
                ^.. responseBody
                .   key "document"
                .   key "children"
                .   nth 0
                .   key "children"
                .   values
                .   key "name"
                .   _String

    return (fileName, ids, names)

main :: IO ()
main = do
    figmaToken             <- getEnv "FIGMA_TOKEN"
    figmaFile              <- getArgs

    (filename, ids, names) <- getNodeIds figmaToken (head figmaFile :: String)

    imagePaths             <- getImages (convertIds (map T.unpack ids))
                                        figmaToken
                                        (head figmaFile :: String)

    filesToSave <- downloadFiles imagePaths

    jpegsToPDF filesToSave ("./export/" ++ T.unpack filename ++ ".pdf")
    print "done"
