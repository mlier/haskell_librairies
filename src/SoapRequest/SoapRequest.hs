{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.SOAP ( invokeWS, ResponseParser(CursorParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, printRequest, printBody )
import           Network.SOAP.Transport.HTTP.TLS ( makeSettings, validateDefault )
import           Network.HTTP.Client ( defaultManagerSettings )
import           Text.XML.Cursor ( laxElement, content, ($//), (&//) )
import qualified Text.XML.Writer as XW
import           Data.Text (Text)


divide :: IO [Text]
divide = do
    let url = "http://www.dneonline.com/calculator.asmx"
    putStrLn url
    transport <- initTransportWithM
        defaultManagerSettings 
        url
        printRequest --pure
        printBody --pure
    let s = "5" :: Text
        t = "10" :: Text
    let soapAction = "http://tempuri.org/Divide"
    let body = XW.elementA "Divide" [("xmlns", "http://tempuri.org/")] $ do 
            XW.element "intA" t
            XW.element "intB" s  
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = cur $// laxElement "DivideResult" &// content
        --parser cur = cur $// content


multiply :: IO [Text]     
multiply = do
    let url = "http://www.dneonline.com/calculator.asmx"
    putStrLn url
    transport <- initTransportWithM
        defaultManagerSettings 
        url
        printRequest --pure
        printBody --pure
    let s = "5" :: Text
    let soapAction = "http://tempuri.org/Multiply"
    let body = XW.elementA "Multiply" [("xmlns", "http://tempuri.org/")] $ do 
            XW.element "intA" s
            XW.element "intB" s  
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = cur $// laxElement "MultiplyResult" &// content

currencies :: IO [Text]
currencies = do
    let url = "http://webservices.oorsprong.org/websamples.countryinfo/CountryInfoService.wso"
    putStrLn url
    transport <- initTransportWithM
        defaultManagerSettings 
        url
        printRequest --pure
        printBody --pure
    let soapAction = ""
    let body = XW.elementA "ListOfCurrenciesByName" [("xmlns", "http://www.oorsprong.org/websamples.countryinfo")] $ do 
                    XW.empty 
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = cur $// laxElement "sISOCode" &// content
        --parser cur = cur $// content          
 
isValidISBN13 :: IO Text
isValidISBN13 = do
    let url = "http://webservices.daehosting.com/services/isbnservice.wso"
    putStrLn url
    transport <- initTransportWithM
        defaultManagerSettings 
        url
        printRequest --pure
        printBody --pure
    let s = "978-1-4612-9090-2" :: Text
    let soapAction = ""
    let body = XW.elementA "IsValidISBN13" [("xmlns", "http://webservices.daehosting.com/ISBN")] $ do 
                XW.element "sISBN" s
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = Prelude.head $ cur $// laxElement "IsValidISBN13Result" &// content

celsiusToFahrenheit :: IO Text 
celsiusToFahrenheit = do
    settings <- makeSettings Nothing Nothing validateDefault
    let url = "https://www.w3schools.com/xml/tempconvert.asmx"
    putStrLn url
    transport <- initTransportWithM
        settings 
        url
        printRequest --pure
        printBody --pure
    let s = "20" :: Text
    let soapAction = "https://www.w3schools.com/xml/CelsiusToFahrenheit"
    let body = XW.elementA "CelsiusToFahrenheit" [("xmlns", "https://www.w3schools.com/xml/")] $ do 
                XW.element "Celsius" s
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = Prelude.head $ cur $// laxElement "CelsiusToFahrenheitResult" &// content
        

main :: IO ()
main = do 
        putStrLn "SoapRequest"
