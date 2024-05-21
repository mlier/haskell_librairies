{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Data.Text ()
import Text.Hamlet.XML
import Text.XML
import           Data.Map        (empty)

main :: IO ()
main = putStrLn "Hello, Haskell!"

books :: Element
books = Element "xml" empty [xml|
<!DOCTYPE Order SYSTEM "order.dtd">
<?xml-stylesheet type="text/css" href="style.css"?>

<!--This is a comment!-->

<order date="2019-02-01">
    <address xmlns:shipping="http://localhost/XML/delivery" xmlns:billing="http://localhost/XML/billing">
        <shipping:name>Ellen Adams
        <shipping:street>123 Maple Street
        <shipping:city>Mill Valley
        <shipping:state>CA
        <shipping:zip>10999
        <shipping:country>USA
        <billing:name>Mary Adams
        <billing:street>8 Oak Avenue
        <billing:city>Old Town
        <billing:state>PA
        <billing:zip>95819
        <billing:country>USA
    <comment>Please use gift wrapping!
    <items>
        <book isbn="9781408845660">
            <title>Harry Potter and the Prisoner of Azkaban
            <quantity>1
            <priceus>22.94
            <comment>Please confirm delivery date until Christmas.
        <book isbn="9780544003415">
            <title>The Lord of the Rings
            <quantity>1
            <priceus>17.74
|]