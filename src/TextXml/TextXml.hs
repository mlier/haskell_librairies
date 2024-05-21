{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude         hiding (writeFile)
import           Data.Map as M   (empty)
import           Text.XML        ( Name(nameLocalName),
                                   Node,
                                   Element(Element, elementName),
                                   Document(Document),
                                   toXMLElement,
                                   toXMLNode,
                                   writeFile,
                                   def,
                                   Prologue(Prologue) )
import           Text.Hamlet.XML ( xml )
import           Text.XML.Cursor (fromDocument, content, element, laxElement,
                                  attributeIs, parent, followingSibling, attribute,
                                  checkElement, check, checkNode, hasAttribute, 
                                  ($/), ($//), (&/), (&//), (&.//), (>=>) )
import           Data.XML.Types  (elementText, nodeText)


-- xpath : https://jean-luc-massat.pedaweb.univ-amu.fr/ens/xml/04-xpath.html
-- cours xml : https://perso.univ-rennes1.fr/pierre.nerzic/XML/poly.pdf
-- perf : https://github.com/haskell-perf/xml


html :: Element
html = Element "html" empty [xml|
<head>
    <title>
        My #
        <b>Title
<body>
    <p>foo bar baz
|]


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
        <book isbn="9782266155489">
            <title>Dune 1
            <quantity>1
            <priceus>14.0
|]


xmlw :: IO ()
xmlw = do
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) html []


-- $ => applique un Axe à un ensemble de noeuds du Cursor
-- $|   : noeud courant du Cursor
-- $/   : enfants du noeud courant du Cursor
-- $//  : descendants du noeud courant du Cursor
-- $.// : noeud courant + descendants du Cursor

-- & => combine deux Axe et renvoie un Axe
-- &|   : applique une fonction à l'Axe
-- &/   : enfants du noeud courant 
-- &//  : descendants du noeud courant 
-- &.// : noeud courant + descendants


xmlc :: Int -> [Text]
xmlc i = case i of
    1 -> cursor
            $// laxElement "Address"
            &// content

    2 -> cursor
            $// element "Address"
            &// content

    3 -> cursor
            $// element "address"
            &// content

    4 -> cursor
            $// element "comment"
            &// content

    5 -> cursor
            $/ element "order"
            &// content
    6 -> cursor
            $/ element "order"
            &/ element "items"
            &// content

    7 -> cursor
            $// element "items"
            &// content

    8 -> cursor
            $/ element "items"
            &// content

    9 -> cursor
            $// element "book"
            &// content

    10 -> cursor
            $// element "book"
            &/ element "title"
            &// content

    11 -> cursor
            $// element "title"
            &// content

    12 -> cursor
            $// element "title"
            >=> parent
            &// content

    13 -> cursor
            $// element "book"
            >=> attributeIs "isbn" "9781408845660"
            >=> parent
            >=> element "items"
            &// content

    14 -> cursor
            $// element "book"
            >=> attributeIs "isbn" "9781408845660"
            &// content

    15 -> cursor
            $// element "book"
            >=> attributeIs "isbn" "9781408845660"
            &/ element "title"
            &// content

    16 -> cursor
            $// element "book"
            >=> attribute "isbn"

    17 -> cursor
            $// hasAttribute "isbn"
            >=> attribute "isbn"

    18 -> cursor
            $// element "book"
            >=> attributeIs "isbn" "9781408845660"
            >=> followingSibling
            &// content

    19 -> cursor
            $// element "book"
            &/ element "priceus"
            >=> check priceEqualFromCursor
            >=> parent
            &// content

    20 -> cursor
            $// element "order"
            &// checkElement titleFromElement
            >=> parent
            &// content

    21 -> cursor
            $// element "title"
            &.// checkElement lordFromElement
            >=> parent
            &// content

    22 -> cursor
            $// element "title"
            >=> checkElement lordFromElement1
            >=> parent
            &// content

    23 -> cursor
            $// element "title"
            >=> checkNode lordFromNode
            >=> parent
            &// content

    24 -> cursor
            $// element "book"
            &/ element "priceus"
            >=> check priceSupFromCursor
            >=> parent
            &// content

    25 -> cursor
            $// element "priceus"
            >=> checkNode priceSupFromNode
            >=> parent
            &// content

    _ -> ["otherwise"]
    
    where
        cursor = fromDocument doc -- pointe vers le noeud racine de l'arbre xml
        doc = Document (Prologue [] Nothing []) books []
        priceEqualFromCursor c = T.concat (c $// content) == "17.74"  
        priceSupFromCursor c = 19 < ( read . T.unpack . T.concat $ (c $// content) :: Double )
        priceSupFromNode c = priceSupFromNode' c > 19.0
        priceSupFromNode' =  ( read . T.unpack . T.concat . nodeText . toXMLNode )::  Node -> Double
        titleFromElement e = "title" `T.isPrefixOf` nameLocalName (elementName e)
        lordFromElement e = "Lord" `T.isInfixOf` T.concat (elementText $ toXMLElement e)
        lordFromElement1 = ("Lord" `T.isInfixOf`) . T.concat . elementText . toXMLElement
        lordFromNode = ("Lord" `T.isInfixOf`) . T.concat . nodeText . toXMLNode
        


