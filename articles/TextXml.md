# Manipuler du XML avec Haskell  

L'objectif n'étant pas de faire un cours entier sur XML, vous pourrez en savoir beaucoup plus sur XML, en lisant le [cours](https://perso.univ-rennes1.fr/pierre.nerzic/XML) de Pierre Nezic de l'université Rennes 1.

Ce que nous allons faire maintenant consiste à fabriquer du XML et le lire à la mode XPATH en utilisant une librairie Haskell.

Retrouver l'ensemble du code source commenté dans cet article [ici](../src/TextXml).

## Utilisation de la librairie Haskell XML-Hamlet

La librairie Haskell XML-Hamlet fournit des fonctionnalités pratiques pour définir des documents XML.

Tout d'abord, vous devez importer les modules nécessaires dans votre fichier Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Hamlet.XML (xml)
```

Ensuite, vous pouvez utiliser la fonction `xml` pour définir un texte XML ou en HTML. La fonction `xml` prend un quasiquoteur XML et renvoie une valeur de type `Element`. Voici un exemple:

```haskell
html :: Element
html = [xml|
<html>
    <head>
        <title>
            My #
            <b>Title
    <body>
        <p>foo bar baz
|]
```
Vous observez la facilité de définir un texte XML en évitant les balises fermantes. La contrainte est d'écrire une balise par ligne. 

```xml
<html>
    <head>
        <title>
            My #
            <b>Title</b>
        </title>
    </head>
    <body>
        <p>foo bar baz</p>
    </body>
</html>
```

Nous pouvons maintenant écrire ce modèle dans un fichier XML en utilisant la fonction `writeFile` de la librairie `Text.XML` :

```haskell
xmlw :: IO ()
xmlw = do
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) html []
```
En ouvrant le fichier, vous retrouvez le texte HTML ci-dessus.


De la même manière, définissons un texte XML plus complexe décrivant un catalogue de livres :

```haskell
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
```

## Naviguer dans un XML avec le module Cursor de la librairie xml-conduit

La librairie Haskell xml-conduit fournit un module `Text.XML.Cursor` pour faciliter la navigation et la manipulation des documents XML. Ce module simule les requêtes pouvant être réalisées avec XPATH. Pour en savoir beaucoup plus sur XPATH, lisez la [présentation complète](https://jean-luc-massat.pedaweb.univ-amu.fr/ens/xml/04-xpath.html) de Jean-Luc Massat.


Tout d'abord, vous devez importer les modules nécessaires dans votre fichier Haskell:
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XML.Cursor (fromDocument, content, element, laxElement,
                                  attributeIs, parent, followingSibling, attribute,
                                  checkElement, check, checkNode, hasAttribute,
                                  ($/), ($//), (&/), (&//), (&.//), (>=>) )
```
Ensuite, vous pouvez utiliser la fonction `fromDocument` pour créer un curseur à partir d'un document XML. Dans le code suivant, nous avons créé un curseur `Cursor` à partir d'un document XML `books`. Un `Cursor` est un noeud XML qui a conscience de sa position dans l'arbre XML, qui est capable de récupérer certains noeuds en relation (enfants, parent...). Dans notre cas la variable `cursor`est rattachée à la racine de l'arbre XML. 

```haskell
cursor = fromDocument doc
    where
        doc = Document (Prologue [] Nothing []) books []
```
Nous allons regarder progressivement l'utilisation de la librairie en commençant par des requête très simples et on va progressivement enrichir les requêtes.

### Récupérer le contenu de tous les noeuds "Address" en mode souple
```haskell
cursor
        $// laxElement "Address"
        &// content
```

La fonction `laxElement` (lax pour relaché ou souple) permet de choisir les noeuds XML avec toutes les écritures du mot "adresse". Par exemple, la fonction renvoit le même résultat si le paramètre est "AdDRess". 

On obtient :
```json
["Ellen Adams","123 Maple Street","Mill Valley","CA","10999", "USA",
"Mary Adams","8 Oak Avenue","Old Town","PA","95819","USA"]
```

### Récupération impossible des noeuds "address" en mode stricte
```haskell
cursor
        $// element "Address"
        &// content
```

Même type de requête mais utilisant la fonction `element` qui est stricte sur le nom du noeud à chercher. 

Cette fonction renvoit rien du tout, elle recherche le nom d'élément exact  : 
```[]```

### Récupérer le contenu de tous les noeuds "address" (stricte)
```haskell
cursor
        $// element "address"
        &// content
```
Par contre, si on donne le nom exact du noeud présent dans le flux XML, la fonction renvoie bien le même résultat ci-dessus. 

On obtient :
```json
["Ellen Adams","123 Maple Street","Mill Valley","CA","10999", "USA",
"Mary Adams","8 Oak Avenue","Old Town","PA","95819","USA"]
```

### Récupérer le contenu de tous les noeuds "comment"
```haskell
cursor
        $// element "comment"
        &// content
```

Si on change de noeud XML, ça marche pareil. 
`$//` est une fonction qui à partir d'un `Cursor` renvoie l'ensemble des noeuds descendants du noeud courant. 
On obtient toutes les balises `comment`: 

```json
["Please use gift wrapping!","Please confirm delivery date until Christmas."]
```

### Récupérer le contenu du noeud "order"
```haskell
cursor
        $/ element "order"
        &// content
```

`$/`est une fonction qui part d'un cursor et qui renvoie les enfants du noeud initial uniquement. 
`&//` renvoie tous les contenus textuels en-dessous du noeud traité. 

On obtient : 
```json 
["Ellen Adams","123 Maple Street","Mill Valley","CA","10999","USA",
"Mary Adams","8 Oak Avenue","Old Town","PA","95819","USA",
"Please use gift wrapping!","Harry Potter and the Prisoner of Azkaban",
"1","22.94","Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Récupérer le contenu du noeud "items" dans le noeud "order"
```haskell
cursor
        $/ element "order"
        &/ element "items"
        &// content
```

Ici on prend les enfants `order` du noeud racine, puis on prend les enfants `items` de chaque noeud `order`.

On obtient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.","The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Récupérer le contenu de tous les noeuds "items"
```haskell
cursor
        $// element "items"
        &// content
```

Ici, on prend tous les noeuds `items` parmi les descendants du noeud racine. Puis on récupère les éléments textuels en dessous de chaque noeud `items`. 

On obient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Récupérer le contenu du noeud "items"
```haskell
cursor
        $/ element "items"
        &// content
```

Ici on prend uniquement les noeuds `items` parmi les enfants du noeud racine. 

Comme il n'y pas d'enfants `items` au noeud racine, on obtient : ```[]```

### Récupérer le contenu de tous les noeuds "book"
```haskell
cursor
        $// element "book"
        &// content
```
Ici, c'est la même requête que ci-dessus en utilisant `$//` et non `$/`. Donc, on sélectionne tous les noeuds `book`sous la racine quelque soit leurs positions.

On obtient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Récupérer le contenu de l'élément "title" dans l'élément "book"
```haskell
cursor
        $// element "book"
        &/ element "title"
        &// content
```

En rajoutant, la fonction `&/ element "title"` on sélectionne le titre de chaque livre. 

On obtient tous les titres de livres : 
```json
["Harry Potter and the Prisoner of Azkaban","The Lord of the Rings","Dune 1"]
```

### Récupérer le contenu de tous les noeuds "title"
```haskell
cursor
        $// element "title"
        &// content
```

Mais, évidemment, il aurait été plus directe de sélectionner uniquement tous les noeuds `title` sous le noeud racine. 

On obtient la même chose : 
```json
["Harry Potter and the Prisoner of Azkaban","The Lord of the Rings","Dune 1"]
```

### Récupérer le contenu du noeud "title" et de ses descendants
```haskell
cursor
        $// element "title"
        >=> parent
        &// content
```

Si on part d'un livre, il peut être nécessaire de revenir à son parent pour en faire quelque chose par l'utilisation de la fonction `>=> parent`. 

On obtient alors : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Récupérer le contenu du noeud "book" avec un  attribut "isbn" défini
```haskell
cursor
        $// element "book"
        >=> attributeIs "isbn" "9781408845660"
        &// content
```

Ici `>=> attributeIs "isbn" "9781408845660"` filtre les livres sélectionnés en retenant celui avec le bon attibut `ìsbn`.

On obtient le livre d'Harry Potter : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas."]
```

### Obtenir la liste des livres (en mode bizarre)
```haskell
cursor
        $// element "book"
        >=> attributeIs "isbn" "9781408845660"
        >=> parent
        >=> element "items"
        &// content
```

Ici on sélectionne un livre particulier, puis on remonte au parent pour sélectionner l'élément `items`. Ca ne sert à rien mais on fait joujou avec les fonctions de navigation dans l'arbre XML. 

On obtient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Liste des livres ayant un attribut "isbn" défini
```haskell
cursor
        $// element "book"
        >=> attributeIs "isbn" "9781408845660"
        &/ element "title"
        &// content
```

Ici ça sert plus à quelque chose. On récupére le titre du livre qui a un `isbn` donné. 

On obtient toujours le livre d'Harry Potter : 
```json
["Harry Potter and the Prisoner of Azkaban"]
```


### Récupérer la valeur de l'attribut "isbn" de tous les éléments "book"
```haskell
cursor
        $// element "book"
        >=> attribute "isbn"
```

Ici on souhaite récupérer l'`isbn` de tous les livres. on utilise la fonction `>=> attribute "isbn"`.

On obtient : 
```json
["9781408845660","9780544003415","9782266155489"]
```


### Récupérer la valeur de l'attribut "isbn" de tous les éléments ayant cet attribut
```haskell
cursor
        $// hasAttribute "isbn"
        >=> attribute "isbn"
```

Si l'attribut `isbn` est utilisé sur plusieurs types de noeud, on peut récupérer tous ces noeuds avec la fonction `$// hasAttribute "isbn"`.

On obtient de nouveau : 
```json
["9781408845660","9780544003415","9782266155489"]
```

### Liste des livres définis après un livre défini par son "isbn" 
```haskell
cursor
        $// element "book"
        >=> attributeIs "isbn" "9781408845660"
        >=> followingSibling
        &// content
```

Affichons ci-dessous l'excellente image de Jean-Luc Massat résumant les différents ensembles de noeuds qu'il est possible d'atteindre depuis le noeud courant. 
![Alt text](https://jean-luc-massat.pedaweb.univ-amu.fr/ens/xml/figures/xpath/les-axes.png)

Notre nouvel exemple utilise la fonction `>=> followingSibling` pour obtenir tous les noeuds qui suivent le noeud courant et les descendants.

On obtient alors dans cet exemple tous les livres après celui d'Harry Potter et sans ce dernier : 
```json
["The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```


### Récupérer le livre ayant un prix d'achat de 17,74 (utilisation des `Cursor`)
```haskell
cursor
        $// element "book"
        &/ element "priceus"
        >=> check priceEqualFromCursor
        >=> parent
        &// content
where
        priceEqualFromCursor c = T.concat (c $// content) == "17.74" 
```

Le filtrage sur les noeuds `Cursor` va renvoyer tous les noeuds `priceus`contenant le prix. La fonction `priceEqualFromCursor` va prendre le contenu textuel de ce noeud (`c $// content`) puis va transformer ce contenu en un texte que l'on compare au texte "17.74".

On obtient : 
```json
["The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Liste des livres ayant un titre (utilisation des `Element`)
```haskell
cursor
        $// element "order"
        &// checkElement titleFromElement
        >=> parent
        &// content
  where
        titleFromElement e = "title" `T.isPrefixOf` nameLocalName (elementName e)
```

On filtre sur les éléments en récupérant d'abord le `Name` de l'élément (défini dans xml-types), que l'on transforme en texte avec `nameLocalName`. 

On obtient la liste de tous les livres : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### Liste des livres dont le titre contient le mot "of the" (utilisation des `Element`)
```haskell
cursor
        $// element "title"
        &.// checkElement lordFromElement
        >=> parent
        &// content

  where
        lordFromElement e = "of the" `T.isInfixOf` T.concat (elementText $ toXMLElement e) 
```
On filtre sur les éléments en les transformant au moyen de `toXMLElement` de `Element` de xml-conduit en `Element` de xml-types. On extrait le texte au moyen de `elementText` et tout s'enchaîne vers la comparaison avec "of the".


On obtient le livre du Seigneur des anneaux : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas.",
"The Lord of the Rings","1","17.74","Dune 1","1","14.0"]
```

### (. free) Liste des livres dont le titre contient le mot "of the" (utilisation des `Element`)
```haskell
cursor
        $// element "title"
        >=> checkElement lordFromElement1
        >=> parent
        &// content
  where
        lordFromElement1 = ("of the" `T.isInfixOf`) . T.concat . elementText . toXMLElement
```

Comme je ne suis pas encore d'un grand niveau en Haskell, je n'arrive pas à faire une fonction utilisant directement la notation "point free". 
Voici donc la même fonction mais beaucoup plus "jolie". 


### Obtenir le livre dont le titre contient le mot "of the" (utilisation des `Node`)
```haskell
cursor
        $// element "title"
        >=> checkNode lordFromNode
        >=> parent
        &// content
  where
        lordFromNode = ("of the" `T.isInfixOf`) . T.concat . nodeText . toXMLNode
```

Ici le filtrage est réalisé sur des noeuds XML `Node` défini dans xml-conduit. La fonction `toXMLNode` réalise une transformation de type `Node -> Node` compatible avec la librairie xml-types. Ensuite, tout s'enchaîne, on transforme les noeuds en texte, puis en entier, puis en boléen (comparaison à "of the").

On obtient le livre du Seigneur des anneaux : 
```json
["The Lord of the Rings","1","17.74"]
```

### Liste des livres avec un prix supérieur à 19 (utilisation des `Cursor`) 
```haskell
cursor
        $// element "book"
        &/ element "priceus"
        >=> check priceSupFromCursor
        >=> parent
        &// content
  where
        priceSupFromCursor c = 19 < ( read . T.unpack . T.concat $ (c $// content) :: Double )
```

Le filtrage sur les noeuds `Cursor` va renvoyer tous les noeuds `priceus`contenant le prix. La fonction `priceSupFromCursor` va prendre le contenu textuel de ce noeud (`c $// content`) puis va transformer ce contenu en un texte, puis un entier, puis un boléen (comparaison à 19). 

On obtient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas."]
```

### Liste des livres avec un prix supérieur à 19 (utilisation des `Node`)
```haskell
cursor
        $// element "priceus"
        >=> checkNode priceSupFromNode
        >=> parent
        &// content
  where
        priceSupFromNode n = priceSupFromNode' n > 19.0
        priceSupFromNode' =  ( read . T.unpack . T.concat . nodeText . toXMLNode )::  Node -> Double
```

Ici le filtrage est réalisé sur des noeuds XML `Node` défini dans xml-conduit. La fonction `toXMLNode` réalise une transformation de type `Node -> Node` compatible avec la librairie xml-types. Ensuite, tout s'enchaîne, on transforme les noeuds en texte, puis en entier, puis en boléen (comparaison à 19).

On obtient : 
```json
["Harry Potter and the Prisoner of Azkaban","1","22.94",
"Please confirm delivery date until Christmas."]
```

## En rémsumé
Dans cet article, nous avons pu apprendre à utiliser certaines librairies pour manipulier du XML : `hamlet`, `xml-conduit`.

© mlier, 2024