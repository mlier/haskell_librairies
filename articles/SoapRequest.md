# Réaliser des requêtes SOAP 

Pour réaliser des requêtes SOAP nous allons utiliser des serveurs SOAP existants qui sont décrits dans la page du site postman : [ici](https://www.postman.com/flight-candidate-55766172/workspace/my-workspace/collection/28173379-5deaf6d4-cd85-4f92-9fd7-9d0e8abc86b9). Décrivons un exemple qui va permettre de faire fonctionner la bibliothèques `soap` publiée sur [hackage](https://hackage.haskell.org/package/soap). 

## Faire une division

Nous utilisons la serveur soap de www.dneonline.com pour simuler des divisions par une requêt SOAP. La requête curl permet d'appeler le webservice.

```bash
curl --location 'http://www.dneonline.com/calculator.asmx' \
--header 'Content-Type: text/xml; charset=utf-8' \
--header 'SOAPAction: http://tempuri.org/Divide' \
--data '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
                xmlns:xsd="http://www.w3.org/2001/XMLSchema" 
                xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <Divide xmlns="http://tempuri.org/">
      <intA>10</intA>
      <intB>5</intB>
    </Divide>
  </soap:Body>
</soap:Envelope>'
```

On obtient la réponse suivante, 10 / 5 = 2 :) : 

```xml
<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope 
        xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
        xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <soap:Body>
        <DivideResponse xmlns="http://tempuri.org/">
            <DivideResult>2</DivideResult>
        </DivideResponse>
    </soap:Body>
</soap:Envelope>
```
Et maintenant, comment faire cette requête en utilisant Haskell. Pour cela, il est nécessaire d'importer les bonnes bibliothèques : `Network.SOAP`pour appeler les webservices, `Text.XML.Writer` pour définir l'enveloppe SOAP et `Text.XML.Cursor` pour lire le XML de la réponse. 

```haskell
import           Network.SOAP ( invokeWS, ResponseParser(CursorParser) )
import           Network.SOAP.Transport.HTTP ( initTransportWithM, 
                    printRequest, printBody )
import           Network.SOAP.Transport.HTTP.TLS ( makeSettings, validateDefault )
import           Network.HTTP.Client ( defaultManagerSettings )
import           Text.XML.Cursor ( laxElement, content, ($//), (&//) )
import qualified Text.XML.Writer as XW
import           Data.Text (Text)
```


- `initTransportWithM` qui permet de définir l'url du webservice appelé, de fournir des éléments de configuration particulier (ajout d'un niveau de sécurité TLS), de donner les éléments d'authentification si nécessaire et d'afficher si nécessaire la requête et la réponse pour pouvoir mettre au point le code.
- `invokeWS` qui permet d'appeler le webservice concrêtement et de définir comment traiter la réponse. 

```haskell
divide :: IO [Text]
divide = do
    let url = "http://www.dneonline.com/calculator.asmx"
    putStrLn url
    transport <- initTransportWithM
        defaultManagerSettings 
        url
        printRequest --pure -- pour ne pas affichier la requête
        printBody --pure -- pour ne pas affichier la réponse
    let s = "5" :: Text
        t = "10" :: Text
    let soapAction = "http://tempuri.org/Divide"
    let body = XW.elementA "Divide" [("xmlns", "http://tempuri.org/")] $ do 
            XW.element "intA" t
            XW.element "intB" s  
    invokeWS transport soapAction () body (CursorParser parser)
    where
        parser cur = cur $// laxElement "DivideResult" &// content

```

On obtient l'affichage suivant remis en page pour une meilleure lecture : 

```xml
http://www.dneonline.com/calculator.asmx
request:<?xml version="1.0" encoding="UTF-8"?>
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
        <soapenv:Body><Divide xmlns="http://tempuri.org/">
            <intA>10</intA>
            <intB>5</intB>
        </Divide></soapenv:Body>
    </soapenv:Envelope>

response:<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"  
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <soap:Body>
        <DivideResponse xmlns="http://tempuri.org/">
            <DivideResult>2</DivideResult>
        </DivideResponse>
    </soap:Body>
</soap:Envelope>
```

Dans le code source du github [ici](../src/SoapRequest/) vous trouverez plusieurs autres exemples de requêtes vers d'autres webservices. Par exemple, celui-ci qui est similaire. Remarquez l'utilisation de `makeSettings`, utilisant la couche TLS pour appeler un site `https` qui, dans ce cas là, n'exige pas de mot de passe. 

```haskell
elsiusToFahrenheit :: IO Text 
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
```





## Conclusion

Dans cet article, on a appris à manipuler la librairie `soap`qui permet de réaliser des requêtes vers des webservices SOAP. 

© mlier, 2024


