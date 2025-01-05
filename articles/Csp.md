# Problèmes de satisfaction de contraintes

Aujourd'hui, il s'agit de pouvoir utiliser la librairie CSP qui permet de résoudre des problèmes de statisfaction de contraintes. Tout est expliqué très bien dans la page Wikipedia [ici](https://fr.wikipedia.org/wiki/Probl%C3%A8me_de_satisfaction_de_contraintes) : les problèmes de satisfaction de contraintes ou CSP (Constraint Satisfaction Problem) sont des problèmes mathématiques où l'on cherche des états satisfaisant un certain nombre de contraintes ou de critères.

Haskell possède une librairie permettant de résoudre de tels problèmes : [csp - Discrete constraint satisfaction problem (CSP) solver](https://hackage.haskell.org/package/csp).
Nous allons pouvoir la mettre en oeuvre sur deux problèmes classiques. 

Retrouver l'ensemble du code source commenté dans cet article [ici](../src/CSP).

## Le problème TWO + TWO = FOUR
On considère l’addition suivante dans laquelle chaque lettre représente un chiffre : TWO + TWO = FOUR. Le problème consiste à trouver quel chiffre correspond à quelle lettre.

Pour résoudre ce problème, on défini les variables T, W, O, F, U, R pouvant prendre des valeur de 0 à 9. On ajoute deux variables R1 et R2 pour la valeur des retenues pour les deux premières colonnes.


``` haskell
dvT <- mkDV [0 .. 9]
dvW <- mkDV [0 .. 9]
dvO <- mkDV [0 .. 9]
dvF <- mkDV [0 .. 9]
dvU <- mkDV [0 .. 9]
dvR <- mkDV [0 .. 9]

dvR1 <- mkDV [0 .. 1]
dvR2 <- mkDV [0 .. 1]
```

L'étape suivante consiste à définir les contraintes qui sont appliquées aux variables. D'abord, il faut que chaque lettre soit un chiffre différent. On va utiliser la fonction `constraint2 (/=)` appliqué à deux variables pour cela. On va appliquer cette fonction pour toutes les variables prises deux par deux. Cela donne : 

``` haskell
assertAllDifferentConstraints [dvT, dvO, dvW, dvF, dvU, dvR]

assertAllDifferentConstraints =  mapAllPairsM_ (constraint2 (/=))

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ _ []    = return ()
mapAllPairsM_ _ [_]   = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l

```

Puis, on ajoute les contraintes pour satisfaire le calcul : 
- O + O = R + 10* la retenue
- W + W + la retenue précédente = U + 10* la retenue
- T + T + la retenue précédente = O + 10*F

Ce qui donne avec la librairie : 

```haskell
  constraint3 oPlusOegaleR dvO dvR dvR1
  constraint wPlusWegaleU [dvW, dvU, dvR1, dvR2]
  constraint tPlusTegaleO [dvT, dvO, dvF, dvR2]
    where
        oPlusOegaleR o r r1 =         r == o + o - 10*r1
        wPlusWegaleU (w:u:r1:r2:_) =  u == r1 + w + w - 10*r2
        tPlusTegaleO (t:o:f:r2:_)  =  o == r2 + t+t - 10*f
```
On cherche toutes les solutions avec la fonction `allCSPSolutions`.




## Problème d'Einstein
Le problème d'Einstein est le suivant : 5 hommes de nationalité différentes, habitent 5 maisons alignées, de couleurs distinctes. Ils boivent des boissons distinctes. Ils ont des professions distinctes, et ils possèdent chacun un animal d’espèces distinctes.

- Les nationalités sont : anglais, espagnol, japonais, italien, norvégien.
- Les couleurs des maisons sont : rouge, vert, blanc, jaune, bleu.
- Les boissons préférées sont : thé, café, lait, jus, eau.
- Les professions sont : peintre, sculpteur, diplomate, violoniste, médecin.
- Leurs animaux sont : chien, escargot, renard, cheval, zèbre.

On connait en plus les informations suivantes : 

- L’anglais vit dans la maison rouge
- L’espagnol possède un chien
- Le japonais est peintre
- L’italien boit du thé
- Le norvégien vit dans la première maison à gauche
- Le propriétaire de la maison verte boit du café
- La maison verte est à droite de la maison blanche
- Le sculpteur élève des escargots
- Le diplomate vit dans la maison jaune
- Le propriétaire de la maison du milieu boit du lait
- Le norvégien est voisin de la maison bleue
- Le violoniste boit des jus de fruits
- Le renard est dans la maison voisine de celle du médecin
- Le cheval est dans la maison voisine de celle du diplomate.

Le but est évidemment de trouver la bonne répartition. Il n'y a qu'une seule solution. 

Ici, la bonne manière de prendre le problème est définir les variables prenant des valeurs de 0 à 4 (pour les 5 maisons). Nos 25 variables sont : 

```haskell
allListVars :: [[String]]
allListVars = [   ["anglais", "espagnol", "japonais", "italien", "norvegien"]
                , ["rouge", "vert", "blanc", "jaune", "bleu"]
                , ["the", "cafe", "lait", "jus", "eau"]
                , ["peintre", "sculpteur", "diplomate", "violoniste", "medecin"]
                , ["chien", "escargot", "renard", "cheval", "zebre"]
              ]
```

Nous pouvons les définir dans la librairie au moyen de : 

```haskell
  dvs <- mapM (mapM ( \a -> mkDV [0 :: Int .. 4] )) listVar
```

On place toutes ces variables dans un dictionnaire (Data.HashMap.Strict) pour pouvoir les manipuler plus facilement dans la définition des contraintes : 

```haskell
  let listNameDvs = zipWith zip listVar dvs
  let dictDvs = M.fromList $ concat listNameDvs
```

Il faut définir des contraintes permettant de dire que deux variables sont voisines, que l'une est à droite de l'autre, qu'une variable est dans la maison du milieu, ou qu'une variable est tout à gauche.  

```haskell
estvoisin x y = x==y+1 || x==y-1
adroite x y = x==y+1
agauche = (==(0 :: Int))
aumilieu = (==(2 :: Int))

assertAllDifferentConstraints =  mapAllPairsM_ (constraint2 (/=))

constraint2egale dict v1 v2 = constraint2 (==) (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)
constraint1agauche dict v = constraint1 agauche (fromJust $ M.lookup v dict )
constraint2adroite dict v1 v2 = constraint2 adroite (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)
constraint1aumilieu dict v = constraint1 aumilieu (fromJust $ M.lookup v dict )
constraint2estvoisin dict v1 v2 = constraint2 estvoisin (fromJust $ M.lookup v1 dict ) (fromJust $ M.lookup v2 dict)

```

Alors, on peut définir les contraintes en suivant le texte du problème ligne à ligne : 

```haskell
mapM_ assertAllDifferentConstraints dvs

constraint2egale dictDvs "anglais" "rouge"
constraint2egale dictDvs "espagnol" "chien"
constraint2egale dictDvs "japonais" "peintre"
constraint2egale dictDvs "italien" "the"
constraint1agauche dictDvs "norvegien"
constraint2egale dictDvs "vert" "cafe"
constraint2adroite dictDvs "vert" "blanc"
constraint2egale dictDvs "sculpteur" "escargot"
constraint2egale dictDvs "diplomate" "jaune"
constraint1aumilieu dictDvs "lait"
constraint2estvoisin dictDvs "norvegien" "bleu"
constraint2egale dictDvs "violoniste" "jus"
constraint2estvoisin dictDvs "renard" "medecin"
constraint2estvoisin dictDvs "cheval" "diplomate"
```

On cherche toutes les solutions avec la fonction `allCSPSolutions`. On en trouve qu'une pour ce problème. 




## En résumé
Dans cet article, nous avons pu apprendre à utiliser la librairie `csp` pour utiliser les algorithmes de résolution de problème de satisfaction de contraintes en l'appliquant sur deux problèmes classiques.  


© mlier, 2025



