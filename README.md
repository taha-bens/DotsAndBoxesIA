# DotsAndBoxes

**Dots and Boxes** (ou ***Pipopipette***) est un jeu de stratégie inventé à la fin du XIX-ème siècle par des étudiants/mathématiciens français. Le principe est très simple : les cases d'une grille générée aléatoirement sont disputées par deux joueurs (traditionnellement, mais on peut jouer à autant qu'on veut pourvu que la grille soit assez grande). Ils posent chacun leur tour un mur sur la grille, et gagnent une case lorsqu'ils parviennent à placer le dernier de ses quatres murs. Le joueur ayant obtenu le plus de cases gagne la partie.



## Règles du jeu:

- Le jeu peut se jouer à **autant de personnes** que l'on souhaite.
- Chaque joueur reçoit un **identifiant** entre 0 et le nombre de joueurs - 1.
- Le jeu se joue sur une grille de cases carrées. Les joueurs jouent **chacun leur tour** en plaçant un mur sur la grille, ce qui définit le bord d'une case. 
- Si un joueur **complète une case**, c'est-à-dire que ses quatre murs ont été placés, alors elle est marquée de l'identifiant du joueur et ce **dernier reçoit un point** et le **droit de rejouer**. 
- Sinon, c'est au tour du prochain joueur de jouer. 
- Certaines cases sont considérées comme **bloquées**, où l'on ne peut pas mettre de murs. Cela apporte plus de variété et de stratégie dans les parties. 
- Le joueur ayant le **plus de points** lorsque la totalité de la grille est remplie gagne la partie.

cf : https://en.wikipedia.org/wiki/Dots_and_boxes


## Comment avons nous pensé à l'intégration des bots:

Nous avons suivi toute l'API que les professeurs ont donné en classe (Voir `ModelAPI.md` et APItype.ml).
Un bot est donc une fonction de type `game_view -> play`. Selon nous cela représente donc une stratégie qu'un joueur applique lors de son tour. Ainsi, pour implémenter un bot il vous suffit de crée une fonction de ce type et de la placer dans la liste `botlist` qui se trouve dans le fichier `main.ml`. Il sera pris en compte par le programme et pourra être intégré à une partie. Un exemple de bot stupid est donné dans le fichier `Bot.ml`

## Getting started :

#### Installer OPAM, OCaml et Dune  :

```shell
# Installation d'OCaml et d'OPAM
sudo apt update
sudo apt install ocaml opam

# Initialisation d'OPAM
opam init
eval $(opam env)

# Installation de Dune
opam install dune
```

#### Installation des dépendances :

```shell
opam install . --deps-only
```

#### Compiler et exécuter le projet :

```shell
# Compilation/exécution directe
dune build
./_build/default/src/main.exe

# Avec le launcher
./launcher.sh
```

#### Compiler et exécuter les tests :

```shell
# Compilation/exécution directe
dune build
dune runtest

# Avec le launcher
./launcher.sh t
```

Assurez vous d'avoir les bibliothèques suivantes : ounit2 qcheck qcheck-ounit

**Attention :** si on lance la commande `dune runtest` et que les tests sont validés, alors un deuxième `dune runtest` ne produira aucun résultat sur la sortie standard. Par défault Dune ne relance pas les tests s'ils ont déjà été validés et que le code n'a pas été modifié.

#### Fichiers de configuration :

Les fichiers `src/dune`, `test/dune`, `./dune-project`et `./dnb.opam`sont des fichiers de configuration. Les deux premiers permettent de décrire la compilation du projet et des tests, le troisième sert à définir la version de Dune utilisée et le dernier est relatif au package managment du projet. Ils sont presques vides pour l'instant mais le seront probablement moins plus tard.
