# DotsAndBoxes

**Dots and Boxes** (ou ***Pipopipette***) est un jeu de stratégie inventé à la fin du XIX-ème siècle par des étudiants/mathématiciens français. Le principe est très simple : les cases d'une grille générée aléatoirement sont disputées par deux joueurs (traditionnellement, mais on peut jouer à autant qu'on veut pourvu que la grille soit assez grande). Ils posent chacun leur tour un mur sur la grille, et gagnent une case lorsqu'ils parviennent à placer le dernier de ses quatres murs. Le joueur ayant obtenu le plus de cases gagne la partie.

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

**Attention :** si on lance la commande `dune runtest` et que les tests sont validés, alors un deuxième `dune runtest` ne produira aucun résultat sur la sortie standard. Par défault Dune ne relance pas les tests s'ils ont déjà été validés et que le code n'a pas été modifié.

#### Fichiers de configuration :

Les fichiers `src/dune`, `test/dune`, `./dune-project`et `./dnb.opam`sont des fichiers de configuration. Les deux premiers permettent de décrire la compilation du projet et des tests, le troisième sert à définir la version de Dune utilisée et le dernier est relatif au package managment du projet. Ils sont presques vides pour l'instant mais le seront probablement moins plus tard.
