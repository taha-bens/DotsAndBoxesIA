#!/bin/bash

if [ $# -eq 0 ]; then
    dune build
    ./_build/default/bin/main.exe
elif [ $# -gt 1 ]; then
    echo "Erreur : exécuter './launcher.sh' ou './launcher.sh t'"
elif [ "$1" == "t" ]; then
    dune build
    dune runtest;
else
    echo "Erreur : exécuter './launcher.sh' ou './launcher.sh t'"
fi