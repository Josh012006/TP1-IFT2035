# TP 1 – Implémentation d’un interpréteur Psil en Haskell

## Description
Ce projet consiste à compléter un interpréteur pour un petit langage fonctionnel de type Lisp, appelé **Psil**, écrit en Haskell. L’objectif principal est de :
1. Éliminer le « sucre syntaxique » (s2l) ;
2. Implémenter la fonction d’évaluation (eval) ;
3. Fournir au moins cinq tests Psil dans `tests.psil` pour valider votre implémentation ;
4. Rédiger un rapport (`rapport.tex`) décrivant votre démarche (problèmes rencontrés, choix effectués, etc.).

Le langage Psil supporte :
- Les entiers signés et les variables ;
- Les appels de fonctions (forme curried) ;
- Les abstractions et définitions locales (`abs`, `def`) ;
- Les opérations arithmétiques prédéfinies (`+`, `–`, `*`, `/`, …) ;
- Les constructeurs et le filtrage sur données (pattern matching) ;
- Les expressions conditionnelles sous forme de `filter` (équivalent de `if`).

## Structure du projet
├── psil.hs            # Code source Haskell fourni ; à compléter (s2l et eval)
├── exemples.psil      # Un fichier qui contient un exemple de code psil pour expliquer la syntaxe
├── tests.psil         # Fichier de tests Psil : contient ≥ 5 exemples commentés
├── rapport.tex        # Rapport (LaTeX) décrivant l’expérience et les choix
└── README.md          # Ce fichier

## Compilation et exécution
```bash
    ghci> :load "psil.hs"
    ghci> run "tests.psil"
```

## Auteurs

- **Josué Mongan**

GitHub : [Josh012006](https://github.com/Josh012006)

- **David Stanescu**

GitHub : [DavidStanescu13](https://github.com/DavidStanescu13)