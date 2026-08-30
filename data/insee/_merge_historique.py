#!/usr/bin/env python3
"""Union dé-dupliquée de deux versions de calendrier_historique.csv.

Utilisé par le workflow themes.yml : sur un runner CI neuf, plusieurs builds
concurrents modifient le même CSV. Plutôt qu'un merge git (qui a déjà laissé
des marqueurs `<<<<<<<` dans le fichier publié), on reconstruit le fichier
entièrement : lignes de `base` (version distante la plus fraîche) + lignes de
`ajout` (build courant) absentes de `base`.

Clé de dé-dup : (Données, date d'embargo YYYY-MM-DD) -- identique à cle() dans
calendrier.qmd. L'Horodatage (1re colonne) est ignoré dans la clé : un rendu
sans changement réel n'ajoute rien. On écarte aussi les repères "Semaine du …"
et les lignes dont la date n'est pas résolue (vieux CSV).

    python3 _merge_historique.py <base.csv> <ajout.csv> <sortie.csv>
"""
import csv
import re
import sys

_DATE = re.compile(r"^\d{4}-\d{2}-\d{2}")


def garder(r):
    return (len(r) == 4
            and not " ".join(r[1].split()).startswith("Semaine du")
            and _DATE.match(r[3].strip()) is not None)


def lire(chemin):
    try:
        with open(chemin, encoding="utf-8", newline="") as f:
            rows = list(csv.reader(f))
    except FileNotFoundError:
        return [], []
    if not rows:
        return [], []
    return rows[0], [r for r in rows[1:] if garder(r)]


def cle(r):
    return (" ".join(r[1].split()), r[3].strip()[:10])


def main():
    base_p, ajout_p, sortie_p = sys.argv[1], sys.argv[2], sys.argv[3]
    entete, base = lire(base_p)
    entete_a, ajout = lire(ajout_p)
    entete = entete or entete_a or ["Horodatage", "Données", "Mise à jour", "Date/heure"]

    vus, out = set(), []
    for r in base + ajout:
        k = cle(r)
        if k in vus:
            continue
        vus.add(k)
        out.append(r)

    with open(sortie_p, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(entete)
        w.writerows(out)
    print(f"_merge_historique: {len(base)} + {len(ajout)} -> {len(out)} lignes")


if __name__ == "__main__":
    main()
