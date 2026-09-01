#!/usr/bin/env bash
# Télécharge un petit logo (favicon) pour chaque organisation de data/index.qmd
# ET du catalogue `datasets` (colonne source) dans data/logos/<id>.png.
# Sources essayées dans l'ordre : unavatar.io (agrège Google/DuckDuckGo/
# favicon/...), puis Google, puis DuckDuckGo.
# À relancer manuellement quand on ajoute une source ; les PNG sont committés.
# Sans logo à ce jour : gfd (globalfinancialdata : aucune favicon exposée) ;
# et les "sources" qui ne sont pas des organismes (crosswalks, elasticity,
# log, pareto, phillips, pouvoir-achat) -> source_logo_md() renvoie "".
# data/logos/meteo.png : pictogramme cloud-sun de Bootstrap Icons (teinté),
# posé à la main -- ne pas le récupérer via favicon (d'où l'absence ici).
# data/logos/insee.png : emblème actuel de l'Insee (3 barres, identité 2024
# commune aux SSM Dares/Drees/...), converti de
# https://www.insee.fr/static/img/favicon.svg -- unavatar.io sert encore
# l'ancien logo, donc posé à la main lui aussi (d'où l'absence ici).
set -u
cd "$(dirname "$0")"
mkdir -p logos

# id domaine
MAP=$(cat <<'EOF'
ameco ec.europa.eu
bea bea.gov
bis bis.org
bls bls.gov
boe bankofengland.co.uk
buba bundesbank.de
cbp census.gov
census census.gov
comtrade comtrade.un.org
ademe ademe.fr
aft aft.gouv.fr
cgedd igedd.developpement-durable.gouv.fr
dgafp fonction-publique.gouv.fr
iea iea.org
notaires notaires.fr
opendata data.gouv.fr
rba rba.gov.au
shiller shillerdata.com
dallas-fed dallasfed.org
destatis destatis.de
ec ec.europa.eu
ecb ecb.europa.eu
eurostat ec.europa.eu
fhfa fhfa.gov
frb federalreserve.gov
frb-ny newyorkfed.org
freddie freddiemac.com
gfd globalfinancialdata.com
ilo ilo.org
imf imf.org
investing investing.com
john-fernald-tfp frbsf.org
maddison rug.nl
oecd oecd.org
ons ons.gov.uk
pwt rug.nl
statjp stat.go.jp
uk gov.uk
un un.org
us usa.gov
wb worldbank.org
wdi worldbank.org
wid wid.world
yahoo finance.yahoo.com
zillow zillow.com
compustat spglobal.com
crsp crsp.org
dbnomics db.nomics.world
rdb db.nomics.world
wrds wharton.upenn.edu
acoss urssaf.org
acpr acpr.banque-france.fr
bdf banque-france.fr
cepii cepii.fr
citepa citepa.org
cre cre.fr
dares dares.travail-emploi.gouv.fr
douanes douane.gouv.fr
drees drees.solidarites-sante.gouv.fr
dvf data.gouv.fr
ined ined.fr
ipp ipp.eu
mtes ecologie.gouv.fr
olap observatoire-des-loyers.fr
rei impots.gouv.fr
sdes statistiques.developpement-durable.gouv.fr
fred stlouisfed.org
quandl nasdaq.com
EOF
)

ok=0; ko=0
while read -r id dom; do
  [ -z "$id" ] && continue
  out="logos/$id.png"
  for url in \
    "https://unavatar.io/$dom?fallback=false" \
    "https://www.google.com/s2/favicons?domain=$dom&sz=64" \
    "https://icons.duckduckgo.com/ip3/$dom.ico"; do
    code=$(curl -sL -o "$out.tmp" -w '%{http_code}' "$url")
    sz=$(wc -c < "$out.tmp" 2>/dev/null || echo 0)
    if [ "$code" = "200" ] && [ "$sz" -gt 100 ]; then
      mv "$out.tmp" "$out"
      echo "OK   $id  <- $dom  ($sz b)"
      ok=$((ok+1)); id=""; break
    fi
  done
  rm -f "$out.tmp"
  if [ -n "$id" ]; then echo "MISS $id  ($dom)"; ko=$((ko+1)); fi
done <<< "$MAP"

echo "---"
echo "$ok logos, $ko manquants"
