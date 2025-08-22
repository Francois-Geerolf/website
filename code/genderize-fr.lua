-- genderize-fr.lua
-- A simple Pandoc filter to strip inclusive writing like auteur.rice → auteur

function Str(el)
  local t = el.text

  -- common inclusive patterns
  t = t:gsub("%.rice", "")       -- auteur.rice -> auteur
  t = t:gsub("·rice", "")        -- auteur·rice -> auteur
  t = t:gsub("/rice", "")        -- auteur/rice -> auteur
  t = t:gsub("%.e%.s", "s")      -- étudiant.e.s -> étudiants
  t = t:gsub("·e·s", "s")        -- étudiant·e·s -> étudiants
  t = t:gsub("/e/s", "s")        -- étudiant/e/s -> étudiants
  t = t:gsub("trice", "teur")    -- directrice -> directeur

  el.text = t
  return el
end

