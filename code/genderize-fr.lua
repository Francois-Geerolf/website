-- genderize-fr.lua
-- Quarto/Pandoc filter to pick masculine/feminine French role nouns
-- from YAML: either `gender: male|female` or `author.gender: male|female`.

local gender = "neutral"

-- Get a simple string from a MetaValue
local function meta_str(v)
  if not v then return nil end
  return pandoc.utils.stringify(v):lower()
end

function Meta(m)
  -- 1) top-level gender: male/female
  local g = meta_str(m.gender)

  -- 2) try author.gender (author can be list or map)
  if not g and m.author then
    if m.author.t == "MetaList" then
      -- first author wins
      if #m.author > 0 then
        g = meta_str(m.author[1].gender)
      end
    else
      -- single author map
      g = meta_str(m.author.gender)
    end
  end

  if g == "male" or g == "m" then gender = "male"
  elseif g == "female" or g == "f" then gender = "female"
  else gender = "neutral" end
end

-- Helper: capitalize first letter if original was capitalized
local function with_case(base, cap)
  if cap == "A" or cap == "C" or cap == "D" or cap == "P" or cap == "L" then
    return base:sub(1,1):upper() .. base:sub(2)
  end
  return base
end

-- Role pairs you want to control.
-- Extend this list as needed.
local roles = {
  -- { masculine, feminine }
  {"auteur",    "autrice"},
  {"lecteur",   "lectrice"},
  {"directeur", "directrice"},
  {"acteur",    "actrice"},
  {"chercheur", "chercheuse"},
  {"professeur","professeure"},
}

-- Build replacers once
local patterns = {}

local function add_role_patterns(masc, fem)
  -- Inclusive connectors: ".", "·", "/"
  -- Optional plural: s
  -- We also normalize the opposite-gender form to the chosen one.
  local function add(pat, repl)
    table.insert(patterns, {pat, repl})
  end

  -- Inclusive forms (masc<sep>fem) → chosen
  -- e.g., auteur.rice(s?), auteur·rice(s?), auteur/rice(s?)
  add("([%l%u])"..masc:sub(1,1) .. masc:sub(2) .. "[%./·]?" ..
      fem:gsub("^"..masc, "") .. "(s?)",
      function(A, s)
        if gender == "female" then
          return with_case(fem, A) .. (s or "")
        elseif gender == "male" then
          return with_case(masc, A) .. (s == "s" and "s" or "")
        else
          -- neutral: default to masculine (customize if you prefer)
          return with_case(masc, A) .. (s == "s" and "s" or "")
        end
      end)

  -- Pure feminine → chosen
  add("([%l%u])"..fem.."(s?)",
      function(A, s)
        if gender == "male" then
          return with_case(masc, A) .. (s == "s" and "s" or "")
        elseif gender == "female" then
          return with_case(fem, A) .. (s or "")
        else
          return with_case(masc, A) .. (s == "s" and "s" or "")
        end
      end)

  -- Pure masculine → chosen (only switch if female requested)
  add("([%l%u])"..masc.."(s?)",
      function(A, s)
        if gender == "female" then
          return with_case(fem, A) .. (s == "s" and "s" or "")
        else
          return with_case(masc, A) .. (s == "s" and "s" or "")
        end
      end)
end

for _, pair in ipairs(roles) do
  add_role_patterns(pair[1], pair[2])
end

function Str(el)
  local t = el.text

  -- Apply all role substitutions
  for _, rule in ipairs(patterns) do
    local pat, repl = rule[1], rule[2]
    t = t:gsub(pat, repl)
  end

  -- Also catch the explicit separators for inclusive forms
  -- e.g. "Auteur·rice", "Auteur/rice", "Auteur.rice"
  -- Already handled above, but this extra pass helps for edge-cases in tokenization.
  el.text = t
  return el
end
