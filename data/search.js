/* ---------------------------------------------------------------------------
 * data/search.js — client-side search over the whole /data catalog
 * ---------------------------------------------------------------------------
 * Reads data/search.json (built by code/_seo_search.R): every source, dataset
 * and theme. No framework, no CDN, no network beyond the one JSON fetch.
 *
 * Markup expected on the page:  <div id="dataset-search"></div>
 * Everything else (input, results list, styles) is created here.
 * ------------------------------------------------------------------------- */
(function () {
  "use strict";
  var mount = document.getElementById("dataset-search");
  if (!mount) return;

  var JSON_URL = "/data/search.json";
  var MAX = 25;

  // --- styles (once) -----------------------------------------------------
  if (!document.getElementById("dataset-search-css")) {
    var css = document.createElement("style");
    css.id = "dataset-search-css";
    css.textContent = [
      "#dataset-search{position:relative;margin:1rem 0 1.5rem}",
      "#dataset-search .ds-in{width:100%;box-sizing:border-box;padding:.6rem .8rem;",
      "font-size:1rem;border:1px solid var(--bs-border-color,#ccc);border-radius:.4rem;",
      "background:var(--bs-body-bg,#fff);color:var(--bs-body-color,#222)}",
      "#dataset-search .ds-in:focus{outline:2px solid #2c7fb8;outline-offset:1px}",
      "#dataset-search .ds-res{position:absolute;z-index:50;left:0;right:0;margin-top:.25rem;",
      "max-height:60vh;overflow-y:auto;background:var(--bs-body-bg,#fff);",
      "border:1px solid var(--bs-border-color,#ccc);border-radius:.4rem;",
      "box-shadow:0 6px 24px rgba(0,0,0,.14)}",
      "#dataset-search .ds-res:empty{display:none}",
      "#dataset-search a.ds-row{display:flex;gap:.5rem;align-items:baseline;",
      "padding:.45rem .7rem;text-decoration:none;color:inherit;border-bottom:1px solid rgba(0,0,0,.06)}",
      "#dataset-search a.ds-row:last-child{border-bottom:0}",
      "#dataset-search a.ds-row.sel,#dataset-search a.ds-row:hover{background:#e8f2f8}",
      "#dataset-search .ds-k{flex:0 0 auto;font-size:.68rem;text-transform:uppercase;",
      "letter-spacing:.03em;padding:.08rem .35rem;border-radius:.25rem;background:#d9e6ee;color:#245}",
      "#dataset-search .ds-k.d{background:#e5e5e5;color:#444}",
      "#dataset-search .ds-k.t{background:#e6efd9;color:#354a1c}",
      "#dataset-search .ds-t{flex:1 1 auto;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}",
      "#dataset-search .ds-m{flex:0 0 auto;font-size:.75rem;color:#888}",
      "#dataset-search .ds-hint{padding:.5rem .7rem;color:#888;font-size:.85rem}",
      "@media (prefers-color-scheme:dark){",
      "#dataset-search a.ds-row.sel,#dataset-search a.ds-row:hover{background:#1f2d36}",
      "#dataset-search .ds-k{background:#2a3f4c;color:#bcd}",
      "#dataset-search .ds-k.d{background:#3a3a3a;color:#ccc}",
      "#dataset-search .ds-k.t{background:#33401f;color:#cde0ac}}"
    ].join("");
    document.head.appendChild(css);
  }

  // --- DOM -------------------------------------------------------------
  var input = document.createElement("input");
  input.type = "search";
  input.className = "ds-in";
  input.placeholder = "Search datasets, sources, themes…";
  input.setAttribute("aria-label", "Search the data catalog");
  input.autocomplete = "off";
  var res = document.createElement("div");
  res.className = "ds-res";
  res.setAttribute("role", "listbox");
  mount.appendChild(input);
  mount.appendChild(res);

  // --- data -----------------------------------------------------------
  var ITEMS = null, LOADING = false, PENDING = null;
  var norm = function (s) {
    s = (s || "").toLowerCase();
    return s.normalize ? s.normalize("NFD").replace(/[̀-ͯ]/g, "") : s;
  };

  function load() {
    if (ITEMS || LOADING) return;
    LOADING = true;
    var tryUrls = [JSON_URL, "search.json"], i = 0;
    (function next() {
      if (i >= tryUrls.length) { LOADING = false; render(); return; }
      fetch(tryUrls[i++], { credentials: "omit" })
        .then(function (r) { if (!r.ok) throw 0; return r.json(); })
        .then(function (j) {
          ITEMS = (j.items || []).map(function (it) {
            it._t = norm(it.t); it._s = norm(it.s);
            it._d = norm(it.d || ""); it._c = norm(it.c || "");
            return it;
          });
          LOADING = false;
          if (PENDING !== null) { var q = PENDING; PENDING = null; run(q); }
        })
        .catch(next);
    })();
  }

  // --- synonyms (data says "consumer price index", users type "inflation") --
  var SYN = [
    ["inflation", "cpi", "hicp", "ipc", "ipch", "price index", "consumer price",
     "prix a la consommation", "deflator", "deflateur"],
    ["gdp", "pib", "gross domestic", "produit interieur", "national accounts",
     "comptes nationaux", "value added", "valeur ajoutee"],
    ["unemployment", "chomage", "jobless", "unemployed"],
    ["employment", "emploi", "payroll", "jobs", "hours worked"],
    ["wage", "wages", "salaire", "salaires", "earnings", "compensation", "remuneration"],
    ["interest rate", "interest rates", "taux d'interet", "yield", "policy rate",
     "bond yield", "taux directeur"],
    ["exchange rate", "taux de change", "forex", "currency"],
    ["trade", "commerce", "exports", "imports", "exportations", "importations",
     "balance of payments", "balance des paiements"],
    ["debt", "dette", "borrowing", "liabilities", "deficit"],
    ["housing", "immobilier", "house price", "logement", "loyer", "rent", "mortgage"],
    ["population", "demographic", "demographie", "demography", "fertility", "mortality"],
    ["poverty", "pauvrete", "inequality", "inegalite", "income distribution"],
    ["productivity", "productivite", "tfp", "output per"],
    ["investment", "investissement", "fbcf", "capital formation"],
    ["consumption", "consommation", "household spending"],
    ["energy", "energie", "electricity", "gas", "oil", "petrole", "power"],
    ["emissions", "co2", "greenhouse", "carbon", "ges", "climat", "climate"],
    ["interest", "monetary policy", "politique monetaire", "central bank"],
    ["us", "usa", "united states", "american", "america"],
    ["uk", "united kingdom", "britain", "british", "england"],
    ["eu", "european union", "europe", "european"]
  ];
  var SYN_MAP = {};
  SYN.forEach(function (g) { g.forEach(function (w) { SYN_MAP[w] = g; }); });
  function variants(tok) { return SYN_MAP[tok] || [tok]; }

  // --- scoring --------------------------------------------------------
  var KIND_BONUS = { s: 2.2, t: 1.6, d: 0 };
  function scoreItem(it, toks) {
    var total = 0;
    for (var i = 0; i < toks.length; i++) {
      var vs = variants(toks[i]), best = 0;
      var pools = [[it._s, 3], [it._d, 3], [it._t, 1.4], [it._c, 0.9]];
      for (var p = 0; p < pools.length; p++) {
        var hay = pools[p][0]; if (!hay) continue;
        for (var v = 0; v < vs.length; v++) {
          var tk = vs[v];
          // the keyword blob, and any very short token (us, uk, g7...), only
          // count on a word boundary, so "us" does not match inside a word
          var wb = p === 3 || tk.length <= 2;
          var pos = wb ? (" " + hay).indexOf(" " + tk) : hay.indexOf(tk);
          if (pos < 0) continue;
          var exact = v === 0 ? 1 : 0.6;   // synonym hit worth a bit less
          var s = pools[p][1] * exact
                * (pos === 0 && p !== 3 ? 2.2 : 1)
                * (hay === tk ? 1.7 : 1);
          if (s > best) best = s;
        }
      }
      if (best === 0) return -1;          // every token must hit somewhere
      total += best;
    }
    total += KIND_BONUS[it.k] || 0;
    if (it.r) total += Math.min(it.r, 5) * 0.25;   // audience nudge
    return total;
  }

  function run(q) {
    q = q.trim();
    if (!q) { res.innerHTML = ""; sel = -1; return; }
    if (!ITEMS) { PENDING = q; res.innerHTML =
      '<div class="ds-hint">Loading catalog…</div>'; load(); return; }
    var toks = norm(q).split(/\s+/).filter(Boolean);
    var out = [];
    for (var i = 0; i < ITEMS.length; i++) {
      var sc = scoreItem(ITEMS[i], toks);
      if (sc >= 0) out.push([sc, ITEMS[i]]);
    }
    out.sort(function (a, b) { return b[0] - a[0]; });
    var n = out.length;
    out = out.slice(0, MAX);
    if (!out.length) {
      res.innerHTML = '<div class="ds-hint">No match for “' +
        q.replace(/[<>&]/g, "") + "”</div>";
      sel = -1; return;
    }
    var label = { s: "source", d: "dataset", t: "theme" };
    var html = out.map(function (r) {
      var it = r[1];
      return '<a class="ds-row" role="option" href="' + it.u + '">' +
        '<span class="ds-k ' + it.k + '">' + label[it.k] + "</span>" +
        '<span class="ds-t">' + esc(it.t) + "</span>" +
        (it.m ? '<span class="ds-m">' + it.m + "</span>" : "") + "</a>";
    }).join("");
    if (n > MAX) html += '<div class="ds-hint">' + (n - MAX) +
      " more… refine your search</div>";
    res.innerHTML = html;
    sel = -1;
  }

  function esc(s) {
    return String(s).replace(/[<>&"]/g, function (c) {
      return { "<": "&lt;", ">": "&gt;", "&": "&amp;", '"': "&quot;" }[c];
    });
  }

  // --- events --------------------------------------------------------
  var sel = -1, timer = null;
  input.addEventListener("input", function () {
    clearTimeout(timer);
    var q = input.value;
    timer = setTimeout(function () { run(q); }, 90);
  });
  input.addEventListener("focus", load);
  input.addEventListener("keydown", function (e) {
    var rows = res.querySelectorAll("a.ds-row");
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      if (!rows.length) return;
      e.preventDefault();
      sel += e.key === "ArrowDown" ? 1 : -1;
      if (sel < 0) sel = rows.length - 1;
      if (sel >= rows.length) sel = 0;
      rows.forEach(function (r, i) { r.classList.toggle("sel", i === sel); });
      rows[sel].scrollIntoView({ block: "nearest" });
    } else if (e.key === "Enter") {
      if (sel >= 0 && rows[sel]) { window.location.href = rows[sel].href; }
      else if (rows[0]) { window.location.href = rows[0].href; }
    } else if (e.key === "Escape") {
      res.innerHTML = ""; sel = -1;
    }
  });
  document.addEventListener("click", function (e) {
    if (!mount.contains(e.target)) { res.innerHTML = ""; sel = -1; }
  });
})();
