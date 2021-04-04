{ pkgs ? import ./pkgs.nix }:
let
  inherit (pkgs.lib) concatStringsSep toInt toLower;
  unlines = concatStringsSep "\n";
  asColumns = xs: "| " + concatStringsSep " | " xs + " |";
  count = e: xs: builtins.length (builtins.filter (y: y == e) xs);
  replicate = e: n: builtins.genList (_: e) n;
  drvs = import ./derivations.nix { inherit pkgs; };
  langs = import ./langs.nix { inherit pkgs; };
  days = import ./days.nix;
  allDayNums = builtins.genList (x: x + 1) 25;
  years = builtins.sort (x: y: builtins.lessThan (toInt x) (toInt y))
    (builtins.attrNames days);
  yearDayLangResult = lang: y: d:
    let
      drv = ((drvs."year${y}" or { })."day${
          toString d
        }" or { }).${lang.extension} or null;
    in if drv == null then
      "X"
    else
      builtins.substring 0 1 (builtins.readFile "${drv}/status");
  langStats = lang:
    map (y: count "G" (map (yearDayLangResult lang y) allDayNums)) years;
  overview = let
    colEntry = n: if n == 25 then "✓" else "${toString n}/25";
    header = asColumns ([ "" ] ++ years);
    headerSep = asColumns (replicate "------" (1 + builtins.length years));
    langRow = lang:
      let rowHead = "[${lang.name}](#${toLower lang.name})";
      in asColumns ([ rowHead ] ++ (map colEntry (langStats lang)));
    ls = [ header headerSep ] ++ map langRow (builtins.attrValues langs);
  in unlines ls;
  languageSummary = lang:
    let
      header = asColumns ([ "Day \\\\ Year" ] ++ years);
      headerSep = asColumns (replicate "------" (1 + builtins.length years));
      colEntry = y: d:
        let r = yearDayLangResult lang y d;
        in if r == "G" then
          "[✓](./${toString y}/day${toString d}/run.${lang.extension})"
        else
          "";
      dayRow = d: asColumns ([ (toString d) ] ++ map (y: colEntry y d) years);
      ls = [ header headerSep ] ++ map dayRow allDayNums;
    in unlines ls;
  languageSummaries = let
    xs = map (l: [ "## ${l.name}" (languageSummary l) ])
      (builtins.attrValues langs);
  in unlines (builtins.concatLists xs);

  template = builtins.readFile ./README.base.md;
in pkgs.writeText "README.md"
(builtins.replaceStrings [ "$$$COMPLETION$$$" "$$$LANGUAGE_COMPLETION$$$" ] [
  overview
  languageSummaries
] template)
