with builtins;
let
  # from lib/strings.nix
  toInt = str:
    let may_be_int = fromJSON str; in
    if isInt may_be_int
    then may_be_int
    else throw "Could not convert ${str} to int.";

  matches = regex: str: (match regex str) != null;
  years = filter (matches "[[:digit:]]{4}") (attrNames (readDir ./.));
  days = year:
    let
      ms = map (match "day([[:digit:]]{1,2})")
        (attrNames (readDir (./. + "/${year}")));
      matched = filter (x: x != null) ms;
      ds = map (x: toInt (head x)) matched;
    in sort lessThan ds;
in listToAttrs (map (y: {
    name = y;
    value = days y;
  }) years)
