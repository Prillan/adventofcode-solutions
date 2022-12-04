{ python310, bash, nixStable }:
python310.pkgs.buildPythonApplication {
  name = "aoc";
  propagatedBuildInputs = [ bash nixStable ] ++ (with python310.pkgs; [ requests ]);
  src = ./aoc-py;
}
