{ writeShellApplication, bash, nixStable }:
writeShellApplication {
  name = "aoc";
  runtimeInputs = [ bash nixStable ];
  text = builtins.readFile ./aoc;
}
