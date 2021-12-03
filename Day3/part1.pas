unit Part1;

{$mode objfpc}{$H+}

interface
uses
  Math, Classes, SysUtils;

procedure Solution(input: TStringList);

implementation

type
  GammaEpsilon = record
    gamma: integer;
    epsilon: integer;
  end;

function CalculateGammaEpsilon(input: TStringList): GammaEpsilon;
type
  BitCountArray = array of integer;
var
  gammaEpsilon: Part1.GammaEpsilon;
  length: integer;
  bitCount: BitCountArray;
  line: string;
  i: integer;
begin
  // Suppress warning
  bitCount := Default(BitCountArray);

  length := input[0].Length;
  setLength(bitCount, input[0].Length);

  for line in input do
    for i := 0 to length do
      // String are 1-indexed ???????
      if line[i + 1] = '0' then
        bitCount[i] := bitCount[i] - 1
      else
        bitCount[i] := bitCount[i] + 1;

  // Now that we have a list of bit counts, convert it to decimal by the following rules
  // If the bit count is negative, it's a 0
  // else it's a 1 (and vice-versa for Epsilon)

  gammaEpsilon.gamma := 0;
  gammaEpsilon.epsilon := 0;

  for i := 0 to length do
    if bitCount[i] > 0 then
      gammaEpsilon.gamma := gammaEpsilon.gamma + Trunc(power(2, length - 1 - i))
    else if bitCount[i] < 0 then
      gammaEpsilon.epsilon := gammaEpsilon.epsilon + Trunc(power(2, length - 1 - i))
    else
      raise EArgumentException.Create('Found an equal number of 0 and 1');

  CalculateGammaEpsilon := gammaEpsilon;
end;

procedure Solution(input: TStringList);
var
  result: Part1.GammaEpsilon;
begin
  result := CalculateGammaEpsilon(input);
  writeln('Part 1');
  writeln('Gamma: ', result.gamma);
  writeln('Epsilon: ', result.epsilon);
  writeln('Result: ', result.gamma * result.epsilon);
end;

end.

