program day3;
{$mode objfpc}

uses
  Classes, SysUtils, Part1;

const
  C_INPUT = 'input.txt';

var
  lines: TStringList;

begin
  lines := TStringList.Create;

  lines.LoadFromFile(C_INPUT);

  Part1.Solution(lines);

  lines.Free;
  readln;
end.

