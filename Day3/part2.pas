// There's definitely a more optimized version of this code but I cannot possibly
// find the energy to spend any longer writing in this god-forsaken language

unit Part2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Solution(input: TStringList);

implementation

type
  Criterias = record
    Oxygen: char;
    CO2: char;
  end;

function FilterCriteria(list: TStringList; position: integer; criteria: char): TStringList;
var
  value: string;
begin
  result := TStringList.Create;
  for value in list do
    if value[position + 1] = criteria then
      result.Add(value);

  FilterCriteria := result;
end;

function FindCriterias(list: TStringList; position: integer): Criterias;
var
  sumOfBits: integer;
  criterias: Part2.Criterias;
  value: string;
begin
  sumOfBits := 0;
  for value in list do
    if value[position + 1] = '0' then
      sumOfBits := sumOfBits - 1
    else
      sumOfBits := sumOfBits + 1;

  if sumOfBits < 0 then
    begin
    criterias.Oxygen := '0';
    criterias.CO2 := '1'
    end
  else
    begin
    criterias.Oxygen := '1';
    criterias.CO2 := '0'
    end;

  FindCriterias := criterias;
end;

procedure Solution(input: TStringList);
var
  i: integer;
  length: integer;
  oxygenCriteria: char;
  co2Criteria: char;
  oxygenValues: TStringList;
  co2Values: TStringList;
  oxygenStopped: boolean;
  co2Stopped: boolean;
begin
  length := input[0].Length;
  oxygenValues := input;
  co2Values := input;
  oxygenStopped := false;
  co2Stopped := false;

  for i := 0 to length - 1 do
    begin
    if not oxygenStopped then
      begin
        oxygenCriteria := FindCriterias(oxygenValues, i).Oxygen;
        oxygenValues := FilterCriteria(oxygenValues, i, oxygenCriteria);
        if oxygenValues.Count = 1 then oxygenStopped := true;
      end;

    if not co2Stopped then
      begin
        co2Criteria := FindCriterias(co2Values, i).CO2;
        co2Values := FilterCriteria(co2Values, i, co2Criteria);
        if co2Values.Count = 1 then co2Stopped := true;
      end;
    end;

  // I can not be bothered to convert to decimal
  writeln('Part 2');
  writeln('Oxygen Rating: ', oxygenValues[0]);
  writeln('CO2 Rating: ', co2Values[0]);
end;

end.

