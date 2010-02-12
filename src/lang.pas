unit lang;

{$mode objfpc}{$H+}

interface

uses
  classes, StdCtrls;

procedure ReadLangFile;
function  StrId(i: integer): string;
procedure SetCaption(ALabel: TLabel; Id: integer);

implementation

uses
  mocglb;

var
  StrList: TStringList;

procedure SetCaption(ALabel: TLabel; Id: integer);
var
  s: string;
begin
  S:= StrId(Id);
  if S <> '' then ALabel.Caption:= S;
end;

function StrId(i: integer): string;
begin
  if (LangFile <> '') and (i < StrList.Count) and (i >= 0) then
    Result:= StrList[i]
  else
    Result:= '';
end;

procedure ReadLangFile;
begin
  if LangFile = '' then
    Exit;
  if not Assigned(StrList) then
    StrList:= TStringList.Create;
  try
    StrList.LoadFromFile(LangFile);
  except
    LangFile:= '';
  end;
end;

end.

