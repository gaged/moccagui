unit moctool;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Classes, SysUtils,emc2pas;

type
  TTool = record
    pocket: integer;
    id: integer;
    zoffset: extended;
    xoffset: extended;
    diameter: extended;
    frontangle: extended;
    backangle: extended;
    orientation: integer;
    comment: string;
    {$ifdef VER_24}
    yoffset: extended;
    aoffset: extended;
    boffset: extended;
    coffset: extended;
    uoffset: extended;
    voffset: extended;
    woffset: extended;
    {$endif}
  end;

var
  Tools: array[0..CANON_TOOL_MAX] of TTool;

procedure LoadToolTable(const FileName: string);
procedure SaveToolTable(const FileName: string);

implementation

uses
  mocglb;

procedure ClearTool(var T: TTool);
begin
  T.pocket:= 0;
  T.id:= 0;
  T.zoffset:= 0.0;
  T.diameter:= 0.0;
  T.xoffset:= 0.0;
  T.frontangle:= 0.0;
  T.backangle:= 0.0;
  T.orientation:= 0;
  T.comment:= '';
  {$ifdef VER_24}
  T.yoffset:= 0;
  T.aoffset:= 0;
  T.boffset:= 0;
  T.coffset:= 0;
  T.uoffset:= 0;
  T.voffset:= 0;
  T.woffset:= 0;
  {$endif}
end;

procedure ClearTools;
var
  i: integer;
begin
  for i:= 0 to CANON_TOOL_MAX - 1 do
    ClearTool(Tools[i]);
end;

function EntryToToken(const T: TTool; id: integer): string;
begin
  Result:= '';
  if (id < 1) or (id > 8) then
    begin
      writeln('Toolfile: Token out of range');
      Exit;
    end;
  case id of
    1: Result:= IntToStr(T.pocket);
    2: Result:= IntToStr(T.id);
    3: Result:= FloatToStr(T.zoffset);
    4: Result:= FloatToStr(T.diameter);
  end;
end;

procedure SaveToolTable(const FileName: string);
var
  i: integer;
  s: string;
  L: TStringList;
  IsLathe: Boolean;
begin
  IsLathe:= False;
  L:= TStringList.Create;
  if not Assigned(L) then
    begin
      writeln('error creating toolfile- stringlist.');
      Exit;
    end;
  try
    for i:= 0 to CANON_TOOL_MAX - 1 do
      begin
        if Tools[i].orientation <> 0 then
          begin
            IsLathe:= True;
            Break;
          end;
      end;
    if IsLathe then
      begin
        writeln('Saving Lathe Toolfile: ' + FileName);
        s:= Format('%6s %4s %11s %11s %11s %12s %12s %7s %s',
          ['POCKET','FMS','ZOFFSET','XOFFSET',
           'DIAMETER','FRONTANGLE','BACKANGLE','ORIENT',
           'COMMENT']);
        L.Add(s);
        for i:= 1 to CANON_TOOL_MAX - 1 do
          if (Tools[i].id > 0) and (Tools[i].pocket > 0) then
            with Tools[i] do
              begin
                s:= Format('%6d %4d %11f %11f %11f %12f %12f %7d  %s',
                [pocket,id,zoffset,xoffset,diameter,frontangle,backangle,
                 orientation,comment]);
                L.Add(s);
              end;
      end
    else
      begin
        writeln('Saving Mill-Toolfile' + FileName);
        s:= Format('%6s %4s %11s %11s  %s',
          ['POCKET','FMS','LENGTH','DIAMETER','COMMENT']);
        L.Add(s);
        writeln(s);
        for i:= 1 to CANON_TOOL_MAX - 1 do
          if (Tools[i].id > 0) then
            with Tools[i] do
              begin
                s:= Format('%6d %4d %11f %11f  %s',[pocket,id,zoffset,diameter,Comment]);
                writeln(s);
                L.Add(s);
              end;
      end;
    if L.Count > 1 then
      L.SaveToFile(FileName);
  finally
    L.Free;
  end;
end;

function TokenToEntry(const s: string;var T: TTool; id: integer): boolean;
begin
  Result:= False;
  if s = '' then Exit;
  if (id < 1) or (id > 8) then
    begin
      writeln('Toolfile: Token out of range');
      Exit;
    end;
  try
    case id of
      1: T.pocket:= StrToInt(s);
      2: T.id:= StrToInt(s);
      3: T.zoffset:= StrToFloat(s);
      4: T.diameter:= StrToFloat(s);
    end;
    Result:= True;
  except
    Result:= False;
    writeln('Skipping Token: ' + s);
  end;
end;

procedure ScanLine(const s: string);
var
  i,C,StartPos,EndPos: integer;
  Token: string;
  Item: integer;
  Last: integer;
  Scanned: integer;
  T: TTool;
begin
  C:= Length(S);
  if C < 1 then Exit;
  i:= 1;
  Item:= 0;
  EndPos:= 1;
  if Vars.IsLathe then Last:= 8 else Last:= 4;
  Scanned:= 0;
  ClearTool(T);
  while i < C do
    begin
      // skip blanks,tabs etc
      while i <= C do if s[i] <= #32 then inc(i) else Break;
      StartPos:= i;
      while i <= C do if s[i] > #32 then inc(i) else Break;
      EndPos:= i;
      if (EndPos > StartPos) and (EndPos <= C) then
        begin
          inc(Item);
          if Item > Last then
            begin
              T.Comment:= Copy(s,StartPos,C-StartPos+1);
              i:= C;
            end
          else
            begin
              Token:= Copy(s,StartPos,EndPos-StartPos);
              if TokenToEntry(Token,T,Item) then
                inc(Scanned)
              else
                Break;
            end;
        end;
      inc(i);
    end;
  if Scanned = Last then
    begin
      if (T.pocket > 0) and (T.pocket < CANON_TOOL_MAX) then
        Tools[T.pocket]:= T
      else
        writeln('Tool- pocket number out of range: ' + IntToStr(T.Pocket));
    end;
end;

procedure LoadToolTable(const FileName: string);
var
  L: TStringList;
  i: integer;
begin
  ClearTools;
  L:= TStringList.Create;
  if not Assigned(L) then
    begin
      writeln('error creating toolfile- stringlist.');
      Exit;
    end;
  try
    L.LoadFromFile(FileName);
    if L.Count < 1 then
      writeln('Error: toolfile exists, but is empty.')
    else
       for i:= 0 to L.Count - 1 do
         ScanLine(L.Strings[i]);
  finally
    L.Free;
  end;
end;

initialization
  ClearTools;
end.

