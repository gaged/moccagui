unit configreader;

{$I mocca.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils;

function ReadConfig(const AFileName: string): boolean;

implementation

uses
  Dom,XMLRead,
  mocglb;

const
  Msg1 = 'Error reading config-file.' + #13;

var
  FileName: string;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;


function GetCmdNumber(const C: string): integer;
var
  i: integer;
begin
  Result:= -2;
  for i:= 0 to CmdNamesMax do
    begin
      if C = CmdNames[i].S then
        begin
          Result:= CmdNames[i].i;
          Exit;
        end;
    end;
end;

procedure ReadTools(Node: TDomNode);
var
  N: TDomNode;
  nv,nn: string;
  S: string;
  i,idx,GridW: integer;
begin
  {$ifdef DEBUG_CONFIG}
  writeln('reading Tools configuration...');
  {$endif}
  for i:= 0 to 8 do
    begin
      ToolCfg[i].Name:= '';
      ToolCfg[i].Width:= 0;
    end;
  if Node = nil then
    begin
      writeln('tool- section not found in config-file');
      Exit;
    end;
  N:= Node.FirstChild;
  while N <> nil do
    begin
      idx:= -1;
      GridW:= -1;
      S:= '';
      {$ifdef DEBUG_CONFIG}
      writeln('reading tool config item');
      {$endif}
      if not N.HasAttributes then
        Exit;
      if N.Attributes.Length < 1 then
        Exit;
      for i:= 0 to N.Attributes.Length - 1 do
        begin
          nn:= LowerCase(N.Attributes[i].NodeName);
          nv:= N.Attributes[i].NodeValue;
          if nn = 'index' then
            begin
              try
                idx:= StrToInt(nv);
              except
                writeln('"index" in tool config is not an integer');
                Exit;
              end;
              if (idx < 0) or (idx > 8) then
                begin
                  writeln('"index" in tool config is out of range');
                  Exit;
                end;
            end;
          if nn = 'width' then
            begin
              try
                GridW:= StrToInt(nv);
              except
                writeln('"width" in tool config is not an integer');
                Exit;
              end;
            end;
          if nn = 'title' then
            begin
              S:= nv;
            end;
        end;
        if (idx >= 0) and (idx <= 8) then
          begin
            if S <> '' then ToolCfg[idx].Name:= S;
            if GridW > 0 then ToolCfg[idx].Width:= GridW;
            {$ifdef DEBUG_CONFIG}
            writeln('read toolitem: ' + S);
            {$endif}
          end;
      N:= N.NextSibling;
    end;
end;

procedure ReadGlColorItem(N: TDOMNode);
var
  nn,nv: string;
  S: string;
  C: TGlColorItem;
  Value: Single;
  i: integer;
begin
  S:= '';
  if N = nil then ;
  if N.HasAttributes then
    if N.Attributes.Length > 0 then
      for i:= 0 to N.Attributes.Length - 1 do
        begin
          if N.Attributes[i] = nil then Exit;
          nn:= N.Attributes[i].NodeName;
          nv:= N.Attributes[i].NodeValue;
          {$ifdef DEBUG_CONFIG}
          writeln('Coloritem: ' + nn + #32 + nv);
          {$endif}
          if nn = 'name'
            then S:= nv
          else
            begin
              try
                Value:= StrToFloat(nv);
                if nn = 'r' then C.r:= Value else
                if nn = 'g' then C.g:= Value else
                if nn = 'b' then C.b:= Value else
                if nn = 'a' then C.a:= Value;
              except
                writeln('configfile <glcolors>: "' + nv + '" is not a valid float')
              end;
            end;
        end;
   if S = 'bg' then GlColors.bg:= C else
   if S = 'table' then GlColors.table:= C else
   if S = 'limits' then GlColors.limits:= C else
   if S = 'cone' then GlColors.cone:= C else
   if S = 'traverse' then GlColors.traverse:= C else
   if S = 'feed' then GlColors.feed:= C else
   if S = 'toolpath' then GlColors.toolpath:= C else
     writeln('invalid color name in config- file');
end;

procedure UpdateMenuItem(P: Pointer; N: TDOMNode);
var
  i,id,iCmd,iBmp: integer;
  nn,nv: string;
begin
  if (P = nil) or (N = Nil) then ;
  if N.HasAttributes then
    if N.Attributes.Length > 0 then
     begin
       id:= -1;
       for i:= 0 to N.Attributes.Length - 1 do
         begin
           if N.Attributes[i] = nil then Exit;
           if N.Attributes[i].NodeName = 'index' then
             try
               id:= StrToInt(N.Attributes[i].NodeValue);
             except
               writeln('menuitem: "index" is not an integer: '
                 + N.Attributes[i].NodeValue);
             end;
         end;
       if (id < 0) or (id > 9) then
         begin
           writeln('Menu- Item index out of range');
           Exit;
         end;
       for i:= 0 to N.Attributes.Length - 1 do
         begin
           if N.Attributes[i] = nil then break;
           nn:= UpperCase(N.Attributes[i].NodeName);
           nv:= N.Attributes[i].NodeValue;
           if nn = 'TEXT' then
             PButtonArray(P)^[id].S:= nv
           else
           if nn = 'CMD' then
           begin
             iCmd:= -2;
             if nv <> '' then
             iCmd:= GetCmdNumber(nv);
             if iCmd <> -2  then
               PButtonArray(P)^[id].T:= iCmd;
           end
           else
           if nn = 'BITMAP' then
           begin
             {$ifdef DEBUG_CONFIG}
             writeln('reading bitmap',nv);
             {$endif}
             iBmp:= -1;
             if nv <> '' then
               begin
                 iBmp:= AddBitmap(nv);
                 if iBmp = -1 then
                   writeln('error adding bitmap ',nv);
               end;
             PButtonArray(P)^[id].G:= iBmp;
           end;
         end;
     end;
end;

procedure ReadMenu(Node: TDomNode; P: Pointer);
var
  N: TDOMNode;
  i: integer;
  iItem: integer;
  S: string;
begin
  if (Node = nil) then
    begin
      writeln('invalid menu- node in xml-file');
      Exit;
    end;
  if (P = nil) then
    begin
      writeln('Invalid Menu- Pointer reading menu- node');
      Exit;
    end;
  for i:= 0 to 9 do
    begin
      PButtonArray(P)^[i].G:= -1;
      PButtonArray(P)^[i].S:= '';
      PButtonArray(P)^[i].T:= -1;
    end;
  N:= Node.FirstChild;
  if N = nil then
    begin
      writeln('error reading menu, no entrys found');
      Exit;
    end;
  while N <> nil do
    begin
      S:= UpperCase(N.NodeName);
      if S <> 'ITEM' then
        begin
          writeln('Node for menuitem is not a "item"');
          Break;
        end;
      UpdateMenuItem(P,N);
      N:= N.NextSibling;
  end;
end;

procedure ReadGlColors(Node: TDomNode);
var
  N: TDomNode;
begin
  if Node = nil then ;
  {$ifdef DEBUG_CONFIG}
  writeln('reading GlColors: ' + Node.NodeName);
  {$endif}
  N:= Node.FirstChild;
  while N <> nil do
    begin
      ReadGlColorItem(N);
      N:= N.NextSibling;
    end;
end;

function ReadConfig(const AFileName: string): Boolean;
var
  Doc:  TXMLDocument;
  Node: TDOMNode;
  S: string;

function GetNode(Name: string): TDOMNode;
begin
  Result:= Doc.DocumentElement.FindNode(Name);
end;

begin
  if AFileName = '' then
    begin
      writeln('Error: Cannot start without a valid config- file (config.xml)');
      Exit;
    end;
  {$ifdef DEBUG_CONFIG}
  writeln('reading config from ' + AFileName);
  {$endif}
  Result:= False;
  FileName:= AFileName;
  try
    ReadXMLFile(Doc,FileName);
  except
    writeln('cannot open config-file: ' + FileName);
    raise;
    Exit;
  end;
  if Doc = nil then
    begin
      writeln('error reading xml- file: ' + AFileName);
      Exit;
    end;
  try
    ReadGlColors(GetNode('glcolors'));
    ReadTools(GetNode('tools'));
    ReadMenu(GetNode('menujog'),@BtnDefJog);
    ReadMenu(GetNode('menumdi'),@BtnDefMdi);
    ReadMenu(GetNode('menurun'),@BtnDefRun);
    ReadMenu(GetNode('menuref'),@BtnDefJogRef);
    ReadMenu(GetNode('menutouchoff'),@BtnDefJogTouch);
    ReadMenu(GetNode('menutool'),@BtnDefJogTool);

    Node:= GetNode('background');
    if Node <> nil then
      begin
        S:= Node.NodeValue;
        if S <> '' then
          BackGroundImage:= ConfigDir + S
        else
          S:= 'None!';
        writeln('Background: ' + S);
      end;

  finally
    Doc.Free;
  end;
  Result:= True;
end;

end.

