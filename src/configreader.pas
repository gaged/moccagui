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

var
  FileName: string;

const
  ERR_CONFIGREAD = 'Error reading the config-file: ';
  ERR_CONFIGDOCNIL = 'Error in configfile. No data read from ';
  ERR_NOSCRIPTSINCONFIG = 'No Scripts found in config-file.';
  ERR_CONFIGSCRIPTS = 'Error in config file: ';
  ERR_NOGLOBALCONFIG = 'Error: no global config entrys found.';

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure ReadGlobalItem(nn,nv: string);
begin
  if (nn = '') then Exit;
  try
    if nn = 'EDGEFINDERDIA' then EdgeFinderDia:= StrToFloat(nv) else
    if nn = 'VERBOSE' then Verbose:= (UpperCase(nv) = 'TRUE') or (nv = '1') else
    if nn = 'INITIALFULLSCREEN' then InitialFullscreen:= (UpperCase(nv) = 'TRUE') or (nv = '1');
  except
    on E: Exception do
      writeln('Error in config.xml: ' + E.Message);
  end;
end;

procedure ReadGlobals(Node: TDomNode);
var
  N: TDomNode;
  nn,nv: string;
begin
  if Node = nil then
    begin
      writeln(ERR_NOGLOBALCONFIG);
      Exit;
    end;
  N:= Node.FirstChild;
  writeln('reading global configuration');
  while N <> nil do
    begin
       if N.HasAttributes then
         if (N.Attributes.Length = 2) then
           begin
             nn:= UpperCase(N.Attributes[0].NodeValue);
             nv:= N.Attributes[1].NodeValue;
             ReadGlobalItem(nn,nv);
           end;         
      N:= N.NextSibling;
    end;
end;

procedure ReadScripts(Node: TDomNode);
var
  N: TDomNode;
  nn,nv: string;
  i: integer;
  iNr: integer;
  sName,sFile: string;
  iBmp: integer;
  NumScripts: integer;
begin
  NumScripts:= 0;
  HasScripts:= False;
  with BtnDefScripts[0] do
    begin
      T:= cmBACK;
      G:= -1;
      S:= '<';
    end;
  for i:= 1 to NumButtons - 1 do
    begin
      MocScripts[i].Name:= '';
      MocScripts[i].Script:= '';
      with BtnDefScripts[i] do
        begin
          T:= -1;
          G:= -1;
          S:= '';
        end;
    end;
  if Node = nil then
    begin
      writeln(ERR_NOSCRIPTSINCONFIG);
      Exit;
    end;
  N:= Node.FirstChild;
  while N <> nil do
    begin
      if not N.HasAttributes then Exit;
      if N.Attributes.Length < 1 then Exit;
      iNr:= -1;
      iBmp:= -1;
      sName:= '';
      sFile:= '';
      for i:= 0 to N.Attributes.Length - 1 do
        begin
          nn:= LowerCase(N.Attributes[i].NodeName);
          nv:= N.Attributes[i].NodeValue;
          if nn = 'index' then
            try
              iNr:= StrToInt(nv);
            except
              on E: Exception do
                begin
                  writeln(ERR_CONFIGSCRIPTS + NV + #13 + E.Message);
                  iNr:= -1;
                  Exit;

                end;
            end;
          if nn = 'name' then
            sName:= nv;
          if nn = 'script' then
            sFile:= nv;
          if nn = 'bitmap' then
            begin
               iBmp:= -1;
               if nv <> '' then
                 begin
                   iBmp:= AddBitmap(nv);
                   if iBmp = -1 then
                     writeln('error adding bitmap:',nv);
                 end;
            end;
         end;
      if (sName <> '') and (sFile <> '') then
        if (iNr >= 0) and (iNr < NumButtons) then
          begin
            MocScripts[iNr+1].Name:= sName;
            MocScripts[iNr+1].Script:= sFile;
            BtnDefScripts[iNr+1].T:= cmSCRIPTBASE + iNr + 1;
            BtnDefScripts[iNr+1].G:= iBmp;
            BtnDefScripts[iNr+1].S:= sName;
            writeln('Added Script: ' + sName +' ,File: '+ sFile);
            Inc(NumScripts);
          end;
      N:= N.NextSibling;
    end;
  HasScripts:= (NumScripts > 0);
end;

procedure ReadTools(Node: TDomNode);
var
  N: TDomNode;
  nv,nn: string;
  S: string;
  i,idx,GridW: integer;
  MaxIndex: integer;
begin
  {$ifdef DEBUG_CONFIG}
  writeln('reading Tools configuration...');
  {$endif}
  if Vars.IsLathe then
    MaxIndex:= LatheColumns
  else
    MaxIndex:= MillColumns;
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
              if (idx < 0) or (idx > MaxIndex) then
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
        if (idx >= 0) and (idx <= MaxIndex) then
          begin
            if S <> '' then
              TitleDef[idx]:= s;
            if GridW > 0 then WidthsDef[idx]:= GridW;
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
    on E: Exception do
      begin
        writeln(ERR_CONFIGREAD + FileName + #13 + E.Message);
        Exit;
      end;
  end;
  if Doc = nil then
    begin
      writeln(ERR_CONFIGDOCNIL + AFileName);
      Exit;
    end;
  try
    ReadGlColors(GetNode('glcolors'));
    ReadTools(GetNode('tools'));
    ReadScripts(GetNode('scripts'));
    ReadMenu(GetNode('menujog'),@BtnDefJog);
    ReadMenu(GetNode('menumdi'),@BtnDefMdi);
    ReadMenu(GetNode('menurun'),@BtnDefRun);
    ReadMenu(GetNode('menuref'),@BtnDefJogRef);
    ReadMenu(GetNode('menutouchoff'),@BtnDefJogTouch);
    ReadMenu(GetNode('menutool'),@BtnDefJogTool);
    ReadGlobals(GetNode('global'));
  finally
    Doc.Free;
  end;
  Result:= True;
end;

end.

