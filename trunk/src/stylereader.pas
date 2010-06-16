unit stylereader;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils;

function ReadXMLStyle(const AForm: TForm; AFileName: string): Boolean;

implementation

uses
  Dom,XMLRead,Graphics,StdCtrls,Controls,mocglb,mocbtn,mocled,mocslider;

const
  Msg1 = 'Error reading style-file.' + #13;

var
  FileName: string;
  CompName: string;
  CompClass: string;
  FormClass: string;
  IsPropertySection: Boolean;
  Form: TForm;
  Comp: TComponent;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure InitComponent;
begin
  if not IsPropertySection then
    Exit;
  Comp:= nil;
  if CompName = Form.Name then
    Comp:= Form
  else
    Comp:= Form.FindComponent(CompName);
  if Comp = nil then
    writeln('Component "' + CompName + '" not found');
end;

procedure AssignFontProperty(PropName,PropVal: string);
var
  AColor: Longint;
begin
  if Pos('FONT.',PropName) = 1 then
    with Comp as TControl do
      begin
        if PropName = 'FONT.NAME' then
          Font.Name:= PropVal
        else
        if PropName = 'FONT.HEIGHT' then
          Font.Height:= StrToInt(PropVal)
        else
        if PropName = 'FONT.COLOR' then
          begin
            if IdentToColor(PropVal,AColor) then
            Font.Color:= AColor;
          end;
        if PropName = 'FONT.STYLE' then
          begin
            if Pos('fsBold',PropVal) > 0 then
              Font.Style:= Font.Style + [fsBold];
          end;
      end;
end;

procedure AssignProperty(const PropName,PropVal: string);
var
  AColor: Longint;
  S: string;
  i: integer;

begin
  if Comp = nil then Exit;
  if Comp is TControl then
    with Comp as TControl do
      begin
        AssignFontProperty(PropName,PropVal);
        if PropName = 'HINT' then
          Hint:= PropVal
        else
        if PropName = 'CAPTION' then
          Caption:= PropVal
        else
        if PropName = 'COLOR' then
          begin
            if IdentToColor(PropVal,AColor) then
              Color:= AColor;
          end
        else
        if PropName = 'WIDTH' then
          Width:= StrToInt(PropVal)
        else
        if PropName = 'LEFT' then
          Left:= StrToInt(PropVal)
        else
        if PropName = 'TOP' then
          Top:= StrToInt(PropVal)
        else
        if PropName = 'HEIGHT' then
          Height:= StrToInt(PropVal)
        else
        if PropName = 'VISIBLE' then
          begin
            if (UpperCase(PropVal) = 'FALSE') or (Propval = '0') then
              Visible:= False
            else
            if (UpperCase(PropVal) = 'TRUE') or (PropVal = '1') then
              Visible:= True;
          end;        
      end;
  if Comp is TMocLed then
    with Comp as TMocLed do
      begin
        if PropName = 'LEDCOLOR' then
          begin
            if IdentToColor(PropVal,AColor) then
              LedColor:= AColor;
          end;
      end;
  if Comp is TLabel then
    with Comp as TLabel do
      begin
        if PropName = 'ALIGNMENT' then
          begin
            S:= UpperCase(PropVal);
            if S = 'LEFT' then Alignment:= taLeftJustify else
            if S = 'RIGHT' then Alignment:= taRightJustify else
            if S = 'CENTER' then Alignment:= taCenter;
          end;
      end;
  if Comp is TForm then
    with Comp as TForm do
      begin
        if PropName = 'IMAGE' then
          begin
            if PropVal <> '' then
              BackGroundImage:= ConfigDir + PropVal
            else
              BackGroundImage:= '';
          end;
        if PropName = 'BORDERSTYLE' then
          begin
            S:= UpperCase(PropVal);
            if S = 'NONE' then BorderStyle:= bsNone else
            if S = 'SINGLE' then Borderstyle:= bsSingle else
            if S = 'SIZEABLE' then BorderStyle:= bsSizeable;
          end;
      end;
  if Comp is TMocButton then
    with Comp as TMocButton do
    begin
      if PropName = 'BITMAP' then
        begin
          i:= AddBitmap(PropVal);
          if (i >= 0) and Assigned(GlobalBitmaps) then
            if Assigned(GlobalBitmaps.Objects[i]) then
              TMocButton(Comp).Glyph.Assign(TBitmap(GlobalBitmaps.Objects[i]));
        end;
      if PropName = 'COMMAND' then
        Command:= PropVal;
    end;
  if Comp is TSlider then
    if PropName = 'BARCOLOR' then
      with Comp as TSlider do
        begin
          if IdentToColor(PropVal,AColor) then
             BarColor:= AColor;
        end;
end;

procedure ParseNode(Node: TDOMNode);
var
  nn,vn,s: string;
begin
  if Node.NodeName = 'properties' then
    begin
      if (CompName <> '') and (CompClass <> '') then
        IsPropertySection:= True;
        InitComponent;
        Exit;
    end;
  if Node.NodeName = 'component' then
    begin
      if Node.Attributes.Length <> 2 then
        Error(Msg1 + 'illegal number of arguments for "component"');
      nn:= Node.Attributes[0].NodeValue;
      vn:= Node.Attributes[1].NodeValue;
      if (nn = '') or (vn = '') then
        Error(Msg1 + 'missing argument for "component"');
      IsPropertySection:= False;
      CompName:= nn;
      CompClass:= vn;
    end
  else
    if (CompName <> '') and (CompClass <> '') then
    begin
      if Node <> nil then
        begin
          //if not Node.HasAttributes then Exit;
          if (Node.Attributes.Length <> 2) or (not IsPropertySection) then
            Exit;
          nn:= UpperCase(Node.Attributes[0].NodeValue);
          vn:= Node.Attributes[1].NodeValue;
          AssignProperty(nn,vn);
        end;
    end
  else
    IsPropertySection:= False;
end;

procedure ReadStyleDocument;
var
  Doc:  TXMLDocument;
  iNode: TDOMNode;

  procedure ProcessNode(Node: TDOMNode);
  var
    cNode: TDOMNode;
    s: string;
  begin
    if Node = nil then Exit;
    ParseNode(Node);
    cNode := Node.FirstChild;
    while cNode <> nil do
    begin
      ProcessNode(cNode);
      cNode:= cNode.NextSibling;
    end;
  end;

begin
  ReadXMLFile(Doc,FileName);
  if Doc = nil then
    Exit;
  try
     iNode := Doc.DocumentElement.FirstChild;
     while iNode <> nil do
       begin
         ProcessNode(iNode);
         iNode := iNode.NextSibling;
       end;
  finally
    Doc.Free;
  end;
end;

function ReadXMLStyle(const AForm: TForm; AFileName: string): Boolean;
begin
  Result:= False;
  FileName:= AFileName;
  Form:= AForm;
  FormClass:= Form.ClassName;
  CompClass:= '';
  CompName:= '';
  IsPropertySection:= False;
  ReadStyleDocument;
end;

end.

