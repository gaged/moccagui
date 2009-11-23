unit linecode;

interface

uses
  classes,sysutils,sim;

{$define debuglc}

function GetError(code: integer): string;
function ParseFile(const AFileName: PChar): integer;

{
type
  TLineCode = class
    constructor Create;
    destructor Destroy;
  private
    procedure CompileLimits;
  public
    function SetSoftLimits(x1,x2,y1,y2,z1,z2: Double): Boolan;
    function ParseFile(const AFileName: PChar): Boolean;
    function GetLastError: string;
  end;
}
       
implementation

const
  LINE_LEN = 256;

var
  Buffer: Array[0..LINE_LEN-1] of Char;
  LastError: string;

function GetError(Code: integer): string;
var
  s: string;
begin
  Buffer[0]:= #0;
  s:= '';
  interp_errortext(Code,PChar(Buffer),255);
  if Buffer[0] <> #0 then
    begin
      s:= s + PChar(Buffer) + #13#10;
      Buffer[0]:= #0;
      interp_line_text(Buffer,255);
      if Buffer[0] <> #0 then
        s:= s + PChar(Buffer);
    end;
  GetError:= s;
  {ifdef debuglc}
  if s <> '' then writeln(s);
  {endif} 
end;

function ParseFile(const AFileName: PChar): integer;
var
  status,i: integer;
begin
  ParseFile:= -1;
  if not Assigned(LineItems) then
    LineItems:= TList.Create
  else
    LineItems.Clear;
  if (AFileName = nil) then
    begin
      {ifdef debuglc}
      writeln('no file to parse!');
      {endif} 
      Exit;
    end;  
  if not Assigned(LineItems) then
    begin
      {ifdef debuglc}
      writeln('cannot write to list, nil!');
      {endif} 
      Exit;
    end;
  status:= interp_init; 
  if status <> INTP_OK then
    begin
      LastError:= GetError(status);
      ParseFile:= status;
      Exit;      
    end;
  status:= interp_open(AFileName);
  if status <> INTP_OK then
    begin
      LastError:= Geterror(status);
      ParseFile:= status;
      Exit;      
    end;
  while status = INTP_OK do
    begin
      status:= interp_read(nil);
      if status = INTP_OK then
        status:= interp_execute;
    end;
  if status <> INTP_OK then
    LastError:= Geterror(status);
  interp_close;
  {ifdef debuglc}
  writeln('Number of items: ' + IntToStr(LineItems.Count));
  {endif}
  ParseFile:= status;
end;

end. 
