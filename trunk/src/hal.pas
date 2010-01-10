unit hal;

{$mode objfpc}{$H+}

interface

uses
  dynlibs,crt,sysutils,classes,ctypes;

const
  HAL_SUCCESS = 0;
  HalLibName = 'libemchal.so';
  HalName = 'moc';

type
  THalBit = Byte;
  PHalBit = ^THalBit;
  PPHalBit = ^PHalBit;
  THalU32 = cuint32;
  PHalU32 = ^THalU32;
  PPHalU32 = ^PHalU32;
  THalS32 = cint32;
  PHalS32 = ^THalS32;
  PPHalS32 = ^PHalS32;
  THalFloat = double;
  PHalFloat = ^THalFloat;
  PPHalFloat = ^PHalFloat;
  TPinDir = (HAL_IN:= 16,HAL_OUT:= 32,HAL_IO:= 16 or 32);
  TPinType = (ptBIT,ptUINT,ptSINT,ptFLOAT);

type
  PHalStruct = ^THalStruct;
  THalStruct = packed record
    Increment: ^THalFLoat;
    x,y,z,b,c: ^THalBit;
    cmd: ^THalU32;
    skewx: ^THalFloat;
    skewy: ^THalFloat;
    skewz: ^THalFloat;
    feed: ^THalFloat;
    maxvel: ^THalFloat;
    jogvel: ^THalFloat;
  end;

function InitHal(Name: string): boolean;
procedure LoadHal;
procedure FreeHal;

function hal_init(const name: PChar): longint; cdecl; external HalLibName;
function hal_ready(compid: integer): longint; cdecl; external HalLibName;
function hal_exit(compid: integer): longint; cdecl; external HalLibName;
function hal_malloc(size: integer): pointer; cdecl; external HalLibName;
function halpr_find_pin_by_name(name: pchar): pointer; cdecl; external HalLibName;

function hal_pin_bit_new(const name: PChar; dir: TPinDir;
  Data: PPHalBit; comp_id: cint): cint; cdecl; external HalLibName;

function hal_pin_float_new(const name: PChar; dir: TPinDir;
  Data: PPHalFloat; comp_id: cint): cint; cdecl; external HalLibName;

function hal_pin_u32_new(const name: PChar; dir: TPinDir;
  Data: PPHalU32; comp_id: cint): cint; cdecl; external HalLibName;

function hal_pin_s32_new(const name: PChar; dir: TPinDir;
  Data: PPHalS32; comp_id: cint): cint; cdecl; external HalLibName;

procedure SetHalJogAxis(Axis: Char);
function  GetHalJogAxis: Char;
procedure SetHalJogIncrement(Incr: double);
function GetHalJogIncrement: double;
function GetSkew(Axis: integer): double;
procedure SetSkew(Axis: integer; Skew: double);

implementation

uses
  mocglb;

var
  HalHandle: Pointer;
  HalData: PHalStruct;
  CompId: cInt;

procedure SetHalJogAxis(Axis: Char);
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  HalData^.x^:= Byte(Axis = 'X');
  HalData^.y^:= Byte(Axis = 'Y');
  HalData^.z^:= Byte(Axis = 'Z');
  HalData^.b^:= Byte(Axis = 'B');
  HalData^.c^:= Byte(Axis = 'C');
end;

function  GetHalJogAxis: Char;
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  if HalData^.x^ <> 0 then Result:= 'X' else
  if HalData^.y^ <> 0 then Result:= 'Y' else
  if HalData^.z^ <> 0 then Result:= 'Z' else
  if HalData^.b^ <> 0 then Result:= 'B' else
  if HalData^.c^ <> 0 then Result:= 'C' else
    Result:= #0;
end;

procedure SetHalJogIncrement(Incr: double);
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  HalData^.Increment^ := Incr;
end;

function GetHalJogIncrement: double;
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  Result:= HalData^.Increment^;
end;

function GetSkew(Axis: integer): double;
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  if Axis = 0 then Result:= HalData^.skewx^ else
  if Axis = 1 then Result:= HalData^.skewy^ else
  if Axis = 2 then Result:= HalData^.skewz^ else
    Result:= 0;
end;

procedure SetSkew(Axis: integer; Skew: double);
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  if Axis = 0 then HalData^.skewx^:= skew else
  if Axis = 1 then HalData^.skewy^:= skew else
  if Axis = 2 then HalData^.skewz^:= skew;
end;

function CheckPin(RetVal: integer): Boolean;
begin
  Result:= (RetVal = HAL_SUCCESS);
  if not Result then
    begin
      hal_exit(CompId);
      CompId:= 0;
      writeln('exporting pin to hal failed!');
    end;
end;

function ExportPinBit(pin: PPHalBit; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_bit_new(PChar(name),HAL_OUT,pin,compid)));
end;

function ExportPinFloat(pin: PPHalFloat; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_float_new(PChar(name),HAL_OUT,pin,compid)));
end;

function ExportPinU32(pin: PPHalU32; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_u32_new(PChar(name),HAL_OUT,pin,compid)));
end;

function ExportPinS32(pin: PPHalS32; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_s32_new(PChar(name),HAL_OUT,pin,compid)));
end;

procedure LoadHal;
var
  LibName: string;
begin
  LibName:= emc2home + '/lib/' + HalLibName;
  HalHandle:= Pointer(LoadLibrary(PChar(LibName)));
  if HalHandle = nil then
    raise Exception.Create('cannot find library ' + LibName);
end;

procedure FreeHal;
begin
  if CompId > 0 then hal_exit(CompId);
  if HalHandle <> nil then
    FreeLibrary(Cardinal(HalHandle));
  HalHandle:= nil;
end;

procedure InitPins;
begin
  if not ExportPinBit(@HalData^.x,'moc.jog.x') then Exit;
  if not ExportPinBit(@HalData^.y,'moc.jog.y') then Exit;
  if not ExportPinBit(@HalData^.z,'moc.jog.z') then Exit;
  if not ExportPinBit(@HalData^.b,'moc.jog.b') then Exit;
  if not ExportPinBit(@HalData^.c,'moc.jog.c') then Exit;
  if not ExportPinFloat(@HalData^.Increment,'moc.jog.increment') then Exit;
  if not ExportPinU32(@HalData^.cmd,'moc.command') then Exit;
  if not ExportPinFloat(@HalData^.skewx,'moc.skew.x') then Exit;
  if not ExportPinFloat(@HalData^.skewy,'moc.skew.y') then Exit;
  if not ExportPinFloat(@HalData^.skewz,'moc.skew.z') then Exit;
  if not ExportPinFloat(@HalData^.jogvel,'moc.jog.vel') then Exit;
  if not ExportPinFloat(@HalData^.maxvel,'moc.maxvel') then Exit;
end;

function InitHal(Name: string): boolean;
begin
  Result:= False;
  HalData:= nil;
  CompId:= 0;
  CompId:= hal_init(PChar(Name));
  if CompId < 0 then
    begin
      writeln('init failed: ',Name);
      Exit;
    end;
  HalData:= hal_malloc(SizeOf(THalStruct));
  if HalData = nil then
    begin
      hal_exit(CompId);
      writeln('malloc failed: ',Name);
      Exit;
    end;
  InitPins;
  if CompId < 1 then
    Exit;
  if hal_ready(CompId) <> HAL_SUCCESS then
    begin
      writeln('Hal component "' + Name + '" failed.');
      Exit;
    end;
  writeln('Hal component "' + Name + '" loaded.');
  Result:= True;
end;

{
procedure TestLoop;
var
  Flag: Integer;
begin
  if HalData <> nil then
    begin
      writeln('press a key to abort...');
      HalData^.increment^ :=  0;
      HalData^.x^ := 0;
      Flag:= 0;
      while not Keypressed do
      begin
        HalData^.increment^ :=  HalData^.increment^ + 0.1;
        inc(Flag);
        if Flag > 9 then Flag:= 0;
        HalData^.x^ := Byte(Boolean(Flag < 5));
        sleep(500);
      end;
      writeln('done...');
    end
  else
    writeln('HalData = nil!');
end;
}

begin
  HalHandle:= nil;
end.

