unit hal;

{$mode objfpc}{$H+}

interface

uses
  crt,sysutils,classes,ctypes, emcint;

const
  HAL_SUCCESS = 0;

  MOC_HAL_NAME = 'mocca';

const
  HalButtonMin = 0;
  HalButtonMax = 9;

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
    increment: ^THalFLoat;
    x,y,z,b,c: ^THalBit;
    command: ^THalU32;
    user_error: ^THalU32;
    rotation_x: ^THalFloat;
    rotation_y: ^THalFloat;
    rotation_z: ^THalFloat;
    feed: ^THalS32;
    maxvel: ^THalS32;
    spindle: ^THalS32;
    jogvel: ^THalS32;
    modeauto: ^THalBit;
    modemanual: ^THalBit;
    modemdi: ^THalBit;
    button0: ^THalBit;
    button1: ^THalBit;
    button2: ^THalBit;
    button3: ^THalBit;
    button4: ^THalBit;
    button5: ^THalBit;
    button6: ^THalBit;
    button7: ^THalBit;
    button8: ^THalBit;
    button9: ^THalBit;
    oemled0: ^THalBit;
    oemled1: ^THalBit;
    oemled2: ^THalBit;
    oemled3: ^THalBit;
    oemled4: ^THalBit;
  end;

function InitHal: boolean;
procedure DoneHal;

function hal_init(const name: PChar): longint; cdecl; external libemchal;
function hal_ready(compid: integer): longint; cdecl; external libemchal;
function hal_exit(compid: integer): longint; cdecl; external libemchal;
function hal_malloc(size: integer): pointer; cdecl; external libemchal;
function halpr_find_pin_by_name(name: pchar): pointer; cdecl; external libemchal;

function hal_pin_bit_new(const name: PChar; dir: TPinDir;
  Data: PPHalBit; comp_id: cint): cint; cdecl; external libemchal;

function hal_pin_float_new(const name: PChar; dir: TPinDir;
  Data: PPHalFloat; comp_id: cint): cint; cdecl; external libemchal;

function hal_pin_u32_new(const name: PChar; dir: TPinDir;
  Data: PPHalU32; comp_id: cint): cint; cdecl; external libemchal;

function hal_pin_s32_new(const name: PChar; dir: TPinDir;
  Data: PPHalS32; comp_id: cint): cint; cdecl; external libemchal;

procedure InitHalPins;

procedure SetHalJogAxis(Axis: Char);

function GetHalButtonDown(Id: integer): Boolean;

function GetHalError: integer;

function GetHalFeed: integer;
procedure SetHalFeed(Value: integer);
function GetHalVelocity: integer;
procedure SetHalVelocity(Value: integer);
function GetHalSpindle: integer;
procedure SetHalSpindle(Value: integer);

function GetHalTaskMode: integer;
procedure UpdateHalTaskMode(Mode: integer);

function GetHalLedState(Led: integer): Boolean;

procedure SetHalRotation(x,y,z: Double);

function GetHalCommand(var Command: integer): Boolean;

implementation

uses
   emc2pas,mocglb;

var
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

function GetHalLedState(Led: integer): Boolean;
begin
  case LEd of
    0: Result:= HalData^.oemled0^ <> 0;
    1: Result:= HalData^.oemled1^ <> 0;
    2: Result:= HalData^.oemled2^ <> 0;
    3: Result:= HalData^.oemled3^ <> 0;
    4: Result:= HalData^.oemled4^ <> 0;
  end;
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

function GetHalCommand(var Command: integer): Boolean;
begin
  Result:= False;
  if HalData^.command^ > 0 then
    begin
      Command:= HalData^.command^;
      HalData^.command^ := 0;
      Result:= True;
    end;
end;

function GetHalError: integer;
begin
  Result:= HalData^.user_error^;
end;

function GetHalFeed: integer;
begin
  Result:= HalData^.feed^;
end;

procedure SetHalFeed(Value: integer);
begin
  HalData^.feed^:= Value;
end;

function GetHalVelocity: integer;
begin
  Result:= HalData^.maxvel^;
end;

procedure SetHalVelocity(Value: integer);
begin
  HalData^.maxvel^:= Value;
end;

function GetHalSpindle: integer;
begin
  Result:= HalData^.spindle^;
end;

procedure SetHalSpindle(Value: integer);
begin
  HalData^.spindle^:= Value;
end;

function GetHalTaskMode: integer;
begin
  if HalData^.modemdi^ <> 0 then
    begin
      HalData^.modeauto^:=  0;
      HalData^.modemanual^:= 0;
      Result:= TASKMODEMDI;
    end
  else
  if HalData^.modeauto^ <> 0 then
    begin
      HalData^.modemanual^:= 0;
      HalData^.modemdi^:= 0;
      Result:= TASKMODEAUTO;
    end
  else
    begin
      HalData^.modemdi^:= 0;
      HalData^.modeauto^:= 0;
      Result:= TASKMODEMANUAL;
    end;
end;

procedure UpdateHalTaskMode(Mode: integer);
begin
  HalData^.modemanual^:= Byte(Mode = TASKMODEMANUAL);
  HalData^.modeauto^:= Byte(Mode = TASKMODEAUTO);
  HalData^.modemdi^:= Byte(Mode = TASKMODEMDI);
end;

procedure SetHalJogIncrement(Incr: double);
begin
  if HalData = nil then
    raise Exception.Create('Set_Hal_Jogincrement: HalData = nil!');
  HalData^.Increment^ := Incr;
end;

function GetHalJogIncrement: double;
begin
  if HalData = nil then
    raise Exception.Create('HalData = nil!');
  Result:= HalData^.Increment^;
end;

procedure SetHalRotation(x,y,z: Double);
begin
  if HalData = nil then
    raise Exception.Create('Set_Hal_Rotation: HalData = nil!');
  HalData^.rotation_x^:= x;
  HalData^.rotation_y^:= y;
  HalData^.rotation_z^:= z;
end;

function CheckButtonDown(Bit: PHalBit): Boolean;
begin
  Result:= Byte(Bit^) <> 0;
  if Result then Byte(Bit^) := 0;
end;

function GetHalButtonDown(Id: integer): Boolean;
begin
  Result:= False;
  if HalData = nil then
    raise Exception.Create('Get_Hal_ButtonDown: HalData = nil!');
  case Id of 
    0:  Result:= CheckButtonDown(HalData^.button0);
    1:  Result:= CheckButtonDown(HalData^.button1);
    2:  Result:= CheckButtonDown(HalData^.button2);
    3:  Result:= CheckButtonDown(HalData^.button3);
    4:  Result:= CheckButtonDown(HalData^.button4);
    5:  Result:= CheckButtonDown(HalData^.button5);
    6:  Result:= CheckButtonDown(HalData^.button6);
    7:  Result:= CheckButtonDown(HalData^.button7);
    8:  Result:= CheckButtonDown(HalData^.button8);
    9:  Result:= CheckButtonDown(HalData^.button9);
  end;
end;

procedure InitHalPins;
begin
  SetHalRotation(0,0,0);
  HalData^.feed^:= State.ActFeed;
  HalData^.maxvel^:= State.MaxVel;
  HalData^.jogvel^:= State.ActJogVel;
  HalData^.modeauto^:= 0;
  HalData^.modemdi^:= 0;
  HalData^.modemanual^:= 1;
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

function ExportPinBit(pin: PPHalBit; Dir: TPinDir; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_bit_new(PChar(name),dir,pin,compid)));
end;

function ExportPinFloat(pin: PPHalFloat; Dir: TPinDir; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_float_new(PChar(name),dir,pin,compid)));
end;

function ExportPinU32(pin: PPHalU32; Dir: TPinDir; Name: string): Boolean;
begin
  Result:= CheckPin((hal_pin_u32_new(PChar(name),dir,pin,compid)));
end;

function ExportPinS32(pin: PPHalS32; Dir: TPinDir; Name: string): Boolean;
begin  
  Result:= CheckPin((hal_pin_s32_new(PChar(name),dir,pin,compid)));
end;

function InitPins: Boolean;
begin
  Result:= False;
  if not ExportPinFloat(@HalData^.Increment,HAL_OUT,'moc.jog.increment') then Exit;  
  if not ExportPinBit(@HalData^.x,HAL_OUT,'moc.jog.x') then Exit;
  if not ExportPinBit(@HalData^.y,HAL_OUT,'moc.jog.y') then Exit;
  if not ExportPinBit(@HalData^.z,HAL_OUT,'moc.jog.z') then Exit;
  if not ExportPinBit(@HalData^.b,HAL_OUT,'moc.jog.b') then Exit;
  if not ExportPinBit(@HalData^.c,HAL_OUT,'moc.jog.c') then Exit;
  if not ExportPinU32(@HalData^.command,HAL_IN,'moc.command') then Exit;
  if not ExportPinU32(@HalData^.user_error,HAL_IN,'moc.errorcode') then Exit;
  if not ExportPinFloat(@HalData^.rotation_x,HAL_OUT,'moc.rot.x') then Exit;
  if not ExportPinFloat(@HalData^.rotation_y,HAL_OUT,'moc.rot.y') then Exit;
  if not ExportPinFloat(@HalData^.rotation_z,HAL_OUT,'moc.rot.z') then Exit;
  if not ExportPinS32(@HalData^.feed,HAL_IO,'moc.feed') then Exit;
  if not ExportPinS32(@HalData^.maxvel,HAL_IO,'moc.maxvel') then Exit;
  if not ExportPinS32(@HalData^.spindle,HAL_IO,'moc.spindle') then Exit;
  if not ExportPinS32(@HalData^.jogvel,HAL_IO,'moc.jog.vel') then Exit;
  if not ExportPinBit(@HalData^.modeauto,HAL_IN,'moc.mode.auto') then Exit;
  if not ExportPinBit(@HalData^.modemanual,HAL_IN,'moc.mode.manual') then Exit;
  if not ExportPinBit(@HalData^.modemdi,HAL_IN,'moc.mode.mdi') then Exit;
  if not ExportPinBit(@HalData^.button0,HAL_IN,'moc.button.0') then Exit;
  if not ExportPinBit(@HalData^.button1,HAL_IN,'moc.button.1') then Exit;
  if not ExportPinBit(@HalData^.button2,HAL_IN,'moc.button.2') then Exit;
  if not ExportPinBit(@HalData^.button3,HAL_IN,'moc.button.3') then Exit;
  if not ExportPinBit(@HalData^.button4,HAL_IN,'moc.button.4') then Exit;
  if not ExportPinBit(@HalData^.button5,HAL_IN,'moc.button.5') then Exit;
  if not ExportPinBit(@HalData^.button6,HAL_IN,'moc.button.6') then Exit;
  if not ExportPinBit(@HalData^.button7,HAL_IN,'moc.button.7') then Exit;
  if not ExportPinBit(@HalData^.button8,HAL_IN,'moc.button.8') then Exit;
  if not ExportPinBit(@HalData^.button9,HAL_IN,'moc.button.9') then Exit;
  if not ExportPinBit(@HalData^.oemled0,HAL_IN,'moc.led.0') then Exit;
  if not ExportPinBit(@HalData^.oemled1,HAL_IN,'moc.led.1') then Exit;
  if not ExportPinBit(@HalData^.oemled2,HAL_IN,'moc.led.2') then Exit;
  if not ExportPinBit(@HalData^.oemled3,HAL_IN,'moc.led.3') then Exit;
  if not ExportPinBit(@HalData^.oemled4,HAL_IN,'moc.led.4') then Exit;
  Result:= True;
end;

function InitHal: boolean;
begin
  Result:= False;
  HalData:= nil;
  CompId:= 0;
  CompId:= hal_init(PChar(MOC_HAL_NAME));
  if CompId < 0 then
    begin
      writeln('init failed: ',MOC_HAL_NAME);
      Exit;
    end;
  HalData:= hal_malloc(SizeOf(THalStruct));
  if HalData = nil then
    begin
      hal_exit(CompId);
      writeln('malloc failed: ',MOC_HAL_NAME);
      Exit;
    end;
  InitPins;
  if CompId < 1 then
    Exit;
  if hal_ready(CompId) <> HAL_SUCCESS then
    begin
      writeln('Hal component "' + MOC_HAL_NAME + '" failed.');
      Exit;
    end;
  writeln('Hal component "' + MOC_HAL_NAME + '" loaded.');
  Result:= True;
end;

procedure DoneHal;
begin
  if CompId > 0 then
    hal_exit(CompId);
end;


end.

