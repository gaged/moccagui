unit mocglb;

{$I mocca.inc}

interface

uses
  Buttons,Graphics,
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, ExtCtrls,
  ExtDlgs, ComCtrls,
  mocemc;
  
const
  MaxAxes = 5;                   // maimum number of axes, default 5 XYZCB
  MaxJogIncs = 10;               // numer of joc-increments

  NumSoftButtons  = 10;          // the buttons of the "bottom" panel
  NumSideButtons  = 10;          // the buttons of the "right" or "left" panel

  NumTotalButtons = NumSideButtons + NumSoftButtons;

  GlobalButtonSize: integer = 72;
  GlobalButtonSpace: integer = 2;

  DroFontSize: integer = 48;

// these are the Buttons in the same order as defined in the
// ImageList of TMainForm. Maybe those will be replaced by a
// inifile to allow the user to change them

const
  bmESTOP   = 0;  // Esc
  bmMACHINE = 1;  // F1
  bmJOG     = 2;
  bmAUTO    = 3;
  bmMDI     = 4;  // F4
  bmSPMINUS = 5;  // F5
  bmSPCW    = 6;
  bmSPCCW   = 7;
  bmSPPLUS  = 8;  // F8
  bmFLOOD   = 9;
  bmTOOL    = 10;
  bmREFACT  = 11;
  bmREFALL  = 12;
  bmNCOPEN  = 13;
  bmNCOPTM1 = 14;
  bmNCPAUSE = 15;
  bmNCRUN   = 16;
  bmNCSTOP  = 17;
  bmOFFS1   = 18;
  //bmSPBRAKE = 9;
  //bmMIST    = 11;


// these are the commands send by Buttonclick or Keyboard
// values > 100 define the button as a std button
// values < 100 define the button as a state- showing button
// they are used in TSpeedButton.Tag

const
  cmESTOP     = 0;
  cmMACHINE   = 1;

  cmJOG       = 10;
  cmAUTO      = 11;
  cmMDI       = 12;

  cmSPMINUS   = 21;
  cmSPPLUS    = 22;
  cmSPCW      = 23;
  cmSPCCW     = 24;
  cmSPBRAKE   = 25;

  cmFLOOD     = 30;
  cmMIST      = 31;

  cmTOOLS     = 40;

  cmREFACT    = 50;
  cmREFALL    = 51;

  cmUNITS     = 60;

  cmZEROALL   = 70;
  cmZEROACT   = 71;
  cmOFFSDLG   = 72;

  cmLIMITS    = 80;

  cmOPEN      = 90;
  cmRUN       = 91;
  cmRUNLINE   = 92;
  cmSTOP      = 93;
  cmSTEP      = 94;
  cmPAUSE     = 95;
  cmOPTSTOP   = 96;
  cmBLOCKDEL  = 97;

  cmMDIEXEC   = 101;
  cmMDIHIST   = 102;

  cmEDITOR    = 110;

  cmReturn  = 1001;
  cmExit    = 1002;
  cmCancel  = 1003;
  cmClose   = 1004;

type
  TButtonDef = record
    T: integer;
    G: integer;
    S: string;
  end;

type
  PButtonArray = ^TButtonArray;
  TButtonArray = Array[0..NumTotalButtons - 1] of TButtonDef;

const
  BtnDefJog: TButtonArray =
     ((T:cmESTOP;    G:-1;    S:'Notaus'),
      (T:cmMACHINE;  G:-1;    S:'Maschine'),
      (T:-1;         G:-1;    S:'Manuell'),
      (T:cmAUTO;     G:-1;    S:'Satzlauf'),
      (T:cmMDI;      G:-1;    S:'MDI'),
      (T:cmSPMINUS;  G:-1;    S:'langsamer'),
      (T:cmSPCW;     G:-1;    S:'Spindel rechts'),
      (T:cmSPCCW;    G:-1;    S:'Spindel links'),
      (T:cmSPPLUS;   G:-1;    S:'schneller'),
      (T:cmFLOOD;    G:-1;    S:'Kühlung'),
      (T:cmREFACT;   G:-1;    S:'Referenzf.'),
      (T:cmREFALL;   G:-1;    S:'Referenzf. Alle'),
      (T:cmZEROACT;  G:-1;    S:'Antasten...'),
      (T:cmZEROALL;  G:-1;    S:'Alle Null'),
      (T:cmOFFSDLG;  G:-1;    S:'Koordinaten..'),
      (T:cmTOOLS;    G:-1;    S:'Werkzeuge..'),
      (T:cmLIMITS;   G:-1;    S:'Grenzwerte'),
      (T:-1;         G:-1;    S:''),
      (T:cmUNITS;    G:-1;    S:'mm/inch'),
      (T:-1;         G:-1;    S:''));

  BtnDefMDI: TButtonArray =
     ((T:cmESTOP;    G:-1;   S:'Notaus'),
      (T:cmMACHINE;  G:-1;   S:'Maschine'),
      (T:cmJOG;      G:-1;   S:'Manuell'),
      (T:cmAUTO;     G:-1;   S:'Satzlauf'),
      (T:-1;         G:-1;   S:'MDI'),
      (T:-1;         G:-1;   S:'langsamer'),
      (T:-1;          G:-1;   S:'Spindel rechts'),
      (T:-1;          G:-1;   S:'Spindel links'),
      (T:-1;          G:-1;   S:'schneller'),
      (T:-1;          G:-1;   S:'Kühlung'),
      (T:cmMDIEXEC; G:-1;       S:'Ausführen'),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''));

  BtnDefRun: TButtonArray =
     ((T:cmESTOP;    G:-1;   S:'Notaus'),
      (T:cmMACHINE;  G:-1;   S:'Maschine'),
      (T:cmJOG;      G:-1;   S:'Manuell'),
      (T:-1;         G:-1;   S:'Satzlauf'),
      (T:cmMDI;      G:-1;   S:'MDI'),
      (T:-1;         G:-1;   S:''),
      (T:-1;          G:-1;   S:''),
      (T:-1;          G:-1;   S:''),
      (T:-1;          G:-1;   S:''),
      (T:-1;          G:-1;   S:''),
     (T:cmOPEN;  G:-1;      S:'Öffnen'),
      (T:cmRUN;   G:-1;       S:'Start'),
      (T:cmSTOP;  G:-1;      S:'Stop'),
      (T:cmPAUSE; G:-1;     S:'Pause'),
      (T:cmSTEP;  G:-1;            S:'Schritt'),
      (T:cmRUNLINE;  G:-1;            S:'Start Zeile'),
      (T:cmOPTSTOP ;        G:-1;            S:'Opt. Pause'),
      (T:cmBLOCKDEL;        G:-1;            S:'"/" Blöcke'),
      (T:-1;        G:-1;            S:''),
      (T:cmEDITOR;  G:-1;            S:'NC-Editor'));

const
  ToolsLathe : array[0..8] of string =
    ('Slot',' Id ','Z-Offset','X-Offset','Durchm. ',
      'Winkel Vorne ','Winkel Hinten','Richtung',
      '  Bezeichnung  ');

  ToolsMill : array[0..4] of string =
    ('Slot',' Id ','Länge   ','Durchm. ','  Bezeichnung  ');

type
  TJogIncrement = record
    Text: string;
    Value: Double;
  end;

type
  TEmcVars = record
    IniFile: string;              // inifile set by paramstr(1)
    IniPath: string;
    NMLFile: string;              // the NML-file read from ini
    ProgramPrefix: string;        // program_prefix from ini
    ProgramFile: string;
    Machine: string;              // the machine as defined in inifile
    PostGuiHalfile: string;
    ToolFile: string;
    ParamFile: string;
    Extensions: string;
    Geometry: string;
    NumAxes: Integer;             // number of axes in ini
    CoordNames: string;           // the given coordinates (XYZ...)
    CycleTime: Double;            // as defined in [DISPLAY], in seconds
    CycleDelay: Longint;          // cycletime / 1000 in msec
    MaxFeedOverride: integer;     // maximum feed override
    MaxSpindleOverride: integer;  // maximum spindle override
    LinearJogSpeed: double;
    AngularJogSpeed: double;
    MaxLinearVel: double;
    MaxAngularVel: double;
    Metric: Boolean;            // metric, default true **FIXME
    UnitVelStr: string;           // linear velocitystr i.e. "inch/min"
    UnitStr: string;              // linear units i.e. inch "in","mm"
    ShowActual: Boolean;          // actual or cmd position
    ShowRelative: Boolean;        // relative or absolute position
    HomingOrderDefined: Boolean;  // check if can do a "home all"
    JogPolarity: Array[0..MaxAxes] of integer;
    JogIncrements: Array[0..MAXJOGINCS] of TJogIncrement; // 0 = continous
    JogIncMax: integer;
    JogIncrement: Double;
    JogContinous: Boolean;
    ActiveAxis: integer;
    StartLine: integer;
  end;

type
  TEmcState = record
    TaskMode: Integer;         // current taskmode, used in (update)
    EStop: Boolean;
    Machine: Boolean;
    SpDir: integer;
    SpInc: integer;
    SpBrake: Boolean;
    SpSpeed: Double;
    SpEnabled: Boolean;
    Flood: Boolean;
    Mist: Boolean;
    Lube: Boolean;
    LubeLevel: integer;
    Probing: Boolean;
    InterpState: integer;
    OptStop: Boolean;
    BlockDel: Boolean;
    ReadLn: integer;
    CurrentLn: integer;
    MotionLn: integer;
    ProgUnits: integer;
    DisplayUnits: integer;
    ORideLimits: Boolean;
    SpindleOverride: integer;

    CurrentTool: integer;
    ToolPrepared: Boolean;
    ToolOffset: double;
    TloAlongW: Boolean;

    Dtg: double;
    Vel: double;
    Acc: double;
    ActVel: Integer;
    MaxVel: integer;
    ActFeed: Integer;
    MaxFeed: integer;
    ActJogVel: integer;
    MaxJogVel: Integer;
    UnitsChanged: Boolean;
  end;

var
  LastError: string;
  UpdateLock: Boolean;

  Vars: TEmcVars;
  State: TEmcState;

  MocBtns: Array[0..NumTotalButtons - 1] of TSpeedButton; // the soft buttons

  GlobalImageList: TImageList;

const
  CoordSysMax = 8;
  CoordSys: Array[0..CoordSysMax] of string =
    ('G54','G55','G56','G57','G58','G59','G59.1','G59.2','G59.3');

  CoordSysVar = '5221';
  CoordSysInc = 10;

type
  TOnClick = procedure(Sender: TObject) of object;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
procedure SetButtonText(ACmd: integer; AText: string);
procedure SetButtonMap(B: PButtonArray; ObjClick: TOnClick);

// function setenv(envname,envval: PChar; overwrite: integer): longint; cdecl; external;

procedure RaiseError(const Msg: string);

implementation

procedure RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure SetButtonMap(B: PButtonArray; ObjClick: TOnClick);
var
  i: Integer;
begin
  if B = nil then Exit;
  for i:= 0 to NumTotalButtons - 1 do
  begin
    if B^[i].T < 0 then
      MocBtns[i].OnClick:= nil
    else
      MocBtns[i].OnClick:= ObjClick;
    if B^[i].G < 0 then
      MocBtns[i].Glyph:= nil
    else
      GlobalImageList.GetBitmap(B^[i].G,MocBtns[i].Glyph);
    MocBtns[i].Tag:= B^[i].T;
    MocBtns[i].Caption:= B^[i].S;
    MocBtns[i].Enabled:= not (B^[i].T < 0);
    MocBtns[i].Down:= False;
  end;
end;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Enabled:= Enable;
          Break;
        end;
end;

procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Down:= SetDown;
          Break;
        end;
end;

procedure SetButtonText(ACmd: integer; AText: string);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Caption:= AText;
          Break;
        end;
end;


initialization

GlobalImageList:= nil;
LastError:= '';
end.

