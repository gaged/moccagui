unit mocglb;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Buttons,Graphics,StdCtrls,
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, ExtCtrls,
  ExtDlgs, ComCtrls,
  mocemc;
  
const
  MaxAxes = 5;                  // maimum number of axes, default 5 XYZCB
  MaxJogIncs = 10;              // numer of joc-increments

  NumSoftButtons = 10;           // the buttons of the "bottom" panel
  NumMainButtons  = 10;         // the buttons of the "right" or "left" panel

  GlobalButtonSize = 64;
  GlobalButtonSpace = 4;

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

const
  caESTOP   = 'Notaus';
  caMACHINE = 'Maschine';
  caJOG     = 'Hand';
  caAUTO    = 'Satzlauf';
  caMDI     = 'Satzeingabe';
  caSPMINUS = 'langsamer';
  caSPPLUS  = 'schneller';
  caFLOOD   = 'Kühlung';
  caTOOL    = 'Werkzg';
  caREFACT  = 'Ref. Achse';
  

// these are the commands send by Buttonclick or Keyboard
// values > 100 define the button as a std button
// values < 100 define the button as a state- showing button
// they are used in TSpeedButton.Tag

const
  cmESTOP   = 0;

  cmMACHINE = 1;
  cmJOG     = 2;
  cmAUTO    = 3;
  cmMDI     = 4;

  cmSPMINUS = 10;
  cmSPPLUS  = 11;
  cmSPCW    = 12;
  cmSPCCW   = 13;
  cmSPBRAKE = 14;

  cmFLOOD   = 21;
  cmMIST    = 22;

  cmTOOL    = 130;

  cmREFACT  = 101;
  cmREFALL  = 102;
  cmOFFS1   = 104;

  cmNCOPEN  = 110;
  cmNCOPTM1 = 30;
  cmNCPAUSE = 31;
  cmNCRUN   = 32;
  cmNCSTOP  = 33;

  cmMDIEXEC = 120;
  cmMDIHIST = 121;
  

const
  // the main glyphs are the glyphs for the bottom- bottons
  cmdMainGlyphs: Array[0..NumMainButtons-1] of integer =
    (bmESTOP,
     bmMACHINE,bmJOG,bmAUTO,bmMDI,
     bmSPMINUS,bmSPCW,bmSPCCW,bmSPPLUS,
     bmFLOOD);

  // the main tags are the tags for the bottom- bottons
  cmdMainTags: Array[0..NumMainButtons-1] of integer =
    (cmESTOP,
     cmMACHINE,cmJOG,cmAUTO,cmMDI,
     cmSPMINUS,cmSPCW,cmSPCCW,cmSPPLUS,
     cmFLOOD);

  // the jog-glyphs are the glyphs for the left and right softbuttons
  cmdJogGlyphs: Array[0..NumSoftButtons - 1] of integer =
    (bmREFACT,bmREFALL,-1,bmOFFS1,-1,-1,-1,-1,-1,-1);

  cmdJogTags: Array[0..NumSoftButtons - 1] of integer =
    (cmREFACT,cmREFALL,-1,cmOFFS1,-1,-1,-1,-1,-1,-1);

  cmdMDIGlyphs: Array[0..NumSoftButtons - 1] of integer =
    (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);

  cmdMDITags: Array[0..NumSoftButtons - 1] of integer =
    (cmMDIEXEC,-1,-1,-1,-1,-1,-1,-1,-1,-1);

  cmdRunGlyphs: Array[0..NumSoftButtons - 1] of integer =
    (bmNCOPEN,bmNCRUN,bmNCSTOP,bmNCPAUSE,-1,-1,-1,-1,-1,-1);

  cmdRunTags: Array[0..NumSoftButtons - 1] of integer =
    (cmNCOPEN,cmNCRUN,cmNCSTOP,cmNCPAUSE,-1,-1,-1,-1,-1,-1);

type
  TJogIncrement = record
    Text: string;
    Value: Double;
  end;

type
  TEmcVars = record
    IniFile: string;              // inifile set by paramstr(1)
    NMLFile: string;              // the NML-file read from ini
    ProgramPrefix: string;        // program_prefix from ini
    ProgramFile: string;
    Machine: string;              // the machine as defined in inifile
    PostGuiHalfile: string;
    ToolTblFile: string;
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
    IsMetric: Boolean;            // metric, default true **FIXME
    UnitVelStr: string;           // linear velocitystr i.e. "inch/min"
    UnitStr: string;              // linear units i.e. inch "in","mm"
    ShowActual: Boolean;          // actual or cmd position
    ShowRelative: Boolean;        // relative or absolute position
    HomingOrderDefined: Boolean;  // check if can do a "home all"
    JogPolarity: Array[0..MaxAxes] of integer;
    JogIncrements: Array[0..MAXJOGINCS] of TJogIncrement;
    JogIncMax: integer;
    JogIncrement: Double;
    ActiveAxis: integer;
    StartFromLine: integer;
  end;

type
  TEMCState = record
    TaskMode: Integer;         // current taskmode, used in (update)
    EStop,
    Machine: Boolean;
    SpDir: integer;
    SpInc: integer;
    SpBrake: Boolean;
    SpSpeed: Double;
    SpEnabled: Boolean;
    Flood: Boolean;
    Mist: Boolean;
    Lube: Boolean;
    LubeLvl: integer;
    Dtg: Double;
    Vel: Double;
    Acc: Double;
    Probing: Boolean;

    InterpState: integer;
    OptStop: Boolean;
    BlockDel: Boolean;
    ReadLn: integer;
    CurrentLn: integer;
    ProgUnits: integer;

    ActVel: Integer;
    ActFeed: Integer;
    ActSpindleOride: Integer;
    ActJogVel: integer;
    ActAngJogVel: integer;

    MaxVel: integer;
    MaxJogVel: Integer;         // linear max jogspeed
    MinJogVel: integer;
    MaxAngJogVel: Integer;      // angular max jogspeed
    MinAngJogVel: integer;
  end;

var
  LastError: string[255];       // updated by checkerror
  UpdateLock: Boolean;          // used to prevent controls to be updated
  
  emcVars: TEmcVars;            // global emc vars
  emcState: TEmcState;          // global emc state

  Emc: TEMC;                    // the base class of the emc interface

  SoftBtns: Array[0..NumSoftButtons - 1] of TSpeedButton; // the soft buttons
  MainBtns: Array[0..NumMainButtons - 1] of TSpeedButton;  // the main buttons

  GlobalImageList: TImageList;
  
procedure EnableMainBtn(ACmd: integer; Enable: Boolean);
procedure SetSoftBtn(ACmd: integer; SetDown: Boolean);

implementation

procedure EnableMainBtn(ACmd: integer; Enable: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumMainButtons - 1 do
    if Assigned(MainBtns[i]) then
      if MainBtns[i].Tag = ACmd then
        begin
          MainBtns[i].Enabled:= Enable;
          Break;
        end;
end;

procedure SetSoftBtn(ACmd: integer; SetDown: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumSoftButtons - 1 do
    if Assigned(SoftBtns[i]) then
      if SoftBtns[i].Tag = ACmd then
        begin
          SoftBtns[i].Down:= SetDown;
          Break;
        end;
end;

end.

