const

  MaxAxes = 5;                   // maimum number of axes, default 5 XYZCB
  MaxJogIncs = 10;               // numer of joc-increments

  NumSButtons  = 10;
  NumMButtons  = 10;
  NumAllButtons = NumSButtons + NumMButtons;

  ButtonSizeLarge = 72;
  ButtonSizeMed = 60;
  ButtonSizeSmall = 48;

  GlobalButtonSize: integer = ButtonSizeLarge;
  GlobalButtonSpace: integer = 2;

  DroFontSize: integer = 36;  // default dro font size
  MainFontSize: integer = 0;  // use default widgetset font
  MainFontBold: Boolean = False;

type
  TButtonDef = record
    T: integer;
    G: integer;
    S: string;
  end;

  PSButtonArray = ^TSButtonArray;
  TSButtonArray = Array[0..NumSButtons-1] of TButtonDef;

  PMButtonArray = ^TMButtonArray;
  TMButtonArray = Array[0..NumMButtons-1] of TButtonDef;


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
  bmSPBRAKE = -1;
  bmMIST    = -1;


// these are the commands send by Buttonclick or Keyboard
// values > 100 define the button as a std button
// values < 100 define the button as a state- showing button
// they are used in TSpeedButton.Tag

  cmABORT     = 0;
  cmESTOP     = 1;
  cmMACHINE   = 2;
  cmJOG       = 3;
  cmAUTO      = 4;
  cmMDI       = 5;

  cmSPMINUS   = 201;
  cmSPPLUS    = 202;
  cmSPCW      = 11;
  cmSPCCW     = 12;
  cmSPBRAKE   = 13;

  cmFLOOD     = 15;
  cmMIST      = 16;

  cmREF       = 220;
  cmREFX      = 221;
  cmREFY      = 222;
  cmREFZ      = 223;
  cmREFA      = 224;
  cmREFB      = 225;
  cmREFC      = 226;
  cmUNREF     = 227;
  cmREFALL    = 228;

  cmTOOLS     = 230;
  cmTOOLEDT   = 231;
  cmTOOLCHG   = 232;
  cmTOOLCL    = 233;
  cmTOOLUNCL  = 234;
  cmTOOLPAR   = 235;
  cmTOOLMSR   = 236;

  cmTOUCHOFF  = 240;
  cmTOUCHX    = 241;
  cmTOUCHY    = 242;
  cmTOUCHZ    = 243;
  cmTOUCHA    = 244;
  cmTOUCHB    = 245;
  cmTOUCHC    = 246;
  cmZEROALL   = 247;
  cmOFFSDLG   = 248;


  cmSETUP     = 250;
  cmVIEW      = 260;

  cmLIMITS    = 260;

  cmOPEN      = 270;
  cmRUN       = 20;
  cmRUNLINE   = 21;
  cmSTOP      = 22;
  cmSTEP      = 271;
  cmPAUSE     = 23;
  cmOPTSTOP   = 24;
  cmBLOCKDEL  = 25;
  cmRELOAD    = 272;

  cmMDIEXEC   = 280;
  cmMDIHIST   = 281;

  cmEDITOR    = 290;

  cmFEEDUP1   = 301;
  cmFEEDUP10  = 302;
  cmFEED100   = 303;
  cmFEEDDN1   = 304;
  cmFEEDDN10  = 305;

  cmJVELUP1    = 310;
  cmJVELUP10   = 311;
  cmJVEL100    = 312;
  cmJVELDN1    = 313;
  cmJVELDN10   = 314;

  cmINCRDN     = 320;
  cmINCRUP     = 321;

  cmSHIFTUP    = 330;
  cmSHIFTDN    = 331;

  cmPARTALGN   = 340;
  cmUNITS      = 350;

  cmBACK       = 999;

  cmSCRIPTBASE = 1000;
  cmSCRIPTS    = 1099;

  BtnDefJog: TMButtonArray =
     ((T:cmESTOP;    G:-1;    S:'Notaus'),
      (T:cmMACHINE;  G:-1;    S:'Maschine'),
      (T:-1;         G:-1;    S:'Manuell'),
      (T:cmAUTO;     G:-1;    S:'Satzlauf'),
      (T:cmMDI;      G:-1;    S:'MDI'),
      (T:cmSPPLUS;  G:-1;     S:'Spindel schneller'),
      (T:cmSPCW;     G:-1;    S:'Spindel Rechts'),
      (T:cmSPCCW;    G:-1;    S:'Spindel Links'),
      (T:cmSPMINUS;   G:-1;   S:'Spindel langsamer'),
      (T:cmFLOOD;    G:-1;    S:'Kühlung'));

  BtnDefJogMain: TSButtonArray =
     ((T:cmREF;      G:-1;    S:'Referenz- Fahrt'),
      (T:cmTOUCHOFF; G:-1;    S:'Antasten'),
      (T:cmTOOLS;    G:-1;    S:'Werkzeug'),
      (T:cmSCRIPTS;  G:-1;    S:'Scripts'),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:cmINCRDN;   G:-1;    S:'Schrittweite -'),
      (T:cmINCRUP;   G:-1;    S:'Schrittweite +'));

  BtnDefJogRef: TSButtonArray =
     ((T:cmBACK;     G:-1;    S:'<'),
      (T:cmREFALL;   G:-1;    S:'Alle Achsen'),
      (T:cmREFX;     G:-1;    S:'X-Achse'),
      (T:cmREFY;     G:-1;    S:'Y-Achse'),
      (T:cmREFZ;     G:-1;    S:'Z-Achse'),
      (T:cmREFA;     G:-1;    S:'A-Achse'),
      (T:cmREFB;     G:-1;    S:'B-Achse'),
      (T:cmREFC;     G:-1;    S:'C-Achse'),
      (T:cmUNREF;    G:-1;    S:'Aufheben'),
      (T:-1;         G:-1;    S:''));


  BtnDefJogTouch: TSButtonArray =
     ((T:cmBACK;     G:-1;    S:'<'),
      (T:cmTOUCHX;   G:-1;    S:'X-Achse...'),
      (T:cmTOUCHY;   G:-1;    S:'Y-Achse...'),
      (T:cmTOUCHZ;   G:-1;    S:'Z-Achse...'),
      (T:cmTOUCHA;   G:-1;    S:'A-Achse...'),
      (T:cmTOUCHB;   G:-1;    S:'B-Achse...'),
      (T:cmTOUCHC;   G:-1;    S:'C-Achse...'),
      (T:cmZEROALL;  G:-1;    S:'Alle Achsen 0.000'),
      (T:cmOFFSDLG;  G:-1;    S:'Offsets...'),
      (T:-1;         G:-1;    S:''));

  BtnDefJogTool: TSButtonArray =
     ((T:cmBACK;     G:-1;    S:'<'),
      (T:cmTOOLCHG;  G:-1;    S:'Werkzeug wechseln..'),
      (T:cmTOOLEDT;  G:-1;    S:'Werkzeuge bearbeiten..'),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''),
      (T:-1;         G:-1;    S:''));

  BtnDefMDI: TMButtonArray =
     ((T:cmESTOP;    G:-1;    S:'Notaus'),
      (T:cmMACHINE;  G:-1;    S:'Maschine'),
      (T:cmJOG;      G:-1;    S:'Manuell'),
      (T:cmAUTO;     G:-1;    S:'Satzlauf'),
      (T:-1;         G:-1;    S:'MDI'),
      (T:-1;         G:-1;     S:''),
      (T:-1;         G:-1;     S:''),
      (T:-1;         G:-1;     S:''),
      (T:-1;         G:-1;     S:''),
      (T:-1;         G:-1;     S:''));

   BtnDefMDI1 : TSButtonArray =
     ((T:cmMDIEXEC; G:-1;       S:'Ausführen'),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''),
      (T:-1;        G:-1;       S:''));

  BtnDefRun : TMButtonArray =
     ((T:cmESTOP;    G:-1;    S:'Notaus'),
      (T:cmMACHINE;  G:-1;    S:'Maschine'),
      (T:cmJOG;      G:-1;    S:'Manuell'),
      (T:-1;         G:-1;    S:'Satzlauf'),
      (T:cmMDI;      G:-1;    S:'MDI'),
      (T:cmOPEN;     G:-1;    S:'Öffnen'),
      (T:cmRELOAD;   G:-1;    S:'Neu Laden'),
      (T:cmOPTSTOP ; G:-1;    S:'Opt. Pause'),
      (T:cmBLOCKDEL; G:-1;    S:'"/" Blöcke'),
      (T:-1;         G:-1;    S:''));

  BtnDefRun1 : TSButtonArray =
     ((T:cmRUN;      G:-1;      S:'Start'),
      (T:cmSTOP;     G:-1;      S:'Stop'),
      (T:cmPAUSE;    G:-1;      S:'Pause'),
      (T:cmSTEP;     G:-1;      S:'Schritt'),
      (T:cmRUNLINE;  G:-1;      S:'Start Zeile'),
      (T:-1;         G:-1;      S:''),
      (T:-1;         G:-1;      S:''),
      (T:-1;         G:-1;      S:''),
      (T:-1;         G:-1;      S:''),
      (T:-1;         G:-1;      S:''));

  ToolsLathe : array[0..8] of string =
    ('Slot',' Id ','Z-Offset','X-Offset','Durchm. ',
      'Winkel Vorne ','Winkel Hinten','Richtung',
      '  Bezeichnung  ');

  ToolsMill : array[0..4] of string =
    ('Slot',' Id ','Länge   ','Durchm. ','  Bezeichnung  '); 

  CoordSysMax = 8;
  CoordSys: Array[0..CoordSysMax] of string =
    ('G54','G55','G56','G57','G58','G59','G59.1','G59.2','G59.3');

  CoordSysVar = '5221';
  CoordSysInc = 10;

type
  TJogIncrement = record
    Text: string;
    Value: Double;
  end;

  TExtents = record
    MinX,MaxX: double;
    MinY,MaxY: double;
    MinZ,MaxZ: double;
  end;

  Tlo = packed record
    x,y,z,a,b,c,u,v,w: double;
  end;

  TSkewCoord = record
    x,y,z: double;
    IsSet: boolean;
  end;

  TAxisDef = record
    AxisChar: Char;
    IsLinear: Boolean;
  end;

  TAxisDefs = Array[0..MaxAxes - 1] of TAxisDef;

  TScriptDef = record
    Name: string;
    Script: string;
  end;

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
    Axis: TAxisDefs;
    CoordNames: string;           // the given coordinates (XYZ...)
    CycleTime: Double;            // as defined in [DISPLAY], in seconds
    CycleDelay: Longint;          // cycletime / 1000 in msec
    MaxFeedOverride: integer;     // maximum feed override
    MaxSpORide: integer;  // maximum spindle override
    MinSpORide: integer;
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
    MLimits: TExtents;            // machine limits
    ScriptDir: string;
  end;

  TEmcState = record
    TaskMode: Integer;         // current taskmode, used in (update)
    EStop: Boolean;
    Machine: Boolean;
    SpDir: integer;
    SpInc: integer;
    SpBrake: Boolean;
    SpSpeed: Double;
    SpEnabled: Boolean;
    SpIncreasing: Integer;
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
    ActSpORide: integer;
    UnitsChanged: Boolean;
  end;

  TGlSettings = record
    UseDirect: Boolean;
    UseRGBA: Boolean;
    UseDoubleBuffered: Boolean;
  end;

  TOnClick = procedure(Sender: TObject) of object;
