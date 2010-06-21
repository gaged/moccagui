const

  MaxAxes = 5;                   // maimum number of axes, default 5 XYZCB
  MaxJogIncs = 10;               // numer of joc-increments
  NumButtons  = 10;              // Number of User-Defined Buttons


type
  TButtonDef = record
    T: integer;
    G: integer;
    S: string;
  end;

  PButtonArray = ^TButtonArray;
  TButtonArray = Array[0..NumButtons-1] of TButtonDef;

// these are the commands send by Buttonclick or Keyboard
// values > 100 define the button as a std button
// values < 100 define the button as a state- showing button
// they are used in TMocButton.Tag

const
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
  cmUNREFALL  = 229;
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
  cmTOUCHWIZ  = 249;
  cmSETUP     = 250;
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
  cmINCRDN     = 320;
  cmINCRUP     = 321;
  cmSHIFTUP    = 330;
  cmSHIFTDN    = 331;
  cmPARTALGN   = 340;
  cmUNITS      = 350;
  cmBACK       = 999;
  cmSCRIPTBASE = 1000;
  cmSCRIPTS    = 1099;
  cmCLOSE      = 360;
  cmFEEDRESET  = 400;

type
  TCmdNames = record
    S: string;
    i: integer;
  end;

const
  CmdNamesMax = 64;

const
  CmdNames: Array[0..CmdNamesMax] of TCmdNames = (
  (S:'cmABORT'     ; i:0),
  (S:'cmESTOP'     ; i:1),
  (S:'cmMACHINE'   ; i:2),
  (S:'cmJOG'       ; i:3),
  (S:'cmAUTO'      ; i:4),
  (S:'cmMDI'       ; i:5),
  (S:'cmSPMINUS'   ; i:201),
  (S:'cmSPPLUS'    ; i:202),
  (S:'cmSPCW'      ; i:11),
  (S:'cmSPCCW'     ; i:12),
  (S:'cmSPBRAKE'   ; i:13),
  (S:'cmFLOOD'     ; i:15),
  (S:'cmMIST'      ; i:16),
  (S:'cmREF'       ; i:220),
  (S:'cmREFX'      ; i:221),
  (S:'cmREFY'      ; i:222),
  (S:'cmREFZ'      ; i:223),
  (S:'cmREFA'      ; i:224),
  (S:'cmREFB'      ; i:225),
  (S:'cmREFC'      ; i:226),
  (S:'cmUNREF'     ; i:227),
  (S:'cmREFALL'    ; i:228),
  (S:'cmUNREFALL'  ; i:229),
  (S:'cmTOOLS'     ; i:230),
  (S:'cmTOOLEDT'   ; i:231),
  (S:'cmTOOLCHG'   ; i:232),
  (S:'cmTOOLCL'    ; i:233),
  (S:'cmTOOLUNCL'  ; i:234),
  (S:'cmTOOLPAR'   ; i:235),
  (S:'cmTOOLMSR'   ; i:236),
  (S:'cmTOUCHOFF'  ; i:240),
  (S:'cmTOUCHX'    ; i:241),
  (S:'cmTOUCHY'    ; i:242),
  (S:'cmTOUCHZ'    ; i:243),
  (S:'cmTOUCHA'    ; i:244),
  (S:'cmTOUCHB'    ; i:245),
  (S:'cmTOUCHC'    ; i:246),
  (S:'cmZEROALL'   ; i:247),
  (S:'cmOFFSDLG'   ; i:248),
  (S:'cmTOUCHWIZ'  ; i: cmTOUCHWIZ),
  (S:'cmSETUP'     ; i:250),
  (S:'cmLIMITS'    ; i:260),
  (S:'cmOPEN'      ; i:270),
  (S:'cmRUN'       ; i:20),
  (S:'cmRUNLINE'   ; i:21),
  (S:'cmSTOP'      ; i:22),
  (S:'cmSTEP'      ; i:271),
  (S:'cmPAUSE'     ; i:23),
  (S:'cmOPTSTOP'   ; i:24),
  (S:'cmBLOCKDEL'  ; i:25),
  (S:'cmRELOAD'    ; i:272),
  (S:'cmMDIEXEC'   ; i:280),
  (S:'cmMDIHIST'   ; i:281),
  (S:'cmEDITOR'    ; i:290),
  (S:'cmINCRDN'    ; i:320),
  (S:'cmINCRUP'    ; i:321),
  (S:'cmSHIFTUP'   ; i:330),
  (S:'cmSHIFTDN'   ; i:331),
  (S:'cmPARTALGN'  ; i:340),
  (S:'cmUNITS'     ; i:350),
  (S:'cmCLOSE'     ; i:360),
  (S:'cmBACK'      ; i:999),
  (S:'cmSCRIPTBASE'; i:1000),
  (S:'cmSCRIPTS'   ; i:1099),
  (S:'cmFEEDRESET' ; i:cmFEEDRESET));

var
  BtnDefJog: TButtonArray;
  BtnDefJogRef: TButtonArray;
  BtnDefJogTouch: TButtonArray;
  BtnDefJogTool: TButtonArray;
  BtnDefMDI: TButtonArray;
  BtnDefRun: TButtonArray;

const
   MaxColumns = 15;
   {$ifdef VER_24}
   LatheColumns = MaxColumns;
   MillColumns = 12;
   {$endif}
   {$ifdef VER_23}
   LatheColumns = 8;
   MillColumns = 4;
   {$endif}

const
  TitleDef : Array[0..MaxColumns] of string =
    ('POC','TNR','X-Offset','Y-Offset','Z-Offset',
     'A-Offset','B-Offset','C-Offset','U-Offset','V-Offset',
     'W-Offset','Durchmesser','Frontangle','Backangle','OR',
     'Bezeichnung');
  WidthsDef : Array[0..MaxColumns] of integer =
    (30,30,60,60,60,60,60,60,60,60,60,100,60,60,30,200);

  {$ifdef VER_24}
  MillIdcDef: Array[0..MaxColumns] of Integer =
    (0,1,11,2,3,4,5,6,7,8,9,10,15,-1,-1,-1);
  LatheIdcDef: Array[0..MaxColumns] of Integer =
    (0,1,11,2,3,4,5,6,7,8,9,10,12,13,14,15);
  {$endif}

  {$ifdef VER_23}
  MillIdcDef: Array[0..MaxColumns] of Integer =
    (0,1,11,4,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
  LatheIdcDef: Array[0..MaxColumns] of Integer =
    (0,1,11,3,4,12,13,14,15,-1,-1,-1,-1,-1,-1,-1);
  {$endif}

const
  CoordSysMax = 8;
  CoordSys: Array[0..CoordSysMax] of string =
    ('G54','G55','G56','G57','G58','G59','G59.1','G59.2','G59.3');

  CoordSysVar = '5221';
  CoordSysInc = 10;

type
  TJogIncrement = record
    Value: Double;
    Text: string;
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
    IsLathe: Boolean;
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
    MaxSpORide: integer;          // maximum spindle override
    MinSpORide: integer;
    LinearJogSpeed: double;
    AngularJogSpeed: double;
    MaxLinearVel: double;
    MaxAngularVel: double;
    Metric: Boolean;              // metric, default true **FIXME
    UnitVelStr: string;           // linear velocitystr i.e. "inch/min"
    UnitStr: string;              // linear units i.e. inch "in","mm"
    UnitRotStr: string;           // rotation velocity untits i.e. rpm or u/min
    ShowActual: Boolean;          // actual or cmd position
    ShowRelative: Boolean;        // relative or absolute position
    HomingOrderDefined: Boolean;  // check if can do a "home all"
    NoForceHoming: Boolean;
    JogPolarity: Array[0..MaxAxes] of integer;
    JogIncrements: Array[0..MAXJOGINCS] of TJogIncrement; // 0 = continous
    JogIncMax: integer;
    JogIncrement: Double;
    JogContinous: Boolean;
    ActiveAxis: integer;
    StartLine: integer;
    MLimits: TExtents;            // machine limits
    ScriptDir: string;
    Editor: string;
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
    CurrentVel: double;
    Acc: double;
    ActVel: Integer;
    MaxVel: integer;
    ActFeed: Integer;
    MaxFeed: integer;
    ActJogVel: integer;
    MaxJogVel: Integer;
    ActSpORide: integer;
    UnitsChanged: Boolean;
    SpindleORideEnabled: Boolean;
    FeedORideEnabled: Boolean;
  end;

  TGlColorItem = record
    r,g,b,a: single;
  end;

  TGlColors = record
    bg: TGlColorItem;
    table: TGlColorItem;
    limits: TGlColorItem;
    cone: TGlColorItem;
    traverse: TGlColorItem;
    feed: TGlColorItem;
    toolpath: TGlColorItem;
    dim1: TGlColorItem;
    dim2: TGlColorItem;
  end;

  TGlSettings = record
    UseDirect: Boolean;
    UseRGBA: Boolean;
    UseDoubleBuffered: Boolean;
    UseStipple: Boolean;
  end;

  TOnClick = procedure(Sender: TObject) of object;
