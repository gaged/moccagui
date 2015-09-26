Die Buttons kann man frei gestalten...

Mocca braucht in der (Emc) Ini-Datei nur noch den Pfad zu einem Verzeichniss in dem ein Layout und eine Config- Datei ist. z.B.:

```
[MOCCA]
config=/home/dein_name/fraese
```

in der

# config.xml #
können die Buttons der unteren Buttonleiste FREI gestaltet werden, diese werden dann zur Laufzeit von Mocca geladen: Auch die Jog Increments Button werden hier definiert.

Beispiel:

```
<menurun>
<item index="0" cmd="cmRUN" bitmap="" text="Start"/>
<item index="1" cmd="cmSTOP" bitmap="" text="Stop"/>
<item index="2" cmd="cmSTEP" bitmap="" text="Schritt"/>
<item index="3" cmd="cmPAUSE" bitmap="" text="Pause"/>
<item index="4" cmd="cmRUNLINE" bitmap="" text="Start ab Zeile"/>
<item index="5" cmd="cmOPTSTOP" bitmap="" text="Opt.-Stops"/>
<item index="6" cmd="cmBLOCKDEL" bitmap="" text="/ Blöcke"/>
<item index="7" cmd="cmRELOAD" bitmap="" text="Neu Laden"/>
<item index="8" cmd="cmOPEN" bitmap="" text="Öffnen..."/>
<item index="9" cmd="cmEDITOR" bitmap="" text="Editor..."/>
</menurun>
```

Um ein Bild auf den button für "Start" zu legen:

```
<item index="0" cmd="cmRUN" bitmap="start.bmp" text="Start"/>
```

folgende "Commands" stehen zur verfügung. Manche machen allerdings noch nichts z.B. cmTOOLCL das für Werkzeug klemmen vorgesehen ist, oder cmTOOLMSR (Tool Mearsure) zm Vermessen des Wkz)

```
 cmABORT = 0
 cmESTOP = 1
 cmMACHINE = 2
 cmJOG = 3
 cmAUTO = 4
 cmMDI = 5
 cmSPMINUS = 201
 cmSPPLUS = 202
 cmSPCW = 11
 cmSPCCW = 12
 cmSPBRAKE = 13
 cmFLOOD = 15
 cmMIST = 16
 cmREF = 220
 cmREFX = 221
 cmREFY = 222
 cmREFZ = 223
 cmREFA = 224
 cmREFB = 225
 cmREFC = 226
 cmUNREF = 227
 cmREFALL = 228
 cmUNREFALL = 229
 cmTOOLS = 230
 cmTOOLEDT = 231
 cmTOOLCHG = 232
 cmTOOLCL = 233
 cmTOOLUNCL = 234
 cmTOOLPAR = 235
 cmTOOLMSR = 236
 cmTOUCHOFF = 240
 cmTOUCHX = 241
 cmTOUCHY = 242
 cmTOUCHZ = 243
 cmTOUCHA = 244
 cmTOUCHB = 245
 cmTOUCHC = 246
 cmZEROALL = 247
 cmOFFSDLG = 248
 cmSETUP = 250
 cmVIEW = 260
 cmLIMITS = 260
 cmOPEN = 270
 cmRUN = 20
 cmRUNLINE = 21
 cmSTOP = 22
 cmSTEP = 271
 cmPAUSE = 23
 cmOPTSTOP = 24
 cmBLOCKDEL = 25
 cmRELOAD = 272
 cmMDIEXEC = 280
 cmMDIHIST = 281
 cmEDITOR = 290
 cmINCRDN = 320
 cmINCRUP = 321
 cmSHIFTUP = 330
 cmSHIFTDN = 331
 cmPARTALGN = 340
 cmUNITS = 350
 cmBACK = 999
 cmSCRIPTBASE = 1000
 cmSCRIPTS = 1099
 cmCLOSE = 360
 cmFEEDRESET = 400
```

durch einfügen von
```
<jogincrements>
    <item index="0" title="Durchgehend" value=""/>
    <item index="1" title="1,000" value="1.0"/>
    <item index="2" title="0,100" value="0.1"/>
    <item index="3" title="0,010" value="0.01"/>
    <item index="4" title="0,001" value="0.001"/>
</jogincrements>
```

im Stamm der Datei (Unterhalb `<CONFIG>`), werden die Knöpfe zur Auswahl der Schrittweite festgelegt.

## `<GLOBAL>` in der config.xml ##
Außerdem existiert ein Abschnitt `<global>`, in diesem können einige Einstellungen vorgenommen werden.
Falls er noch nicht existiert, kann er einfach ein Child von `<CONFIG>` eingefügt werden.

### Display-Mode der Spindeldrehzahl ###
```
<global>
(...)
    <integer name="SPINDLE_SPEED_DISPLAYMODE" value="1" />
</global>
```
  * 0 bedeutet, dass, falls die Spindel steht, die Default\_Spindle\_Speed angezeigt wird.
  * 1 bedeutet, dass immer die aktuelle Drehzahl angezeigt wird.
  * 2 bedeutet, dass im Falle der stehenden Spindel die Drehzahl gezeigt wird, mit der die Spindel loslaufen wird

### Einheit der Spindeldrehzahl ###
Standardmäßig wird rpm als Einheit verwendet. So lässt sich das ändern:
```
<global>
(...)
    <string name="SPINDLE_SPEED_UNITS" value="U/min" />
</global>
```

### Dezimalstellen der Drehzahl ###
Möchte man sich die Spindeldrehzahl mit Dezimalstellen anzeigen lassen, kann man deren Anzahl so einstellen:
```
<global>
(...)
    <integer name="SPINDLE_SPEED_DECIMALS" value="1" />
</global>
```

### Durchmesser des Kantentasters ###
Falls man einen Kantentaster verwendet, und der Durchmesser automatisch verrechnet werden soll, muss dieser hier eingestellt werden:

```
<global>
(...)
    <float name="EDGEFINDERDIA" value="5" />
</global>
```


# jog.xml #
Definiert die Lager der Jog Button, wie X+ / X- / Y+ / Y- usw.
Der Hintergrund der eiunzelnen Fenster kann mit der Color Eigenschaft verändert werden.
```
<component name="JogClientForm"....
<ident name = "Color" value="clBlue"/>
```
Ändert den Hintergrund in Blau.
Die Farbwerte können auch als HEX Werte mit vorangestellten $-Zeichen eingegeben werden:
```
<ident name="Color" value="$00FA9A"/>
```
ergibt strahlendes Grün.

Eine Farbauswahl findet man z.B. unter:
http://www.uni-magdeburg.de/counter/rgb.txt.shtml

# mdi.xml #
Beschreibung des Fensters, welches im MDI Modus sichtbar ist.

# mocca.xml #
In dieser Datei werden z.B. die Button der rechten Seitenleiste und der unten angeordneten Knöpfe, die nicht in der Knopfleiste sind definiert.

Der Startmodus und die Rahmenart kann wie folgt festgelegt werden

```
<CONFIG>
  <Component>
    <component name="MainForm" class="TMainForm">
      <properties>
        <ident name="BorderStyle" value="None"/>
```

Gültige Werte sind "NONE", "SINGLE", "SIZEABLE"

Knöpfe können auch unsichtbar gemacht werden, indem im properties Bereich Vissible = False setzt.
Die Größe definiert Height und With
die Lage wird durch Top und Left bestimmt.
```
<component name="ButtonSpindlebrake" class="TMocButton">
     <properties>
            <boolean name="Visible" value="false"/>
            <integer name="Left" value="1164"/>
            <integer name="Height" value="80"/>
            <integer name="Top" value="720"/>
            <integer name="Width" value="100"/>
            <string name="Command" value="cmSPBRAKE"/>
            <boolean name="ShowClicks" value="true"/>
            <string name="Caption" value=""/>
            <ident name="Color" value="clBtnFace"/>
            <string name="Bitmap" value="spindlebrake.png"/>
            <string name="Hint" value="Spindelbremse Ein- oder Ausschalten."/>         
     </properties>
</component>
```
eingefügt wird.

# mocmain.xml #
**Achtung**
**Hier bin ich mir nicht sicher!!**
Hauptbildschirm, legt die Größe und Lager der unterschiedlichen Client Forms fest, mocca.xml überschreibt die Einstellungen der mocmain.xml

# offsetdialog.xml #
Beschreibung des Dialogfensters zum Bearbieten der Koordinatensysteme

# run.xml #
Definiert das Aussehen der Clientform, die angezeigt wird, wenn Modus Programm gewählt wurde.

# tooledit.xml #
Definiert das Aussehen, des Dialoges zum Verwalten der Werkzeugdatenbank.

# touchoffdialog.xml #
Diese Datei definiert das Aussehen des PopUp Fensters des Antastdialoges.
Das ist das Fenster, wo immer nur eine Kante angetastet wird und kein Kantentaster verrechnet wird.

# touchoffwiz.xml #
Diese Datei definiert das Aussehen des PopUp Fensters des Antastdialoges, bei dem der Durchmesser eines Kantentasters berücksichtigt werden kann.