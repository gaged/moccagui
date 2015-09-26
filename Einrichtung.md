Diese wiki Seite ist für Einrichter bestimmt. Hier sollte alles stehen, was zum Einrichten von EMC2 mit mocca Oberfläche notwendig ist, insbesondere die Ergänzungen zum ini file.

# mocca as EMC2 GUI #

Using mocca as a GUI for EMC2:

  1. Get mocca compiled (see wiki comiling mocca) and installed or download and install the binary
  1. edit your EMC ini file: `DISPLAY = mocca`, see below for more details
  1. start emc using the ini-file mentioned above

# mocca als EMC2 Oberfläche #

mocca als Grafikfrontend für EMC2 einstellen:

  1. Mocca gemäß Wiki kompilieren und installieren oder Binary herunterladen und installieren
  1. Die ini Datei der Maschine bearbeiten: `DISPLAY = mocca`, weitere Einträge siehe unten
  1. EMC starten, mocca wird geladen

# Mocca Ini- Parameter: #

In der Sektion DISPLAY muss `DISPLAY = mocca` eingetragen werden. Weitere optionale Parameter:

| **Parameter** | **Bereich** | **Default** | **Bedeutung** |
|:--------------|:------------|:------------|:--------------|
|WINDOW\_SIZE   |1-3          |1            |1=normal, 2=Vollbild, 3=Maximiert|
|BUTTON\_SIZE   |small,medium,large |large        |Größe der Buttons (Rechte und untere Leiste) |
|FONT\_SIZE     |8-16         |10           |Schriftgröße allgemein |
|FONT\_BOLD     |0-1          |0            |Fettschrift    |
|DRO\_FONT\_SIZE |10-72        |36           |Schriftgröße der Achsanzeige |
|GLRGBA         |0-1          |1            |RGBA Parameter der OpenGl Erweiterung |
|GLDOUBLEBUFFERED |0-1          |1            |DOUBLEBUFFERED Parameter der OpenGl Erweiterung |
|GLDIRECT       |0-1          |1            |GLDIRECT Parameter der OpenGl Erweiterung |
|SCRIPT\_DIR    |Pfad         |             |Suchpfad für Scripts |


Bei Problemen mit MesaGl Treibern und Grafikkarten, die OpenGl Directbuffer nicht unterstützen kann der Wert GLDIRECT entsprechend verändert werden.

Beispiel:
```
DISPLAY = mocca
WINDOW_SIZE = 3
BUTTON_SIZE = medium
FONT_SIZE = 10
FONT_BOLD = 1
DRO_FONT_SIZE = 36
```

## Scripte ##

Beispiel:

"SCRIPT\_DIR = /home/your\_name/myscripts"
Mit SCRIPT\_DIR wird der Suchpfad für Scripts angegeben. Mocca lädt Scripts aus diesem Verzeichniss. Scripts haben die Datei-Endung .mdi Beispiel: toolchange.mdi

Scripts werden in der Ini-Datei fortlaufend mit SCRIPT\_0 .. SCRIPT\_9 definiert.
Beispiel:

SCRIPT\_0
NAME = Werkzeuglänge vermessen
SCRIPT = /home/your\_name/measure.mdi

SCRIPT\_1
NAME = Schmierzyklus
SCRIPT = /home/your\_name/lube.mdi

hierbei ist:
"NAME = Button1"
Mit Name wird der Text bezeichnet der im Button für das Script angezeigt wird.

SCRIPT = /home/your\_name/script.mdi
Mit Script wird das ausführbare Script (NC-Code) bezeichnet das beim drücken des Buttons "Button1" ausgeführt wird.