Hier sollte alles stehen was für den Bediener der Maschine von Bedeutung ist.

# Bedienung von mocca #

## Umschalten zwischen mm/inch ##

Im Infobereich auf die Einheit mm bzw. Inch (doppelt) klicken.

## Umschalten zwischen Relativ/Absolut ##

Im Infobereich auf Relativ bzw. Absolut (doppelt) klicken.

## Umschalten zwischen Aktuell/Befohlen ##

Im Infobereich auf Aktuell bzw. Befohlen (doppelt) klicken.

## Scripte ##

Beispiel:

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