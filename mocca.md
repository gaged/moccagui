Hier sollte alles stehen was zum kompilieren von mocca aus den sourcen erforderlich ist.
Wer das fertige binary nicht verwenden kann/will bzw eigene Änderungen am source code vornehmen will, ist hier richtig.

## Übersicht ##

Voraussetzungen:
  * Installiertes EMC2 2.3.4, einschließlich -dev
  * Installiertes lazarus, siehe wiki seite "lazarus"

Kompilieren und Installieren erfordert folgende Schritte
  1. Source code herunterladen bzw. aktualisieren
  1. Kompilieren mit makefile
  1. Installieren

## Source code ##
### herunterladen ###
  1. `svn checkout http://moccagui.googlecode.com/svn/trunk/ mocca`
  1. `cd mocca`
### aktualiseren ###
Wenn das Verzeichnis mocca durch svn checkout bereits angelegt wurde, kann es mit svn update immer wieder auf den neuesten Stand aktualisiert werden. Mit -r XX auch auf eine beliebige Version XX.
  1. `cd mocca`
  1. `svn update [-r XX]`

## Kompilieren mit makefile ##
  1. Ins src Verzeichnis wechseln: `cd src`
  1. Makefile anpassen, siehe [mocca#makefile](mocca#makefile.md)
  1. Alles kompilieren und linken: `make all`

## Installieren ##
  1. Nach ../lib wechseln: `cd ../lib`
  1. Mocca ausführbar machen: `chmod a+x mocca`
  1. Einen symbolischen Link in /usr/bin einfügen: `sudo ln -s mocca /usr/bin/mocca`
Das letztere muss nur einmalig gemacht werden

## makefile ##

Das Makefile muss den lokalen Gegebenheiten angepasst werden.

Dies betrifft insbesondere die Pfade zu EMCDIR und ggf. zu lazarus.

Zusätzlich kann es erforderlich sein, die Include Verzeichnisse zu erweitern (Pfade ggf. anpassen).

Sollte es beim Start von Mocca zu einem X-Server Fehler kommen, so ist im Makefile die Zeile

```
WM=gtk2
```
durch
```
WM=gtk
```
zu ersetzen und neu zu kompilieren. Ggf. müssen noch einige Libraries nachinstalliert werden, siehe Wiki Lazarus installieren.

Wenn beim linken eine Fehler mit
```
undefined reference to `__dso_handle'
```
auftaucht, im makefile -UDSOHANLDE durch -DDSOHANDLE ersetzen (Fehler des C-Compilers bzw der zugehörigen Dateien)

## mocca.inc ##

Die Datei mocca.inc enthält einige $define direktiven. Eine davon ($DEFINE OWNGL) wird dazu verwendet das Lazarus OpenGlControl durch ein internes TGlControl zu ersetzen. Wer also Lazarus NICHT mit dem Package LazOPenGlContext.0.0.1 verwendet MUSS hier $DEFINE OWNGL definieren.