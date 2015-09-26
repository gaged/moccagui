# Vorwort #

EMC 2.4.x funktioniert nur mit Ubuntuversionen ab Hardy Heron also Ubuntu 8.04<br />
Mit älteren Ubuntuversionen ist bei 2.3.5 Schluss.<br />
Mit 2.4.0 wurde ein neues Format für die Werkzeug Tabellen eingeführt, diese sollten allerdings automatisch konvertiert werden.


# Update #

Das Updaten ist sehr einfach.
Man benötigt nur 2 Minuten Zeit und das Administrator-Passwort seines Betriebssystems.

## Schritt 1 ##

Öffnet die Einstellungen für die Paketquellen:
<br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step1.jpg' />

## Schritt 2 ##

Wechselt auf den Reiter "Software von Drittanbietern",<br />
dann wählt die markierte Zeile aus, und klickt auf Bearbeiten: <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step2.jpg' />

## Schritt 3 ##

Ändert das emc2.3 in emc2.4. Danach mit Ok bestätigen <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step3.jpg' />

## Schritt 4 ##

Jetzt das ganze nochmal mit dieser Zeile: <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step4.jpg' />

## Schritt 5 ##

Wieder das 2.3 in 2.4 ändern und mit Ok bestätigen: <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step5.jpg' />

## Schritt 6 ##

Wenn ihr jetzt auf Schließen klickt, kommt folgende Meldung.<br />
Hier klickt ihr "Neu laden" und wartet ein bisschen ab. <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step6.jpg' />

## Schritt 7 ##

Falls sich danach bei Euch die Aktualisierungsverwaltung nicht von alleine öffnet, könnt ihr dies hier selber machen: <br /><br />
<img src='http://www.burning-world.de/fotos/emc/update2.4/step7.jpg' />

## Schritt 8 ##

Über die Aktualisierungsverwaltung oder Synaptic oder per Terminal wird jetzt die neueste 2.4.x Version von EMC installiert. <br />
Jetzt müssen noch ein paar Anpassungen durchgeführt werden:<br />
  1. Öffnet die Ini-Datei eurer EMC-Konfiguration
  1. Sucht die Zeile mit NML\_FILE = ...
  1. Entfernt diese Zeile
<br />
Weitere Änderungen sind nur notwendig, wenn ihr Änderungen an der HAL vorgenommen habt, oder die hostmot2 Firmware verwendet. <br />
Weiter Infos dazu findet ihr hier:
[Link](http://wiki.linuxcnc.org/cgi-bin/emcinfo.pl?UPDATING#Changes_between_2_3_x_and_2_4_x)