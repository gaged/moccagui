# Grundidee #

Die meisten Pakete für Ubuntu gibt es ja über Paketquellen zum Download.
Dadurch wird man bei Erscheinen von neuen Versionen direkt darauf aufmerksam gemacht.

Um diesen Vorteil für Mocca auch zu nutzen, gibt es speziell für Mocca ebenfalls eine Paketquelle.

# Pro & Kontra #

Pro ist der erhöhte Komfort. Man muss nicht mehr hier auf der Seite nachschauen, ob es schon eine neue Version gibt, und sie dann manuell installieren.

Kontra ist die Sicherheit:
Theoretisch kann ich (oder jemand der sich Zugang zur Paketquelle verschafft hat) irgendein in Ubuntu vorhandenes Paket mit Schadcode versehen.
Wenn er nun angibt das dieses Paket das allerneueste ist, will Ubuntu dieses nun installieren.

Allerdings wird mit dem unten beschriebenen Verfahren vor der Installation angezeigt, dass das neue Paket nicht authentifiziert ist.

Wenn also ein anderes Paket als mocca nicht authentifiziert ist, einfach nicht installieren.

Genauere Informationen zu den Sicherheitsrisiken findet ihr hier:
http://wiki.ubuntuusers.de/Fremdquellen

# Einrichtung #

Nehmen wir also die Einrichtung der neuen Quelle in die Hand.

(In Klammern stehen Anweisungen für Ubuntu 8.04 falls diese sich von 10.04 unterscheiden)

## Schritt 1 ##

Öffnet unter System > Systemverwaltung den Punkt "Software-Paketquellen"
Dazu müsst ihr euer Administratorpasswort eingeben.

## Schritt 2 ##

Unter dem Reiter "Andere Software" klickt ihr auf "Hinzufügen"
(Ubuntu 8.04: "Software von Drittanbietern")

## Schritt 3 ##
Im sich öffnenden Fenster gebt ihr folgende Zeile ein:
` deb http://mocca.burning-world.de lucid unstable `

Wenn ihr noch Ubuntu 8.04 Hardy Heron benutzt, muss die Zeile so aussehen:
` deb http://mocca.burning-world.de hardy unstable `

Danach das Hinzufügen mit einem Klick bestätigen.
("Software-Paketquelle hinzufügen")

## Schritt 4 ##
Jetzt könnt ihr das Fenster "Software-Paketquellen" schließen.
Dabei bekommt ihr eine Meldung das ihr die Paketlisten neu laden müsst.
Also klickt ihr auf "Neu laden"

Jetzt werden die Paketinformationen heruntergeladen, danach schließt sich das Fenster.

## Ende ##

Wenn ihr jetzt unter System > Systemverwaltung die Aktualisierungsverwaltung aufruft, wird Mocca in der Liste angezeigt. (Sofern eine neue Version vorhanden ist)

Wenn ihr dort auf "Aktualisierungen installieren" klickt, öffnet sich ein Fenster mit der Paketliste.
Dort findet sich auch die Überschrift "NICHT AUTHENTIFIZIERT" mit Mocca darunter.

Sollte hier ein Paket sein, dass nichts mit Mocca zu tun hat, **ABBRECHEN!** klicken.
Sollte dieser Fall eintreten, schreibt mir bitte umgehend eine email mit dem Paketnamen, und dem Datum+Uhrzeit eures Update-versuches.

(Solltet ihr bereits eine andere Fremdquelle hinzugefügt haben, und deshalb die Authentifizierungs-Warnung angezeigt werden, müsst ihr mir natürlich keine Mail schreiben)


# Erstinstallation #

Wenn ihr Mocca noch nicht installiert habt, könnt ihr das nach den obigen Schritten besonders einfach tun.

## Schritt 1 ##
Ihr müsst euch für ein Paket entscheiden:
  1. Ihr benutzt EMC2 mit einer Version <= 2.3.5 dann ist euer Paket **mocca23**
  1. Ihr benutzt EMC2 Version 2.4.x. Euer Paket heisst dann **mocca24**
  1. Für die kommenden **LinuxCNC** 2.5.x Versionen ist das Paket dann **mocca25**

## Schritt 2 ##

Jetzt öffnet ihr ein Terminal.
Dort gebt ihr folgende Zeile ein:

` sudo apt-get install ____Paketname____ `
Statt _Paketname_ setzt ihr den Namen aus Schritt 1 ein.
Also so, wenn ihr EMC 2.4 benutzt:
` sudo apt-get install mocca24 `

## Schritt 3 ##

Dann werdet ihr euer Rootpasswort eingeben müssen.
Kurz darauf habt ihr Mocca installiert.

## Schritt 4 & Ende ##

Als letzt müsst ihr EMC noch einrichten, damit Mocca auch verwendet wird.
Wie das geht lest ihr hier nach:
[LINK](http://code.google.com/p/moccagui/wiki/Installieren#Konfiguration)

### Autor ###

Benjamin Brockhaus <benjaminbrockhaus /klammeraffe\ gmx /klecks\ de>