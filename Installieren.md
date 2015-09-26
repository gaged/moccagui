# Paketauswahl #

## EMC/LinuxCNC Version ##
Unter dem Register "Downloads" befinden sich verschiedene Pakete:

  1. mocca23: Für alte EMC Installationen (Version max. 2.3.5)
  1. mocca24: Für die neueren EMC Installationen (Version 2.4.x)
  1. mocca25: Für neue Installationen nach dem Wechsel auf den **Namen LinuxCNC** (Version 2.5.x)
  1. mocca26: Für die kommenden Versionen LinuxCNC 2.6.x


## Ubuntu Version ##
In der Beschreibung in der Downloadliste steht jeweils für welches Betriebssystem das Paket gedacht ist.
  * Hardy bezeichnet Ubuntu 8.04
  * Lucid ist Ubuntu 10.04

**Wichtig** ist, dass Sie das passende Paket für ihr Betriebssystem herunterladen.

Für EMC unter Ubuntu 6.06 gibt es leider kein Mocca.
Hier müssen Sie auf ein neues Ubuntu updaten.

# Installation #

Laden Sie also das, für Sie, richtige Paket herunter.
Öffnen Sie es z.B. mit einem Doppelklick, oder übers Terminal mit gdebi.

Danach einfach ein Klick auf Installieren, und kurz warten.

Alternativ können Sie Mocca auch über die Paketquelle installieren.
Dadurch werden Sie automatisch mit den neuesten Updates versorgt.
Die notwendigen Schritte können Sie hier nachlesen:
[LINK](https://code.google.com/p/moccagui/wiki/MoccaUpdate)

# Konfiguration #

Jetzt muss EMC nur noch mitgeteilt werden, das Sie ab jetzt auf Axis verzichten wollen ;)


---

**Update** Es gibt jetzt einen Einrichteassistenten, der das unten beschriebene für Sie erledigt. Dazu muss nur das mocca-setup-de Paket geladen und installiert werden. (->Paketquelle oder Downloadseite)
Danach nur noch noch mocca-setup in einem Terminal aufrufen, der Rest dürfte selbsterklärend sein.

---


Dazu müssen Sie die INI-Datei ihres Profils öffnen. Diese finden Sie normalerweise unter:
/home/**ihr\_name**/emc2/configs/**ihre\_config**/**ihre\_config**.ini

Suchen Sie in der INI-Datei die Zeile "DISPLAY = " und ersetzen Sie das dort genannte Displayprogramm (standard: axis) durch mocca.

So sollte die Zeile dann aussehen:
```
DISPLAY = mocca
```


---


Bei der Installation wurden verschiedene Designs in folgenden Ordner kopiert:
/usr/share/**PAKETNAME**/skins

wenn Sie EMC2.4.x installiert haben, dann lautet der Pfad z.B. so:
/usr/share/mocca24/skins

Suchen Sie sich nun ein Design aus, mit der passenden Auflösung und Sprache.

Den Pfad zu diesem Design müssen Sie nun kurz im Kopf behalten.


---


Gehen Sie an das Ende ihrer INI-Datei.
Beginnen Sie dort eine neue Sektion:
[MOCCA](MOCCA.md)

In diese Sektion kommt die Variable:
CONFIG =

Und diese Variable muss den Pfad zum gewünschten Design beinhalten.
Beispiel:
```
[MOCCA]
CONFIG = /usr/share/mocca24/skins/default_de
```

Jetzt müssen Sie die INI-Datei nur noch abspeichern.
Danach können Sie EMC mit ihrem Profil starten, und die neue Benutzerumgebung genießen.

# Probleme #
## Fehlende Abhängigkeiten ##
Möglicherweise fehlen zu Installation der Mocca-Pakete noch andere Pakete auf Ihrem Rechner.
Stellen Sie sicher, dass Sie diese Pakete installiert haben, und versuchen Sie dann die Installation erneut.

### Ubuntu 8.04 Hardy Heron ###
  * libglib1.2dbg
  * libgdkpixbuf2
  * libgtkglext1

### Ubuntu 10.04 Lucid Lynx ###
  * libgtkglext1