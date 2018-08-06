===========================================================================
MediathekView
---------------

Lizenz: GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
 -> Link:         https://www.gnu.org/licenses/gpl-3.0.txt
 -> Link deutsch: http://www.gnu.de/documents/gpl-3.0.de.html

Webseite:  https://mediathekview.de
Forum:     https://forum.mediathekview.de
Quellcode: https://github.com/mediathekview/MediathekView
Anleitung: https://mediathekview.de/anleitung/
FAQ:       https://mediathekview.de/faq/


Das Programm MediathekView durchsucht die Online-Mediatheken verschiedener 
Sender und listet die gefundenen Sendungen auf. Die Liste kann mit 
verschiedenen Filtern nach Beiträgen durchsucht werden. Mit einem 
Programm eigener Wahl können die Filme angesehen und aufgezeichnet werden.
Es lassen sich Abos anlegen und neue Beiträge automatisch herunterladen.

===========================================================================

###########################################################################
###########################################################################
---------------------------------------------------------------------------
MediathekView startet nicht:
---------------------------------------------------------------------------

https://mediathekview.de/faq/#mediathekview-startet-nicht

* Java ist nicht oder nicht in der richtigen Version installiert (Java8). Zum 
  Java-Download: http://java.com/de/
  Unter Linux müssen dazu 2 Pakete installiert werden!
  bei arch: java-openjfx und jre8-openjdk,
  bei Ubuntu 16.04: default-jre (entspricht openjdk-8-jre) und openjfx

* ZIP-Datei nicht entpackt (Windows): Die Programmdatei wurde direkt im 
  ZIP-Archiv doppelgeklickt. Die ZIP-Datei muss erst entpackt werden, dazu sind 
  alle Dateien aus dem ZIP-Archiv in ein beliebiges Verzeichnis zu kopieren. Dort 
  kann dann die Programmdatei "MediathekView.exe" doppelgeklickt werden.

* Benötigte Dateien wurden aus dem Programm-Ordner gelöscht (Windows): Die 
  benötigten Java-Bibliotheken (libs) oder die Hilfsprogramme (im Ordner "bin") 
  fehlen, da Dateien aus dem MediathekView-Programmordner gelöscht oder verschoben 
  wurden -> Neuinstallation.


###########################################################################
###########################################################################
---------------------------------------------------------------------------
Installation:
---------------------------------------------------------
*Windows:
MediathekView wird nicht installiert; das Entpacken der heruntergeladenen 
ZIP-Datei ist quasi die Installation:
- die heruntergeladene ZIP-Datei in einen Ordner entpacken
- den entpackten Ordner ins Benutzerverzeichnis verschieben
- den eben verschobenen MediathekView-Ordner öffnen, 
	die Datei "MediathekView.exe" ansteuern und per Rechtsklick in "Senden an"
	eine Verknüpfung auf den Desktop legen. Von dort aus kann MediathekView 
	dann jeweils gestartet werden.
- die ZIP-Datei kann nach dem Entpacken gelöscht werden
https://mediathekview.de/anleitung/#windows

*OS X:
Für OS X 10.7.3 (und neuer) gibt es eine separate Applikation "MediathekView.app"
https://mediathekview.de/anleitung/#mac

*Linux:
MediathekView wird nicht installiert; das Entpacken der heruntergeladenen 
ZIP-Datei ist quasi die Installation.
https://mediathekview.de/anleitung/#installation-1



###########################################################################
###########################################################################
---------------------------------------------------------------------------
Starten:
---------------------------------------------------------------------------
Für Windows (MediathekView.exe), Linux (MediathekView.sh) sind eigene 
Startdateien enthalten, mit welchen MediathekView direkt gestartet werden kann. 

Für OS X 10.7.3 und neuer besteht eine eigenständige Applikation (MediathekView.app);

Ansonsten kann man die Programmdatei auch so starten:
Windows: Doppelklick auf "MediathekView.jar"
Linux (in der Konsole): java -jar MediathekView.jar
OS X: Doppelklick auf die separate Startdatei "MediathekView.command"
    oder via Terminalbefehl: java -jar MediathekView.jar
    (beide Varianten stehen nur zur Verfügung, wenn man die ZIP-Datei ohne die 
    Angabe "OSX" im Dateinamen heruntergeladen hat)



===========================================================    
Starten mit zusätzlichen Parametern
-----------------------------------------------------------

java -jar MediathekView.jar [Pfad] [Parameter]
java -jar MediathekView.jar c:\temp
java -jar MediathekView.jar Einstellungen/.mediathek3

Das Programm verwendet das Verzeichnis "Einstellungen" (relativ zur Programmdatei)
oder "c:\temp" für die Einstellungen.
Die Programmeinstellungen (Filmliste, Einstellungen, gesehene Filme) werden 
standardmäßig im Home-Verzeichnis (Benutzer-Verzeichnis) in einem Ordner ".mediathek3" 
gespeichert (beim Start ohne die Angabe eines Pfades).

java -jar MediathekView.jar -v
Das Programm gibt nur die Versionsnummer aus.



===========================================================    
Starten im portablen Modus (MediathekView Portable)
-----------------------------------------------------------
https://mediathekview.de/anleitung/#starten-im-portablen-modus-mediathekview-portable



===========================================================    
Auto
-----------------------------------------------------------

java -jar MediathekView.jar [Pfad] -auto
java -jar MediathekView.jar -auto
java -jar MediathekView.jar -fastauto

Das Programm startet im Auto-Modus. Es wird die Filmliste aktualisiert und dann 
alle neuen Abos geladen. Das Programm beendet sich dann selbst wieder. Diese 
Funktion eignet sich dazu, alles automatisch aktuell zu halten.

java -Djava.awt.headless=true -jar MediathekView.jar -auto
Wird das Programm ohne GUI (-auto) auf einem Rechner mit grafischer Oberfläche 
gestartet, kann man damit den Splashscreen unterdrücken.




###########################################################################
###########################################################################
---------------------------------------------------------------------------
Hotkeys / Tastenkürzel:
---------------------------------------------------------------------------

Menüs
======
alt+d		Datei
alt+f		Filme
alt+o		Downloads
alt+b		Abos
alt+a		Ansicht
alt+h		Hilfe

Menüpunkte
===========
Einstellungen			F4
neue Filmliste			F5
Programm beenden		ctrl + q

Filter löschen			ctrl + l
Blacklist anzeigen		ctrl + b

Beschreibung anzeigen	F10
Buttons anzeigen		F11


Tab Filme
==============
ctrl + p	Film abspielen
return		Film abspielen (in der Tabelle Filme)
ctrl + d	Download erstellen

ctrl + t	springt in die Tabelle
ctrl + s	Die Tabelle wird nach Sender sortiert und der Focus wir auf 
			die erste Tabellenzeile gesetzt
ctrl + f	springt ins Suchfeld 
			(entweder in der Toolbar oder "ThemaTitel" im Suchpanel wenn es 
			angezeigt wird)
ctrl + i	Infos zum Film anzeigen
ctrl + m	Titel in der Mediensammlung suchen
ctrl + u	Film-URL kopieren
ctrl + h	Film-URL in "HD" kopieren (wenn nicht vorhanden, dann normale URL)
ctrl + k	Film-URL in "Klein" kopieren (wenn nicht vorhanden, dann normale URL)
ctrl + g	Filme als gesehen markieren
ctrl + n	Filme als ungesehen markieren


Tab Downloads
==============
ctrl + d	Download starten
Rücktaste	Download zurückstellen
Entf		Download dauerhaft löschen
ctrl + w	Downloads aktualisieren
return		Download ändern (in der Tabelle Downloads)

ctrl + t	springt in die Tabelle
ctrl + i	Infos zum Film anzeigen
ctrl + m	Titel in der Mediensammlung suchen
ctrl + u	URL kopieren
ctrl + g	Filme als gesehen markieren
ctrl + n	Filme als ungesehen markieren	


Tab Abos
=========
Entf		Abos löschen
Return		Abos ändern (in der Tabelle Abos)

ctrl + t	springt in die Tabelle
