
==============================================================    
Weitere Infos im Wiki:
http://sourceforge.net/p/zdfmediathk/wiki/Anleitung
==============================================================    

Das Programm MediathekView durchsucht die Online-Mediatheken verschiedener 
Sender und listet die gefundenen Sendungen auf. Die Liste kann mit 
verschiedenen Filtern nach Beiträgen durchsucht werden. Mit einem 
Programm eigener Wahl können die Filme angesehen und aufgezeichnet werden.
Es lassen sich Abos anlegen und neue Beiträge automatisch herunterladen.



###########################################################################
###########################################################################
---------------------------------------------------------------------------
Installation
---------------------------------------------------------
*Windows:
MediathekView wird nicht installiert; das Entpacken der heruntergeladenen 
ZIP-Datei ist quasi die Installation:
- die heruntergeladene ZIP-Datei in einen Ordner entpacken
- den entpackten Ordner ins Benutzerverzeichnis verschieben
- den eben verschobenen MediathekView-Ordner öffnen, 
	die Datei "MediathekView__Start.exe" ansteuern und per Rechtsklick in "Senden an"
	eine Verknüpfung auf den Desktop legen. Von dort aus kann MediathekView 
	dann jeweils gestartet werden.
- die ZIP-Datei kann nach dem Entpacken gelöscht werden

*OS X
Für OS X 10.7.3 (und neuer) gibt es eine separate Applikation "MediathekView.app"
http://sourceforge.net/p/zdfmediathk/wiki/Anleitung/#mac

*Linux
MediathekView wird nicht installiert; das Entpacken der heruntergeladenen 
ZIP-Datei ist quasi die Installation.
http://sourceforge.net/p/zdfmediathk/wiki/Linux/#manuelle-installation



###########################################################################
###########################################################################
---------------------------------------------------------------------------
Starten
---------------------------------------------------------
Für Windows (MediathekView__Start.exe), Linux (MediathekView__Linux.sh) sind eigene 

Startdateien enthalten, mit welchen MediathekView direkt gestartet werden kann. 

Für OS X 10.7.3 und neuer besteht eine eigenständige Applikation (MediathekView.app);
für OS X 10.6 liegt die Startdatei "MediathekView__Mac_Start.command" vor.


Ansonsten kann man die Programmdatei auch so starten:
Windows: Doppelklick auf "MediathekView.jar"
Linux (in der Konsole): java -jar MediathekView.jar
OS X: Doppelklick auf die separate Startdatei "MediathekView__Mac_Start.command"
    oder via Terminalbefehl: java -jar MediathekView.jar
    (beide Varianten stehen nur zur Verfügung, wenn man die ZIP-Datei ohne die 
    Angabe "OSX" im Dateinamen heruntergeladen hat)



===========================================================    
Starten mit zusätzlichen Parametern
---------------------------------------------------------

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
---------------------------------------------------------
Weitere Infos im Wiki:
http://sourceforge.net/p/zdfmediathk/wiki/Anleitung/#starten



===========================================================    
Auto
---------------------------------------------------------

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
Hotkey
---------------------------------------------------------

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




###########################################################################
###########################################################################
---------------------------------------------------------------------------
Build
---------------------------------------------------------
Wer das Programm selbst übersetzten will, sollte sich Netbeans installieren:
http://www.netbeans.org/

Die Quelltexte gibt es hier (Repository: MediathekView):
https://github.com/xaverW

In Netbeans ein neues Projekt "with existing Sources" anlegen und den Ordner mit 
den Quelltexten ("src") importieren.

Es müssen noch die benötigten Libs importiert werden:
(im Ordner "libs")

Dann kann das Projekt übersetzt und gestartet werden.


--------------------------------------------------------------------------------
Die Windows-Startdatei "MediathekView__Start.exe" wurde mit:
++ Launch4j ++ (Cross-platform Java application wrapper, 
http://launch4j.sourceforge.net )
erstellt. Das Config-File dafür ist "launch4j.xml"
--------------------------------------------------------------------------------

