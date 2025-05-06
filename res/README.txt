---------------------------------------------------------------------------
MediathekView
---------------------------------------------------------------------------

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

---------------------------------------------------------------------------
MediathekView startet nicht:
---------------------------------------------------------------------------

https://mediathekview.de/faq/#mediathekview-startet-nicht

* MediathekView benötigt mindestens Java 21 welches mit geliefert wird.

* ZIP-Datei nicht entpackt (Windows): Die Programmdatei wurde direkt im 
  ZIP-Archiv doppelgeklickt. Die ZIP-Datei muss erst entpackt werden, dazu sind 
  alle Dateien aus dem ZIP-Archiv in ein beliebiges Verzeichnis zu kopieren. Dort 
  kann dann die Programmdatei "MediathekView.exe" doppelgeklickt werden.

* Benötigte Dateien wurden aus dem Programm-Ordner gelöscht (Windows): Die 
  benötigten Hilfsprogramme (im Ordner "bin") fehlen, da Dateien aus dem 
  MediathekView-Programmordner gelöscht oder verschoben wurden -> Neuinstallation.


---------------------------------------------------------------------------
Installation:
---------------------------------------------------------------------------
*Windows:
MediathekView kann installiert oder das Zip entpackt werden.
Der Installer (Exe) führt durch die Installation und hinterlegt MediathekView im Menü.

Zip:
- die heruntergeladene ZIP-Datei in einen Ordner entpacken
- den entpackten Ordner ins Benutzerverzeichnis verschieben
- den eben verschobenen MediathekView-Ordner öffnen, 
	die Datei "MediathekView.exe" ansteuern und per Rechtsklick in "Senden an"
	eine Verknüpfung auf den Desktop legen. Von dort aus kann MediathekView 
	dann jeweils gestartet werden.
- die ZIP-Datei kann nach dem Entpacken gelöscht werden
https://mediathekview.de/anleitung/#windows

*Linux:
MediathekView kann installiert oder das tar.gz entpackt werden.
Den Installer gibt es als rpm, deb und sh.
https://mediathekview.de/anleitung/#installation-1



---------------------------------------------------------------------------
Starten:
---------------------------------------------------------------------------
Für Windows (MediathekView.exe), Linux (MediathekView) sind eigene 
Startdateien enthalten, mit welchen MediathekView direkt gestartet werden kann. 

Ansonsten kann man die Programmdatei unter Linux auch so starten:
jre/bin/java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED --add-opens java.desktop/sun.awt.X11=ALL-UNNAMED -ea -cp "MediathekView.jar:dependency/*" mediathek.Main

Achtung: Nur wenn jre/bin mit angegeben wird, wird auch die mitgelieferte JRE genutzt!


===========================================================    
Starten mit zusätzlichen Parametern (Windows)
-----------------------------------------------------------
jre\bin\java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED -ea -cp "MediathekView.jar;./dependency/*" mediathek.Main [Pfad] [Parameter]
jre\bin\java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED -ea -cp "MediathekView.jar;./dependency/*" mediathek.Main C:\Temp
jre\bin\java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED -ea -cp "MediathekView.jar;./dependency/*" mediathek.Main Einstellungen\.mediathek3

Das Programm verwendet das Verzeichnis "Einstellungen" (relativ zur Programmdatei)
oder "c:\temp" für die Einstellungen.
Die Programmeinstellungen (Filmliste, Einstellungen, gesehene Filme) werden 
standardmäßig im Home-Verzeichnis (Benutzer-Verzeichnis) in einem Ordner ".mediathek3" 
gespeichert (beim Start ohne die Angabe eines Pfades).


===========================================================    
Starten im portablen Modus (MediathekView Portable)
-----------------------------------------------------------
Windows: MediathekView_Portable.exe
jre\bin\java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED -ea -cp "MediathekView.jar;./dependency/*" mediathek.Main Einstellungen\.mediathek3


Linux: MediathekView_Portable
java -XX:+UseShenandoahGC -XX:ShenandoahGCHeuristics=compact -XX:+UseStringDeduplication -XX:MaxRAMPercentage=50.0 --enable-native-access=ALL-UNNAMED --add-modules jdk.incubator.vector --add-exports=java.desktop/sun.swing=ALL-UNNAMED --add-opens java.desktop/sun.awt.X11=ALL-UNNAMED -ea -cp "MediathekView.jar:dependency/*" mediathek.Main Einstellungen/.mediathek3