<u>**13.8.1**</u>

- **BUGFIX (macOS):** Timeout-Fehler bei Updatesuche während hoher Netzwerkauslastung behoben.
- **BUGFIX:** Teilweise inkorrekte Darstellung der ComboBox "Nur die Filme der letzten Tage laden:" wurde behoben.
- **BUGFIX:** Die Filterung der Filmliste nach Tagen filterte zu viele gültige Ergebnisse heraus. Dies wurde behoben.
- **BUGFIX:** Seltener Absturz des Programms beim Start im Zusammenhang mit Abos wurde behoben.
- **BUGFIX:** Minimal- und Maximallänge im Filter lässt sich nun wieder vollständig einstellen.
- **BUGFIX:** Das Filmmodell wurde teilweise zu oft gefiltert und verlangsamte so das Programm.
- **BUGFIX:** Das Programm deaktiviert während der Modellverarbeitung nun zuverlässiger relevante Steuerelemente.
- **FEATURE:** Während der Modellverarbeitung wird nun rechts unten in der Statusleiste eine Fortschrittsanzeige eingeblendet.
- Filtergeschwindigkeit beim Einlesen der Filmliste wurde verbessert.
  
<u>**13.8**</u>

- **Java wurde auf Version 16 aktualisiert.**
- **MediathekView verwendet eine neue Speicherverwaltung für die 64bit Programmversionen.** *Die Nutzung der vmoptions-Datei wird damit hinfällig.* Das Programm reserviert nun per Default maximal 50% des vorhandenen Arbeitspeichers und sorgt dafür dass zu jeder Zeit nur der wirklich notwendige Speicher belegt wird. Im Endeffekt verbraucht MediathekView nun deutlich weniger RAM über die Laufzeit als es bisher der Fall war, hat aber die Chance bei Bedarf kurzfristig mehr Speicher als bisher anzufordern. **Dies funktioniert nur mit den offiziellen MediathekView Packages.** Bei nicht-offizieller Anpassung der Startskripte muss darauf geachtet werden das **kein Oracle JDK** verwendet wird. Der neue Garbage Collector wird von Oracle nicht unterstützt. *Es wird empfohlen die vmoptions-Datei nicht mehr zu verwenden.*
- **macOS:** MediathekView funktioniert in Zusammenarbeit mit **AltTab v6.15.1 und neuer.** In Zusammenarbeit mit dem Entwickler konnte eine Lösung für das Problem gefunden werden.**Die Nutzung des offiziellen App-Bundle ist dafür zwingend erforderlich!**
- **Die Mediensammlung wurde entfernt.** Es gibt deutlich bessere Alternativen dafür.
- **mediadb.txt** (die alte Mediensammlung) wird beim Start (wenn vom OS unterstützt) in den Papierkorb verschoben. Ansonsten wird sie gelöscht.
- Datenbanksupport für Nutzung auf Systemen mit wenig Arbeitsspeicher entfernt. MV benötigt mindestens 768MB RAM um halbwegs zu laufen. **Aufgrund der Größe der Filmlisten wird dies nicht empfohlen!**
- Menüeintrag *"Downloads/Alle Downloads um xx:yy Uhr starten"* in *"Downloads/Alle Downloads zeitverzögert starten..."* umbenannt.
- Infodialog hinzugefügt wenn keine Downloads für ein zeitverzögertes Starten vorliegen.
- Der Dialog *Bandbreitennutzung* zeigt nun sämtlichen verursachten Netzwerktraffic von MV an, nicht mehr nur Downloads.
- **FEATURE:** Online-Suche nach Thema oder Titel kann über das Kontextmenü im Filme-Tab mit verschiedenen Anbietern durchgeführt werden.
- **FEATURE:** Die Sender-Website zu einem Film kann nun per Kontextmenü im Film-Tab an JDownloader übergeben werden.
- **FEATURE:** In den PSets kann der Parameter `%w` genutzt werden, um die Sender-Website-URL des Films an ein Programm zu übergeben.
- **FEATURE(macOS):** Download-Button im Update-Dialog hinzugefügt.
- **FEATURE:** Im Tab "Filme" kann über das Kontext-Menü *"In Zwischenablage kopieren"* nun auch Film-Thema und Titel kopiert werden.
- **FEATURE:** Im Tab "Filme" wurde das Kontextmenü übersichtlicher gegliedert.
- **FEATURE(Linux/Windows):** MediathekView kann mittels portable Modus nun auf einem Rechner mit mehreren *unabhängigen* Instanzen betrieben werden.
- **FEATURE:** Im Tab "Beschreibung" kann nun der Link zur Webseite mittels Kontextmenü *"URL kopieren"* extrahiert werden.  
- **BUGFIX:** ffmpeg-Downloads sollten nun auch bei erhöhtem Aufkommen von Frameverlusten bei erfolgreichem Abschluss keinen Fehler mehr anzeigen.
- **BUGFIX:** In *"Einstellungen\Aufzeichnung & Abspielen\Set bearbeiten"* wurde das Springen des Cursor an das Textende in den Eingabefeldern behoben.
- **BUGFIX:** Livestreams werden beim Abspielen nicht mehr als gesehen markiert.
- **BUGFIX(macOS):** Im Tab "Beschreibung" werden Titel nun fett dargestellt.
- **BUGFIX:** Weißer Hintergrund der Filminfo im "Film speichern" Dialog zeigt je nach Plattform nun korrekte Hintergrundfarbe an.
- **BUGFIX:** Datum wird in unterschiedlichen Zeitzonen wieder korrekt dargestellt.
- **BUGFIX:** Manuell angelegte Downloads werden nicht mehr automatisch gestartet wenn in *"Einstellungen/Erweitert"* der automatische Start von Abos ausgewählt wurde.
- **BUGFIX(Linux):** RPM-Signaturfehler von install4j behoben.
- **BUGFIX:** In der *history.db* wird nur noch die URL hinterlegt. Titel und Thema werden nicht mehr gespeichert da sie unnötig sind.**Die Datenbank ist kein Nachschlagewerk und soll auch nicht von Hand modifiziert werden!**
- **BUGFIX:** SRT-Zeitangaben werden nun gemäß Spezifikation erstellt.
- **BUGFIX:** Inkorrekte Fehlerausgabe während Wiedergabe eines Films korrigiert.

<u>**13.7.1**</u>

**ACHTUNG(macOS): Die Verwendung des Programms "AltTab" führt zu häufigen Programmfehler wie Einfrieren/Abstürze,etc. Es wird dringend davon abgeraten MediathekView und AltTab gleichzeitig zu betreiben.**

- Abfrage der Filmgrößen im "Film speichern" Dialog beschleunigt.
- Performance des Filme-Tab bei Nutzung deutlich verbessert.
- Geschwindigkeit des Filterns nach Sendern wurde deutlich verbessert.
- Speicherverbrauch reduziert
- Ausgabe der ffmpeg-Informationen während eines Downloads wird nur noch bei aktiviertem erweiterten Loggen ausgegeben.  
- **BUGFIX:** Info-Dialog wird nun angezeigt dass keine neuen Programminfos vorliegen.
- **BUGFIX:** "URL kopieren" Kontextmenü zeigt bei Playlists wieder unterschiedliche Auflösungen an.
- **BUGFIX:** Bandbreitenmonitor funktioniert nun zuverlässig, wenn er beim Start nicht geöffnet war.
- **BUGFIX:** Livestreams werden nun nicht mehr bei eingeschränktem Laden der Filmliste oder durch den Zeitraum-Filter herausgefiltert.
- **BUGFIX:** Infodatei zeigt statt -1 nun keine Größeninformationen mehr an, wenn diese nicht ermittelt werden kann.
- **BUGFIX:** "Abos verwalten"-Dialog konnte nicht mit Tastenkombination oder Escape geschlossen werden.
- **BUGFIX:** Fehlerhafte RegExp im Suchfeld lassen das Programm nicht mehr einfrieren, ungültige RegExp im Suchfeld werden nicht mehr auf die Filmliste angewendet.
- **BUGFIX:** Es können nun keine "fehlerhaften" Einträge im Filter-Zeitraum Spinner eingeben werden.
- **BUGFIX:** Markierung eines Films als gesehen in der Liste hat eine höhere Priorität als Geoblocking.
- **FEATURE:** Abos verwalten Dialog nutzt nun zur Darstellung des Aktiv/Inaktiv-Zustands keine eigenen Icons mehr.
- **FEATURE:** Filter-Zeitraum Spinner schlägt nun bei Erreichen der Start- oder Endwerte um (wraparound).

<u>**13.7**</u>

- **Erstellt vor dem Update zwingend ein Backup eurer Konfiguration! 
Ein Mischbetrieb von 13.7 und älteren Versionen ist ohne Verlust von Downloadhistorie und anderen Parametern NICHT möglich!!!**
- **(macOS): Minimum Version des Betriebssystems für das signierte Pogramm: 10.11 El Capitan**
- **(macOS): Aufgrund der Richtlinien zur Notarisierung der Applikation musste der Speicherort des ffmpeg binary erneut geändert werden.** Vorhandene Programmset müssen *manuell* angepasst werden auf folgenden Speicherort: **bin/ffmpeg**. 
- JavaFX auf Version 15.0.1 aktualisiert.
- Java JVM Version 15 ist nun Voraussetzung fü das Programm.
- Die gesehen/manuell gedownloadet Historie wurde vom Textformat auf eine SQLite Datenbank umgestellt. Die Migration erfolgt automatisch beim Start der neuen Version. **Backup vorher erstellen!**
- **FEATURE (macOS):** Programmgröße wurde deutlich reduziert
- **FEATURE (macOS):** *Eigene Hilfsprogramme* sind für Apple Silicon optimiert.
- **FEATURE (Windows/Linux):** Die Schriftgröße der gesamten Applikation (Anteile ohne JavaFX) lässt sich nun anpassen via *"Hilfe/Globale Schriftgröße ändern..."*. Um die Änderungen vollständig anzuwenden ist ein Neustart des Programms erforderlich. Es kann zu Darstellungsproblemen in Dialogen kommen wenn die Schrift zu groß ist. Dies verbessert die Lesbarkeit mit HighDPI-Bildschirmen.
- Dialog "Filminformation" nach JavaFX portiert
- **FEATURE:** Schriftgröße der Tabelle im <u>Tab "Filme"</u> lässt sich nun wieder vergrößern/verkleinern und zurücksetzen (über das Kontextmenü des Tabellenheader). Anpassungen erfolgen jeweils in 2er Schritten.
- **FEATURE:** Die Speicherzuweisung für das Programm kann nun über den Menüeintrag *"Hilfe/Speicherzuweisung ändern..."* angepasst werden. *Dies funktioniert nur bei Nutzung der offiziellen Binaries.*
- **FEATURE:** Sender können nun direkt beim Laden der Filmliste ausgeschlossen werden. **Bitte entfernt ggf. Eure entsprechenden Einträge in der Blacklist mit der ihr bisher Sender ausgeblendet habt!** Dies erhöht dann auch zusätzlich etwas die Geschwindigkeit des Programms.
- **FEATURE:** MediathekView meldet beim Programmstart ob fehlerhafte RegEx vorhanden sind. **Es ist die Aufgabe des Nutzers diese zu beseitigen, eine Automatismus hierfür ist nicht vorhanden.**
- **FEATURE:** Erweitere Log-Ausgaben können aktiviert werden mit dem Parameter '-e' bzw. '--enhanced-logging'.
- **FEATURE:** F12-Taste zeigt nun den Filterdialog an.
- **FEATURE:** Einstellung der CheckBox *"Alte Filmliste nicht löschen, nur erweitern"* wird nun gespeichert und wiederhergestellt.
- **FEATURE:** Filme können im Tab "Filme" mittels Kontextmenü auch via JDownloader geladen werden. Dieser muss hierzu im Hintergrund laufen.
- **FEATURE:** "Filmliste laden"-Dialog kann in Position und Größe angepasst werden. Die Änderungen werden beim erneuten Öffnen des Dialogs wieder angewendet.
- **FEATURE (macOS):** Es wird nun statt *MediathekView Shutdown Helper* ein neues integriertes Programm genutzt um nach Downloads den Rechner herunter zu fahren. Dies kann nun auch über das **Terminal konfiguriert** werden. Der Benutzer kann nachfolgend angeben, ob der Rechner immer heruntergefahren werden oder nur schlafen soll: `defaults write org.mediathekview.mv_shutdown_helper shutdownAction -string "shutdown"` für Ausschalten des Rechner, oder zum Schlafen: `defaults write org.mediathekview.mv_shutdown_helper shutdownAction -string "sleep"`. Die Eingaben müssen jeweils nur einmal im Terminal durchgeführt werden und haben dauerhaft Bestand. Sie werden jeweils angewendet wenn man auf Downloads beim Beenden wartet und "Rechner herunterfahren" aktiviert hat. Standardverhalten ist das Ausschalten des Rechners.
- **BUGFIX (macOS):** Programm blieb nach Installation macOS 11.1 Update beim Start hängen.  
- **BUGFIX (macOS):** TouchBar wurde unter macOS 11.1 beim Start nicht angezeigt.
- **BUGFIX (Windows):** Nicht dargestelltes Desktop-Icon wird nun dargestellt.
- **BUGFIX (Windows/Linux):** Install4j Fehlermeldung bezüglich Java Version wurde aktualisiert.
- **BUGFIX (Windows):** Unter Windows konnte es vorkommen dass ein Speichern nach dem Laden der Filmliste nicht möglich war.
- **BUGFIX (32bit):** Falsch benanntes ffmpeg-Programm wurde umbenannt.
- **BUGFIX (32bit):** Absturz des Programms beim ersten Start auf 32bit System behoben.
- **BUGFIX:** Livestreams können nicht mehr als gesehen markiert werden.  
- **BUGFIX:** Einige Filme wurden nicht korrekt als Hörfassung klassifiziert.
- **BUGFIX:** Fehler bei der Darstellung des freien Speicherplatzes im Download-Dialog behoben.
- **BUGFIX:** In die Infodatei wird nun die wirklich genutzte Film-URL und die reale Größe geschrieben.
- **BUGFIX:** Darstellungsfehler in der Filmliste behoben.
- **BUGFIX:** Berechnung der Zeilenhöhe berücksichtigt nun die jeweiligen Font Metrics.
- **BUGFIX:** Im Kontextmenü des Tab Film wurde der Eintrag "Film-URL kopieren" in "URL kopieren" umbenannt. Gleichzeitig wird nun geprüft ob es sich um einen regulären Film mit mehreren URLs handelt oder um eine PlayList. Dementsprechend wird die Auswahl nun korrekt angeboten.
- **BUGFIX:** Selektierte Einträge werden nun - konform zu den Betriebssystemen - nicht mehr fett hervorgehoben.
- **BUGFIX:** Filme ohne Längenangabe werden nicht mehr durch Minimal-Längen Filter herausgefiltert.
- **BUGFIX:** Fehler behoben, dass manchmal keine Daten in der Abo-Tabelle dargestellt werden.  
- **BUGFIX:** Minimal- und Maximallänge des Filmlängenfilters beeinflussen sich nun nicht mehr.
- **BUGFIX:** Tabelle der Farbeinstellungen ist nun nicht mehr zu klein.
- **BUGFIX:** Liste erweitern beim manuellem Filmliste laden funktioniert nun auch, wenn das Adressfeld leer ist.
- **BUGFIX:** Einträge ohne Datum werden nun nicht mehr mit fiktivem Datum 1.1.1970 in die Filmliste sortiert.
- **BUGFIX:** Splash screen wird nun nicht mehr über allen Fenstern dauerhaft dargestellt.
- **BUGFIX:** Livestreams werden nicht mehr in der Historie aufgezeichnet.
- **BUGFIX:** "Abos verwalten"-Dialog wählt nun bei gesetztem Filter nicht mehr die falschen Abos aus.
- **BUGFIX:** Farbauswahl bei Psets für Buttons werden nun angewendet auf allen Plattformen.