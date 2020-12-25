<u>**13.7.1**</u>

- Abfrage der Filmgrößen im "Film speichern" Dialog beschleunigt.
- **BUGFIX:** Info-Dialog wird nun angezeigt dass keine neuen Programminfos vorliegen.
- **BUGFIX:** "URL kopieren" Kontextmenü zeigt bei Playlists wieder unterschiedliche Auflösungen an.
- **BUGFIX:** Bandbreitenmonitor funktioniert nun zuverlässig, wenn er beim Start nicht geöffnet war.

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