<u>**14.0.0**</u>

- User Interface wurde primär für neue macOS-Versionen überarbeitet.
- Das Programm überprüft beim Start ob die JVM-Parameter korrekt sind und gibt ansonsten eine Warnung aus. Dies kann über die Paramter `-nj` oder `--no-jvm-param-checks` deaktiviert werden.
- Das Programm prüft unter Linux ob der `sun.java2d.uiScale`-Parameter korrekt angegeben wurde. Im Fehlerfall wird ein Warndialog angezeigt.
- *Globale Schriftgröße ändern...*-Befehl wurde unter Linux entfernt da es keine Auswirkungen mehr hat.
- **BUGFIX:** *"Filme/Blacklist öffnen..."* Eintrag wurde in *"Filme/Blacklist bearbeiten..."* umbenannt und die Icons vereinheitlicht.
- **BUGFIX:** Manchmal inkorrekte Darstellung des Bookmark-Status im Film-Tab wurde korrigiert.
- **BUGFIX:** Sendericons werden nun vollständig und hochauflösend gerendert.
- **FEATURE:** Die Toolbars können nun zum Schweben in allen Tabs abgetrennt werden.
- **FEATURE:** Tab Filmbeschreibung wurde neu designed und stellt nun auch das Sendericon dar.
- **FEATURE:** Icons wurden durch hochauflösende Vektorvarianten ersetzt.
- **FEATURE:** Neues Suchfeld inklusive Suchhistorie.
- **FEATURE:** Geoinformationen werden nun als *gesperrt/entsperrt* Icons dargestellt. Detailinformationen werden über Tooltip-Text eingeblendet.
- **FEATURE:** Ein manchmal unsichtbarer Filter-Dialog kann nun über das Menü *Hilfe/Filterdialog-Position zurücksetzen* auf den sichtbaren Monitorbereich zurückgesetzt werden.
- **FEATURE:** Untertitelverfügbarkeit wird nun über ein `cc`-Icon an Anfang des Titels angezeigt. Die Spalte `UT` wird in einer späteren Programmversion entfernt werden.

<u>**13.9.1**</u>

- Die Binaries für macOS waren beschädigt und mussten erneuert werden.

<u>**13.9.0**</u>

- **Dies ist die letzte Version mit Unterstützung für 32-Bit Betriebssysteme. Ab Version 14 werden nur noch 64-Bit Betriebssysteme unterstützt. (Minimum: Windows 10, macOS 10.14 (Mojave), Ubuntu 18.04 LTS, Debian 8, centOS 7)**
- **`ACHTUNG:` Es wird ein neues "Look&Feel" für die Oberfläche verwendet, welches für eine deutlich bessere Lesbarkeit und Schärfe der Anwendung sorgt. Wer irgendwelche "Skalierungstricks" mittels `-Dsun.java2d.uiScale=`, etc. verwendet hat MUSS diese entfernen!**
- **BUGFIX:** Das Filtern nach "ZDF-tivi" funktioniert nun zuverlässig.
- **BUGFIX:** Themen-ComboBox im Blacklist-Bearbeitungsdialog zeigt nun wieder alle Themen ungefiltert an.
- **BUGFIX(Windows 32bit):** Möglicher Absturz mit Stack Overflow Error der JVM wurde behoben.
- **BUGFIX:** Das mehrfach hintereinander durchgeführte Löschen von einzelnen Blacklist-Regeln löscht nun nicht mehr die falschen Regeln. Die Dateninkonsistenz wurde beseitigt.
- **BUGFIX:** Aktualisierung von Bibliotheken die Abstürze vor allem in Verbindung mit Apple Silicon CPUs verursachen konnten.
- **BUGFIX:** Diverse Fehler im Dialog für Abo-Historie wurden beseitigt.
- **BUGFIX:** Algorithmus für Skalierung der Sendericons funktioniert nun richtig.
- **BUGFIX(Windows):** Sendericons sehen nun nicht mehr so bescheiden unter Windows 10 aus.
- **BUGFIX:** Seltener Fehler beim Speichern der Tabellenkonfiguration wurde behoben.
- **BUGFIX:** Die Höhe der Tabelleneinträge für Hilfsprogramme wurde korrigiert.
- **BUGFIX:** Die Höhe der Tabelleneinträge für Programmsets wurde korrigiert.
- **BUGFIX:** Eintragungen im Suchfeld wurde verbessert.
- **FEATURE:** MediathekView nutzt nun das moderne FlatLaf Look&Feel zur Darstellung der Oberfläche. Dies verbessert die Darstellung des Programms unter Windows und Linux erheblich. *Es ist nun nicht mehr möglich, ein anderes L&F zu verwenden da diese veraltet sind und eine bescheidene Darstellung (v.a. unter Windows) verursachen.* **In diesem Zusammenhang ist auch die Einstellungsmöglichkeit einer globalen Skalierung entfallen. Der entsprechende Menüpunkt wurde wieder entfernt.**
- **FEATURE:** Im Einstellungen-Dialog können in Blacklist-Dialog nun gleichzeitig mehrere Regeln gelöscht werden.
- **FEATURE:** *ARTE.EN, ARTE.ES, ARTE.IT und ARTE.PL* wurden als neue Sender hinzugefügt. Die Bereitstellung der Daten erfolgt über den Server sobald sie verfügbar werden. Bei bestehenden Installationen müssen diese bei Bedarf in den Einstellungen aktiviert werden.
- **FEATURE:** Der Zeitraum-Spinner im Filter-Dialog lässt nun Werte von 1-365 Tagen zu. Diese können auch manuell eingetragen und mit der ENTER-Taste bestätigt werden. Für eine unbegrenzte Anzeige muss das **"∞"**-Symbol eingegeben werden. Desweiteren wurde ein Tooltip mit Hilfetext hinzugefügt.
- **FEATURE:** Die Blacklist-Tabelle in *Einstellungen/Blacklist* kann nun gefiltert werden um Einträge zu suchen.
- **FEATURE:** Das Programm entfernt vorhandene Blacklist-Regel-Duplikate nun automatisch.
- **FEATURE:** Das Anlegen von Blacklist-Regel-Duplikaten in den Einstellungen wird nun verhindert.
- **FEATUER:** Im Download-Tab wird der Titel nun zusätzlich in einem Tooltip angezeigt wenn die Spaltenbreite zu klein ist.
- **FEATURE:** Im Filme-Tab wird der Titel nun zusätzlich in einem Tooltip angezeigt wenn die Spaltenbreite zu klein ist.
- **FEATURE:** Die Filter der Senderliste (im Filterdialog) können über das Kontextmenü komplett zurückgesetzt werden.
- **FEATURE:** Die lokale Filmliste kann nun im Menü **"Hilfe/Lokale Filmliste löschen"** gelöscht werden.
- Im Beschreibungsfeld wird das Thema wieder mit angezeigt.
- Log4j2 Bibliothek wurde erneut aktualisiert um potentielle *[Log4Shell](https://nvd.nist.gov/vuln/detail/CVE-2021-44228)* Sicherheitslücke zu schließen.
- Geschwindigkeit der Blacklistfilterung wurde für *große* Blacklisten teils erheblich gesteigert (macOS 12.1 Intel i9 -16%, macOS 12.0.1 Apple M1 -27%, Windows 10 AMD Ryzen 4700U -36%, Ubuntu 21.04 AMD Ryzen 4700U -31%). Testfall war eine Filmliste mit 500k Filmen und reale Blacklisten mit ca. 2500 Einträgen. Alle Betriebssysteme waren 64bit.
- Textlängenbegrenzung für Thema und Titel in Abo-Historie wurde entfernt.
- generelle Optimierungen der Abo-Historie
- Java Version wurde auf 17.0.2 erhöht um Fehler v.a. unter macOS zu beseitigen.
- ffmpeg 5.0 wird für macOS mitgeliefert.
- ffmpeg 5.0 wird für windows 64bit mitgeliefert.
- Livestreams können nicht mehr der Merkliste hinzugefügt werden.

<u>**13.8.1**</u>

- **Java wurde auf Version 17 aktualisiert.**
- **BUGFIX (macOS):** Timeout-Fehler bei Updatesuche während hoher Netzwerkauslastung behoben.
- **BUGFIX (macOS):** möglicher Absturz der App unter macOS Monterey wurde behoben.
- **BUGFIX:** Teilweise inkorrekte Darstellung der ComboBox "Nur die Filme der letzten Tage laden:" wurde behoben.
- **BUGFIX:** Die Filterung der Filmliste nach Tagen filterte zu viele gültige Ergebnisse heraus. Dies wurde behoben.
- **BUGFIX:** Seltener Absturz des Programms beim Start im Zusammenhang mit Abos wurde behoben.
- **BUGFIX:** Minimal- und Maximallänge im Filter lässt sich nun wieder vollständig einstellen.
- **BUGFIX:** Das Filmmodell wurde teilweise zu oft gefiltert und verlangsamte so unnötigerweise das Programm.
- **BUGFIX:** Das Programm deaktiviert während der Modellverarbeitung nun zuverlässiger relevante Steuerelemente.
- **BUGFIX:** Das Einlesen von fehlerhaften Aboeinträgen führt nicht mehr zum Einfrieren des Programms.
- **BUGFIX:** Im Tab Filme wurde ein HQ-Download mit JDownloader angeboten, obwohl keine HQ-URL für den Film existierte.
- **FEATURE:** Während der Modellverarbeitung wird nun rechts unten in der Statusleiste eine Fortschrittsanzeige eingeblendet.
- Filtergeschwindigkeit beim Einlesen der Filmliste wurde verbessert.
- TouchBar support wurde entfernt da die Bibliothek nicht mehr gepflegt wird.
- Log4j2 Bibliothek wurde aktualisiert um potentielle *[Log4Shell](https://nvd.nist.gov/vuln/detail/CVE-2021-44228)* Sicherheitslücke zu schließen.
- macOS **Apple Silicon** native Version als separater Download vorhanden (benötigt mindestens macOS BigSur). Diese wurde noch nicht so ausführlich getestet und kann Fehler beinhalten.

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
