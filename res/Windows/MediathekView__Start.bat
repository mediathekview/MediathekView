:: Wenn man in den Einstellungen (nicht im Filter!!), das
:: Laden der Filmliste auf die letzten 14 Tage reduziert,
:: bekommt man eine Filmliste mit weniger als 18.000 
:: Einträgen. Damit läßt sich MV leicht mit nur wenig
:: Speicher starten, und ohne Einschränkung anwenden.
:: Nachdem die Einstellungen geändert wurden, und
:: MV neu gestartet wird, versuch es hiermit:
 
:: java -Xms128M -Xmx256M -jar ./@JARNAME@



:: ================================================
:: Das sind verschiedene Möglichkeiten das Programm
:: zu starten, die anderen Aufrufe sind auskommentiert
:: der Pfad zum Programm "PFAD" muss angepasst werden
:: Durch Entfernen des "::" vor einer Zeile wird die Zeile als Befehl interpretiert.
:: Durch Schreiben eines "::" zu Beginn einer Zeile wird diese nicht mehr als Befehl interpretiert.


:: Start in einer extra Dos-Box die minimiert startet
:: Die Parameter "-Xms128M -Xmx1G" helfen bei geringem Arbeitsspeicher. 

start /min javaw -Xms128m -Xmx1024m -jar "C:\Users\PFAD\@JARNAME@"


:: Start mit mehr Speicher für das Programm
:: java -Xms128M -Xmx1G -jar "C:\Users\PFAD\@JARNAME@"


:: Start mit noch mehr Speicher, falls neue Filmliste trotzdem nicht voll geladen werden kann
:: java -Xms128M -Xmx1.5G -jar "C:\Users\PFAD\@JARNAME@


:: Start mit Pfad zu Java
:: "%path-to-32-Bit-java%\javaw.exe"  -Xms128M -Xmx1G -jar "C:\Users\PFAD\@JARNAME@"


:: "Einstellungen/.mediathek3" legt den Ort relativ zur Datei "@JARNAME@"
:: im MV-Programmordner fest. Wer keine portablen Einstellungen verwenden will,
:: löscht diese Zeichenfolge. Wer die Programm-Einstellungen zum Beispiel auf das
:: Laufwerk D: legen will, kann alternativ einen entsprechenden Pfad angeben,
:: z.B. "D:\MediathekView\Einstellungen\.mediathek3". Die Ordner "Einstellungen" bzw.
:: "MediathekView" müssen vorhanden sein bzw. zuerst erstellt werden.
:: start javaw -Xms128M -Xmx1G -jar @JARNAME@ "Einstellungen\.mediathek3" exit


:: Es wird ein Proxyserver verwendet.
:: java -jar -Dhttp.proxyHost=proxyserver -Dhttp.proxyPort=8080 "C:\Users\PFAD\@JARNAME@"


:: Der Parameter "-Djava.net.preferIPv4Stack=true", "-Djava.net.preferIPv6Addresses=true" ermöglicht eine 
:: Verbindung zum Internet, wenn der verwendete Netzwerk-Stack von Java nicht automatisch 
:: richtig erkannt wird, wodurch die Filmliste nicht geladen werden könnte.
:: java -Djava.net.preferIPv4Stack=true -Xms128M -Xmx1G -jar "C:\Users\PFAD\@JARNAME@"
:: java -Djava.net.preferIPv6Addresses=true -Xms128M -Xmx1G -jar "C:\Users\PFAD\@JARNAME@"


:: -Dhttp.proxyHost=proxyserver
:: -Dhttp.proxyPort=8080
:: -Djava.net.preferIPv4Stack=true
:: -Djava.net.preferIPv6Addresses=true
:: -Xms128M
:: -Xmx1G

:: Weitere Infos können z.B. hier gefunden werden
:: https://docs.oracle.com/javase/7/docs/api/java/net/doc-files/net-properties.html
:: https://docs.oracle.com/javase/8/docs/technotes/tools/windows/java.html
:: http://www.heise.de/developer/artikel/Feintuning-der-Speicherbelegung-von-Java-Programmen-mit-visualgc-227258.html


