:: das sind verschiedene Möglichkeiten das Programm
:: zu starten, die anderen Aufrufe sind auskommentiert
:: der Pfad zum Programm "PFAD" muss angepasst werden


:: Start in einer extra Dos-Box die minimiert startet
:: Die Parameter "-Xms128M -Xmx1G" helfen bei geringem Arbeitsspeicher. 
start /min javaw -Xms128m -Xmx1024m -jar "C:\Users\PFAD\MediathekView.jar"


:: Start mit mehr Speicher für das Programm
:: java -Xms128M -Xmx1G -jar "C:\Users\PFAD\MediathekView.jar"


:: Start mit Pfad zu Java
:: "%path-to-32-Bit-java%\javaw.exe"  -Xms128M -Xmx1G -jar "C:\Users\PFAD\MediathekView.jar"


:: Es wird ein Proxyserver verwendet.
:: java -jar -Dhttp.proxyHost=proxyserver -Dhttp.proxyPort=8080 "C:\Users\PFAD\MediathekView.jar"


:: Der Parameter "-Djava.net.preferIPv4Stack=true", "-Djava.net.preferIPv6Addresses=true" ermöglicht eine 
:: Verbindung zum Internet, wenn der verwendete Netzwerk-Stack von Java nicht automatisch 
:: richtig erkannt wird, wodurch die Filmliste nicht geladen werden könnte.
:: java -Djava.net.preferIPv4Stack=true -Xms128M -Xmx1G -jar "C:\Users\PFAD\MediathekView.jar"
:: java -Djava.net.preferIPv6Addresses=true -jar "C:\Users\PFAD\MediathekView.jar"

