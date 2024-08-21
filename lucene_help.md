MediathekView verwendet im **Experten-Modus** zur Suche nach Filmtiteln nun die Volltextsuche [Apache Lucene](https://lucene.apache.org). Damit einhergehend kann die bisher verwendete Suchfunktionalität nicht mehr genutzt werden, jedoch erschließen sich durch die neu integrierte professionelle Suche viel mehr Möglichkeiten, die Ergebnisse einzugrenzen.

**Wird im Suchfeld nur Text ohne ein unten aufgeführtes Tag eingegeben, ist gds. das Tag `titel` der Suchbereich.**

MediathekView bietet die folgenden *tags* (Achtung: hier sind **immer** Kleinbuchstaben zu verwenden) für die Formulierung von Suchabfragen an:

| Name          | Typ     | Beschreibung                                                 |
| ------------- | ------- | ------------------------------------------------------------ |
| sender        | String  | Dieser entspricht einem der Werte, die im Filter-Dialog in der Senderliste hinterlegt sind. |
| titel         | String  | Titel wie in der Tabelle dargestellt.                        |
| thema         | String  | Thema wie in der Tabelle dargestellt.                        |
| beschreibung  | String  | Die im Programm dargestellte Filmbeschreibung. Vollindiziert. |
| livestream    | Boolean | *true* wenn es sich um einen Livestream handelt, ansonsten nicht definiert. |
| highquality   | Boolean | *true* wenn HQ Variante des Films verfügbar, ansonsten nicht definiert. |
| untertitel    | Boolean | *true* wenn Untertitel für Download verfügbar, ansonsten nicht definiert. |
| trailerteaser | Boolean | *true*, wenn es sich um einen Trailer, Teaser oder eine Vorschau handelt, ansonsten nicht definiert. |
| audioversion  | Boolean | *true*, wenn es sich um eine Hörfassung handelt, ansonsten nicht definiert. |
| signlanguage  | Boolean | *true*, wenn es sich um eine Gebärdensprache-Version handelt. |
| sendedatum    | Date    | Sendedatum im Format *YYYYMMDD*, wenn keines vorhanden wird **19000101** gesetzt. |
| neu           | Boolean | *true*, wenn der Film neu in die Liste aufgenommen wurde, ansonsten nicht definiert. |
| länge         | int     | Filmlänge in Sekunden, **0** wenn keine Information vorhanden ist. |
| größe         | int     | Filmgröße in Megabytes, **0** wenn keine Information vorhanden ist. |

Es ist nicht möglich, Filme anhand von URLs bzw. Teilsegmenten davon zu suchen. 

Um sich mit der Abfragesyntax vertraut zu machen sind nachfolgende Links empfohlen:

1. [Lucene Query Syntax](https://ci-builds.apache.org/job/Lucene/job/Lucene-Artifacts-main/javadoc/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package.description)
2. [Lucene Tutorial](http://lucenetutorial.com/lucene-query-syntax.html)
