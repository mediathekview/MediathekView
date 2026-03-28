# Lucene-Suche im Filme-Tab

Dieses Dokument beschreibt, wie die Lucene-Suche im Filme-Tab von MediathekView technisch und praktisch funktioniert.

## Überblick

Im Lucene-Modus arbeitet das Suchfeld direkt auf einem lokal erzeugten Lucene-Index.

Wichtig dabei:

- Der Standard-Suchbereich ist `titel`.
- Die Suchabfrage unterstützt die übliche Lucene-Syntax mit Feldern, Phrasen, Wildcards und Bereichen.
- Führende Wildcards sind erlaubt, also zum Beispiel `*krimi` oder `?atort`.
- Die GUI-Filter für Sender, Zeitraum, Filmlänge, ungesehen, Geo-Blockierung, Abos und Bookmarks werden zusätzlich zur Lucene-Abfrage angewendet.
- Einige Felder sind echte Textfelder, andere Booleans, Datumswerte oder Integer-Felder.

## Wie die Suche arbeitet

Der Ablauf ist:

1. Der eingegebene Suchtext wird als Lucene-Abfrage interpretiert.
2. Falls Suchtext vorhanden ist, wird er gegen den Lucene-Index geparst.
3. Danach werden weitere GUI-Filter als zusätzliche Filter angewendet.
4. Am Ende werden nur die Filme angezeigt, die sowohl zur Suchabfrage als auch zu den übrigen Filtern passen.

## Wichtige Besonderheiten

Bei normalen Textfeldern ist Groß-/Kleinschreibung im Regelfall egal.

Für `sender` sollten Abfragen trotzdem immer in Kleinbuchstaben geschrieben werden, also zum Beispiel `sender:ard`.

## Lucene Query Syntax

### Standardfeld

Wenn nur freier Text eingegeben wird, sucht MediathekView im Feld `titel`.

Beispiele:

```text
tatort
"heute journal"
*krimi
```

Das ist funktional gleichbedeutend mit:

```text
titel:tatort
titel:"heute journal"
titel:*krimi
```

### Grundlegende Operatoren

#### Pflicht, Alternative, Ausschluss

```text
+tatort +muenster
tatort OR polizeiruf
tatort -wien
```

#### Phrasensuche

```text
titel:"heute journal"
beschreibung:"wahre geschichten"
```

#### Gruppierung

```text
(titel:tatort OR titel:polizeiruf) AND sender:ard
```

#### Wildcards

```text
titel:krimi*
titel:*bericht
titel:te?t
```

Hinweis:

- Führende Wildcards sind explizit erlaubt.
- Zu viele breite Wildcard-Abfragen können die Suche merklich verlangsamen.

#### Bereichsabfragen

Bereiche funktionieren besonders für Datums- und Zahlenfelder:

```text
sendedatum:[20250101 TO 20251231]
länge:[1800 TO 5400]
größe:[500 TO 5000]
season:[1 TO 3]
episode:[10 TO 20]
startzeit:[18:00:00 TO 23:00:00]
```

#### Exakte Boolesche Abfragen

Boolesche Felder werden im Index als String `true` gespeichert.

```text
livestream:true
highquality:true
untertitel:true
```

## Verfügbare Suchschlüssel

Die folgende Liste zeigt die im Filme-Tab verfügbaren Suchschlüssel.

| Schlüssel | Typ | Bedeutung | Hinweise |
|---|---|---|---|
| `sender` | String | Sendername | Immer kleingeschrieben abfragen |
| `titel` | Text | Filmtitel | Standardfeld |
| `thema` | Text | Thema | Volltext über Leerzeichen getrennt |
| `beschreibung` | Text | Beschreibung | Eignet sich für Inhaltsbegriffe |
| `livestream` | Boolean | Livestream | Nur `true` ist sinnvoll |
| `highquality` | Boolean | HQ-Version verfügbar | Nur `true` ist sinnvoll |
| `untertitel` | Boolean | Untertitel verfügbar | Nur `true` ist sinnvoll |
| `trailerteaser` | Boolean | Trailer, Teaser oder Vorschau | Nur `true` ist sinnvoll |
| `audioversion` | Boolean | Hörfassung / Audioversion | Nur `true` ist sinnvoll |
| `signlanguage` | Boolean | Gebärdensprach-Version | Nur `true` ist sinnvoll |
| `sendedatum` | Datum als `YYYYMMDD` | Sendetag | Fehlende Werte werden als `19000101` indexiert |
| `wochentag` | Text | Wochentag des Sendetermins | Werte wie `Montag`, `Dienstag` |
| `neu` | Boolean | Film ist neu in der Liste | Nur `true` ist sinnvoll |
| `länge` | Integer | Filmlänge in Sekunden | Werte immer in Sekunden, `0` wenn unbekannt |
| `größe` | Integer | Dateigröße in Megabyte | `0`, wenn unbekannt |
| `duplicate` | Boolean | Film wurde als Duplikat klassifiziert | Nur `true` ist sinnvoll |
| `startzeit` | String `HH:mm:ss` | Startzeit | Lexikografisch sortierbar, daher auch Bereiche möglich |
| `season` | Integer | Staffelnummer | `0`, wenn unbekannt |
| `episode` | Integer | Episodennummer | `0`, wenn unbekannt |

## Erweiterte Beispiele pro Suchschlüssel

### `sender`

```text
sender:ard
sender:zdf
(sender:ard OR sender:zdf) +titel:tagesschau
sender:arte AND beschreibung:dokumentation
```

Hinweis:

- Sender sollten immer kleingeschrieben werden.

### `titel`

```text
titel:"heute journal"
titel:tatort*
titel:(krimi OR thriller)
+titel:reportage -titel:sport
```

### `thema`

```text
thema:nachrichten
thema:"kultur"
thema:(politik OR wirtschaft)
thema:dokumentation AND sender:3sat
```

### `beschreibung`

```text
beschreibung:"wahre begebenheit"
beschreibung:(klimawandel OR energiewende)
beschreibung:*migration*
beschreibung:geschichte AND sender:arte
```

Hinweis:

- `beschreibung` ist oft das beste Feld für inhaltliche Freitextsuche.

### `livestream`

```text
livestream:true
livestream:true AND sender:ard
livestream:true AND (sender:ard OR sender:zdf)
livestream:true AND sender:phoenix
```

### `highquality`

```text
highquality:true
highquality:true AND titel:tatort
highquality:true AND größe:[1000 TO 8000]
highquality:true AND sender:zdf
```

### `untertitel`

```text
untertitel:true
untertitel:true AND sender:arte
untertitel:true AND beschreibung:dokumentation
untertitel:true AND -audioversion:true
```

### `trailerteaser`

```text
trailerteaser:true
trailerteaser:true AND titel:film
trailerteaser:true AND sender:zdf
-trailerteaser:true AND titel:krimi
```

Hinweis:

- In der Praxis ist `-trailerteaser:true` oft nützlicher als die positive Suche.

### `audioversion`

```text
audioversion:true
audioversion:true AND sender:ard
audioversion:true AND untertitel:true
-audioversion:true AND titel:"heute show"
```

### `signlanguage`

```text
signlanguage:true
signlanguage:true AND sender:zdf
signlanguage:true AND beschreibung:nachrichten
-signlanguage:true AND titel:reportage
```

### `sendedatum`

```text
sendedatum:20250309
sendedatum:[20250101 TO 20250331]
sendedatum:[20240101 TO 20241231] AND sender:arte
sendedatum:[20250301 TO 20250309] AND titel:tatort
```

Hinweise:

- Format immer `YYYYMMDD`.
- Fehlende Datumswerte werden als `19000101` gespeichert.

### `wochentag`

```text
wochentag:montag
wochentag:freitag AND sender:3sat
wochentag:(samstag OR sonntag) AND thema:kinder
wochentag:dienstag AND startzeit:[20:00:00 TO 23:00:00]
```

Hinweis:

- Durch den Analyzer sind Kleinbuchstaben in der Abfrage unkritisch.
- Für Livestreams ist `wochentag` in der Praxis nicht hilfreich, weil Livestreams dieses Feld typischerweise nicht haben.

### `neu`

```text
neu:true
neu:true AND sender:ard
neu:true AND sendedatum:[20250301 TO 20250309]
neu:true AND -duplicate:true
```

### `länge`

```text
länge:[0 TO 900]
länge:[1800 TO 5400]
länge:[5400 TO 20000] AND thema:dokumentation
länge:[1200 TO 2400] AND sender:zdf
```

Hinweise:

- Werte immer in Sekunden.
- Beispiele: `1800` = 30 Minuten, `5400` = 90 Minuten.

### `größe`

```text
größe:[0 TO 500]
größe:[700 TO 3000]
größe:[3000 TO 10000] AND highquality:true
größe:[1000 TO 5000] AND titel:tatort
```

Hinweis:

- Werte sind Megabyte.

### `duplicate`

```text
duplicate:true
duplicate:true AND sender:ard
duplicate:true AND titel:tatort
-duplicate:true AND neu:true
```

### `startzeit`

```text
startzeit:"20:15:00"
startzeit:[18:00:00 TO 22:00:00]
startzeit:[05:00:00 TO 09:00:00] AND sender:phoenix
startzeit:[20:00:00 TO 23:59:59] AND sender:ard
```

Hinweis:

- Das Format muss `HH:mm:ss` sein.

### `season`

```text
season:1
season:[1 TO 3]
season:10 AND titel:folge
season:[5 TO 8] AND episode:[1 TO 4]
```

### `episode`

```text
episode:1
episode:[1 TO 10]
episode:12 AND season:3
episode:[100 TO 200] AND sender:zdf
```

## Kombinierte Praxisbeispiele

### Krimis am Abend in guter Qualität

```text
(titel:tatort OR titel:polizeiruf OR beschreibung:krimi) AND highquality:true AND startzeit:[20:00:00 TO 23:30:00]
```

### Arte-Dokumentationen mit Untertiteln aus einem Datumsbereich

```text
sender:arte AND (thema:dokumentation OR beschreibung:dokumentation) AND untertitel:true AND sendedatum:[20240101 TO 20251231]
```

### Neue Folgen einer Serie

```text
neu:true AND season:[1 TO 20] AND episode:[1 TO 999] AND titel:"die sendung"
```

### Lange Reportagen ohne Trailer

```text
thema:reportage AND länge:[2700 TO 20000] AND -trailerteaser:true
```

## Zusammenspiel mit den GUI-Filtern

Nicht alles muss direkt in die Lucene-Abfrage geschrieben werden. Zusätzlich zur Suchabfrage verarbeitet der Filme-Tab noch diese Filter:

- ausgewählte Sender im Filterdialog
- Zeitraum
- Filmlänge min/max
- nur ungesehene Filme
- nur Bookmarks
- Geo-Blockierung ausblenden
- Abos ausblenden

Das heißt:

- Eine Lucene-Abfrage kann korrekt sein und trotzdem weniger Treffer liefern, wenn GUI-Filter aktiv sind.
- Der Zeitraum im GUI wird nicht aus dem Suchtext gelesen, sondern separat als Lucene-Datumsfilter hinzugefügt.

## Typische Stolperfallen

### `sender` nicht kleingeschrieben

Besser:

```text
sender:ard
```

Riskanter:

```text
sender:ARD
```

### `länge` und `größe` erwarten Zahlen

Falsch:

```text
länge:90min
```

Richtig:

```text
länge:[5400 TO 7200]
```

### `sendedatum` erwartet kein deutsches Datumsformat

Falsch:

```text
sendedatum:09.03.2026
```

Richtig:

```text
sendedatum:20260309
```

### Boolesche Felder haben praktisch nur `true`

Statt:

```text
livestream:false
```

besser:

```text
-livestream:true
```

## Kurzreferenz

```text
Freitext                  -> sucht in titel
feld:wert                 -> sucht in einem bestimmten Feld
"exakte phrase"           -> Phrasensuche
* und ?                   -> Wildcards
[von TO bis]              -> Bereichssuche
+term                     -> muss vorkommen
-term                     -> darf nicht vorkommen
(a OR b) AND c            -> Gruppierte Logik
```

## Weiterführende Links

- Offizielle Lucene-Dokumentation zur Suchsyntax:
  [https://lucene.apache.org/core/10_4_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package.description](https://lucene.apache.org/core/10_4_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package.description)

## Fazit

Die Lucene-Suche im Filme-Tab ist deutlich mächtiger als die klassische Suche:

- Freitext ohne Feldname sucht in `titel`
- strukturierte Felder ermöglichen präzise Filter
- Zahlen-, Datums- und Zeitbereiche sind direkt möglich
- komplexe Abfragen lassen sich über `AND`, `OR`, `-`, Phrasen und Wildcards formulieren

Für gute Ergebnisse lohnt es sich, Freitext in `titel` mit strukturierten Filtern wie `sender`, `sendedatum`, `länge`, `untertitel` oder `highquality` zu kombinieren.
