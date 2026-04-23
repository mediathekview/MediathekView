# Suche in der Audiothek

Die Suche im Tab **Audiothek** filtert die angezeigten Einträge nach dem Inhalt des Feldes **Filter**.

Die Suche wird gestartet, wenn im Suchfeld die Eingabetaste gedrückt wird.

Der Suchtext kann über die integrierte Löschen-Funktion im Suchfeld oder mit der Taste `Esc` entfernt werden. Danach wird wieder die vollständige Liste angezeigt.

## Lokale Suche

Ohne weiteren Suchschlüssel wird in allen aktuell sichtbaren Suchspalten gesucht:

- `Sender`
- `Genre`
- `Thema`
- `Titel`
- `Datum`
- `Zeit`

Groß- und Kleinschreibung werden dabei nicht beachtet.

Die Suche arbeitet als Teiltreffersuche.

Beispiele:

```text
tatort
hörspiel
ard
```

Damit werden alle Einträge gefunden, in denen der jeweilige Begriff im sichtbaren Suchbereich vorkommt.

## Suche mit Suchschlüsseln

Die Suche kann auf ein bestimmtes Feld eingeschränkt werden. Dazu wird das Schema

```text
schlüssel:wert
```

verwendet.

Verfügbare Suchschlüssel sind:

- `sender`
- `genre`
- `thema`
- `theme`
- `titel`
- `title`
- `datum`
- `date`
- `zeit`
- `time`
- `dauer`
- `duration`
- `größe`
- `groesse`
- `size`

Beispiele:

```text
sender:ard
thema:krimi
titel:"die drei ???"
datum:12.03.2026
größe:120
```

Bei Werten in Anführungszeichen wird exakt nach diesem Ausdruck gesucht. Ohne Anführungszeichen wird wieder als Teiltreffer gesucht.

Mehrere Suchteile können kombiniert werden, zum Beispiel:

```text
sender:ard thema:hörspiel
titel:welk datum:11.03.2026
```

Dann müssen alle angegebenen Teile passen.

## Online-Suche

Wenn die Checkbox **Online-Suche** aktiviert ist, werden zusätzlich passende Ergebnisse von **Podcastindex** geladen und in der Trefferliste ergänzt.

Diese Suche läuft parallel zur lokalen Suche. Währenddessen zeigt die Werkzeugleiste rechts neben **Online-Suche** einen Aktivitätsindikator an.

Die Online-Suche wird nur bei freiem Suchtext verwendet. Sobald ein Suchschlüssel wie `sender:`, `titel:`, `thema:` oder `datum:` verwendet wird, findet nur noch die lokale Audiothek-Suche statt.

Wird die Checkbox ausgeschaltet, werden nur noch die lokalen Audiothek-Einträge angezeigt.

## Hinweise

Die Anzahl der Treffer wird in der Statusleiste angezeigt.

Die Suchergebnisse hängen auch davon ab, welche Spalten in der Tabelle eingeblendet sind. Bei einer freien Suche ohne Suchschlüssel wird nur in den sichtbaren Suchspalten gesucht. `Dauer` und `Größe` werden dabei nicht berücksichtigt.
