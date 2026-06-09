# Datenschutz bei HLS

Dieses Dokument beschreibt in einfacher Form, welche Daten MediathekView für HLS-Streams an den Server  übermittelt.

MediathekView nutzt diese Daten, um bereits bekannte Informationen zu HLS-Streams wiederzuverwenden und neu erkannte Informationen gesammelt zu übertragen.

## Nicht gesendete Daten

Der Client sendet **keine**:

- öffentliche IP-Adresse des Clients als Anwendungsfeld
- Benutzernamen
- E-Mail-Adressen
- Konto-Identifikatoren
- Dateiinhalte

## Lookup-Anfrage

Wenn MediathekView prüft, ob Informationen zu einem HLS-Stream bereits bekannt sind, werden folgende Angaben übermittelt:

- `m3u8Url`
  - die geprüfte HLS-Playlist-URL
- `quality`
  - die gewünschte Qualitätsstufe, falls verfügbar
  - derzeit `LOW`, `NORMAL` oder `HIGH_QUALITY`
- `country`
  - der vom Client verifizierte zweibuchstabige ISO-Ländercode

Zweck:

- der Server kann prüfen, ob für diese Kombination bereits ein Ergebnis vorliegt
- wenn ein Ergebnis vorhanden ist, kann MediathekView es wiederverwenden und muss den Stream nicht erneut lokal prüfen

## Upload-Anfrage

Wenn MediathekView neue Informationen zu HLS-Streams gesammelt hat, werden diese gebündelt hochgeladen. Dabei enthält jedes Datenpaket:

- `schemaVersion`
- `sentAt`
- `events`

Jeder einzelne Eintrag enthält:

- `timestamp`
  - den Zeitpunkt der Erfassung
- `appVersion`
  - die verwendete MediathekView-Version
- `platform`
  - Betriebssystem und Architektur, zum Beispiel `Mac OS X/aarch64`
- `country`
  - der vom Client verifizierte zweibuchstabige ISO-Ländercode
- `httpStatus`
  - das Ergebnis der Abfrage, zum Beispiel `200`, `403` oder `404`
- `m3u8Url`
  - die geprüfte HLS-Playlist-URL
- `resolutionUrl`
  - die ausgewählte Varianten-URL, falls verfügbar
- `quality`
  - die Qualitätsstufe, falls verfügbar
  - derzeit `LOW`, `NORMAL` oder `HIGH_QUALITY`
- `fileSize`
  - die ermittelte Dateigröße in Bytes
  - `-1` wird für bestimmte bekannte Fehlerfälle wie `403` oder `404` verwendet

## Lokale Queue vor dem Upload

Vor dem Upload speichert der Client ausstehende HLS-Stream-Info-Ereignisse lokal in:

```text
~/.mediathek3/hls-stream-info-data.ndjson
```

Diese Queue wird verwendet, um Uploads zu bündeln und sie bei Bedarf später erneut zu versuchen.
