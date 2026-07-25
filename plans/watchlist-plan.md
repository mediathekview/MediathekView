# Watchlist mit Benachrichtigungs-Glocke im Tab „Filme"

Stand: implementiert und verifiziert (JDK 25, `./mvnw test` — 1567 Tests grün, davon 17 neue Watchlist-Tests).

## Zielbild

- Sendung per Kontextmenü (Filme-Tabelle) auf eine Watchlist setzen: Sender+Thema, optional mit Titel (analog Abo-Kontextmenü).
- Nach jeder Filmlisten-Aktualisierung werden neue Folgen erkannt → roter Punkt an der Glocke in der FilmToolBar + eine OS-Sammelnotification (`NotificationService`).
- Klick auf die Glocke → `JidePopup`-Fenster (Muster: Audiothek-Download-Manager): gestapelte Zeilen-Karten der neuen Folgen. Der rote Punkt verschwindet beim Öffnen, die Zeilen bleiben bis manuell entfernt.
- Pro Zeile: `x`-Button rechts (entfernt nur die Benachrichtigung) und Kontextmenü mit „In Filmliste anzeigen", „Film aufzeichnen...", „Sendung von Watchlist entfernen".
- Fenster schließt automatisch, wenn die letzte Zeile entfernt wird; bei Klick ohne Einträge erscheint der Hinweis „Keine neuen Folgen".
- Verwaltung der Einträge: Kontextmenü-Toggle am Film + Verwaltungsdialog (Filme-Menü „Watchlist verwalten...").

## Architektur-Entscheidung

Eigenständiges Modul — kein Eingriff in die Abo-/Download-Infrastruktur (Abos sind fest mit Download-Pipeline, `AboHistoryController`, Psets und CLI-Flows verknüpft). Wiederverwendete Muster:

- JSON-Persistenz wie `AboRuleStorage` (kotlinx.serialization, Temp-Datei + atomarer Move, tolerantes Lesen)
- Service-Fassade wie `BookmarkServices` (speichert bei jeder Mutation selbst)
- MessageBus-Events (`BaseEvent`), `SwingDispatch` für EDT-Übergänge
- `JidePopup`-Mechanik wie `AudiothekPanel` (Anchor-Button, `AWTEventListener` für Outside-Click, `HIDE_ON_MOVED`, Auto-Hide bei leer)
- Ikonli-Icons via `IconUtils` (`MaterialDesignB.BELL_OUTLINE`, `MaterialDesignC.CLOSE_CIRCLE_OUTLINE`) — keine neue SVG-Ressource nötig

## Domänen-Logik

- **Erkennung:** `DatenFilm.isNew` wird bei jedem Import durch `FilmListImportApplier` gesetzt. `WatchlistServices` abonniert `FilmListReadStopEvent` (feuert nach Import, beim Startup-Lesen der gespeicherten Liste und im CLI-Pfad) und matcht asynchron auf `Dispatchers.IO` (Mutex-serialisiert, Frühausstieg bei leerer Watchlist, Iteration über `allFilms.snapshot()`).
- **Dedup:** `seenUrlKeys` pro Eintrag (komprimierte Keys via `film.storedNormalQualityUrl`).
- **Kein Flood beim Anlegen:** Beim Setzen eines Eintrags werden alle aktuell matchenden URL-Keys in `seenUrlKeys` vorbefüllt (asynchron auf IO, nicht auf dem EDT).
- **Matching:** sender/thema equals-ignore-case; `title` optional contains-ignore-case.
- **Punkt-Zustand:** `hasUnseenNotifications` (persistiert) — `true` bei neuen Treffern, `false` beim Öffnen des Fensters (`markAllSeen`). Die Zeilen-Liste (`notifications`) ist davon getrennt und bleibt bis zum manuellen Entfernen erhalten.
- **Verknüpfung:** Notifications hängen über eine stabile `entryId` (UUID) am Eintrag — nicht am Namen, da Sender+Thema- und Titel-Variante derselben Sendung denselben Namen teilen können.
- **Notification-Payload:** entryId, entryName, sender, thema, title, sendeDatum, urlNormalQuality (für Film-Lookup via `allFilms.getFilmByAnyUrl(url)` bei „In Filmliste anzeigen"/„Film aufzeichnen...").

## Dateien

### Neu: Domäne (`mediathek/daten/watchlist/`)
- `DatenWatchlistEntry.kt` — id (UUID), name, sender, thema, title, `seenUrlKeys`; `matches(film)`; `copy()`
- `WatchlistNotification.kt` — Payload wie oben
- `WatchlistServices.kt` — thread-sichere Eintrags-/Notification-Verwaltung; `addEntryFromFilm` (async, mit Prefill-Snapshot), `removeEntry` (kaskadiert Notifications via `entryId`), `removeNotification`, `markAllSeen`/`hasUnseenNotifications`; Matching + `WatchlistChangedEvent` (async) + OS-Sammelnotification (`MessageType.INFO`, folgt der globalen Notification-Einstellung); Persistenz bei jeder Mutation
- `WatchlistStorage.kt` — JSON `watchlist.json` (Version, DTOs, Temp-File + `moveAtomicallyWithFallback`, tolerantes Lesen, generiert fehlende IDs beim Lesen)

### Neu: UI
- `mediathek/gui/messages/WatchlistChangedEvent.kt` — `BaseEvent`
- `mediathek/gui/watchlist/WatchlistBellButton.kt` — JButton, `BELL_OUTLINE` via `IconUtils.toolbarIcon(...)`; gemalter roter Punkt oben rechts gesteuert via `setNotificationState(hasUnseen, pendingCount)`; Tooltip mit Zustand/Anzahl
- `mediathek/gui/watchlist/WatchlistNotificationPanel.kt` — Muster `AudioDownloadManagerPanel`: ScrollPane + BoxLayout-Y, Zeilen-Karten (Titel fett, „Sender · Thema · Datum"), `x`-Button (CLOSE_CIRCLE_OUTLINE, Hand-Cursor, nur `BUTTON1`); `JPopupMenu` pro Zeile („In Filmliste anzeigen", „Film aufzeichnen..." — deaktiviert wenn Film nicht mehr in der Liste, „Sendung von Watchlist entfernen"); Row-Diffing in `setNotifications(...)`; `emptyListener` nur beim Übergang nicht-leer → leer; Hinweis „Keine neuen Folgen" bei leer
- `mediathek/gui/watchlist/ManageWatchlistDialog.kt` — modal (Muster `ManageAboDialog`), Tabelle Sender/Thema/Titel via einfachem `AbstractTableModel`, „Löschen" (nur bei Selektion), „Schließen"; Refresh auf `WatchlistChangedEvent`, `unsubscribe` in `dispose`
- `mediathek/gui/tabs/tab_film/context/FilmWatchlistContextActions.kt` — Untermenü „Watchlist" („Sendung auf Watchlist setzen" / „...mit Titel..." bzw. „...von Watchlist entfernen"), Muster `FilmAboAndBlacklistContextActions`; deaktiviert ohne Selektion

### Geändert (minimal, additiv)
- `StandardLocations.kt` — `getWatchlistFilePath()`
- `Daten.kt` — `val watchlist = WatchlistServices(filmCatalog.allFilms, notifications)`
- `Main.kt` — `daten.watchlist.loadFromFile()` neben `bookmarks.loadFromFile()` (GUI- und CLI-Pfad)
- `MediathekGui.createTabFilme` — reicht `daten.watchlist` an `GuiFilme` durch
- `GuiFilme.kt` — Konstruktor-Parameter `watchlist`; `WatchlistBellButton` + `JidePopup` (lazy, Anchor = Bell-Button, Audiothek-Flags); `AWTEventListener` für Outside-Click (Registrierung in `init`, Entfernung in `disposePanel`); `@Handler handleWatchlistChangedEvent` → Bell-Status + sichtbares Popup via `SwingDispatch`; Aktionen `showInFilmTable` (Suchfeld + `postActionEvent()`), `recordFilm` (Popup schließen + `startFilmDownloads` → `DialogAddDownload`), `removeWatchlistEntry` (via `entryId`); Menüeintrag „Watchlist verwalten..." im Filme-Menü
- `FilmToolBar.kt` — Bell-Button am rechten Ende (mit Separator)
- `TableContextMenuHandler.kt` / `FilmContextMenuBuilder.kt` / `FilmTableHostAdapters.kt` — Watchlist-Untermenü eingehängt (Host um `watchlist()` erweitert)

## Ablauf „neue Folge"

1. Filmlisten-Update → `FilmListImportApplier` markiert `isNew` → `FilmListReadStopEvent`
2. `WatchlistServices` matcht auf IO, dedupliziert via `seenUrlKeys`, legt Notifications an, speichert, publiziert `WatchlistChangedEvent` + OS-Sammelnotification
3. `GuiFilme`-Handler setzt roten Punkt an der Glocke (EDT)
4. Klick auf Glocke → Popup öffnet, `markAllSeen()` löscht den Punkt (Zeilen bleiben)
5. `x`/Kontextmenü-Aktionen entfernen Zeilen; letzte Zeile → Auto-Hide
6. Zustand überlebt Neustarts (`watchlist.json`)

## Tests & Validierung

- `WatchlistStorageTest` (5): Roundtrip inkl. `seenUrlKeys`/`hasUnseenNotifications`/IDs, Defaults, fehlende Datei, korrupte Datei (toleranter Fallback), unbekannte Zukunfts-Felder
- `WatchlistServicesTest` (12): Prefill-Snapshot beim Anlegen (kein Flood), Titel-Filter, Duplikat-Schutz, Notification nur für neue Treffer, Dedup über Läufe, Titel-Contains-Ignore-Case, Leer-Frühausstieg, `markAllSeen`-Semantik (Punkt aus/Liste bleibt), `removeEntry`-Kaskade nur eigener Notifications, `removeNotification`, `findEntryFor`-Varianten, Persistenz über Service-Instanzen
- Validierung: `./mvnw -q -DskipTests process-sources compile` + `./mvnw test` (1567 Tests, 0 Failures)

## Bekannte Grenzen / Hinweise

- Roter Punkt ohne Zähler (Anzahl im Tooltip).
- „In Filmliste anzeigen" nutzt das normale Suchfeld (Thema-Text) — aktive Filter (z. B. „nur Ungesehene") bleiben wirksam.
- `JidePopup` (jide-oss 3.7.15) referenziert `java.applet.Applet` und ist daher nur bis JDK 25 lauffähig (Entfernung in JDK 26). Das Projekt läuft mit JDK 25; bei einem künftigen JDK-26-Umstieg muss auch der bestehende Audiothek-Download-Manager ersetzt werden.
- CLI-Modus sammelt Notifications in `watchlist.json` mit (kein OS-Popup headless — Backend dort deaktiviert).
- Kein neuer Config-Key nötig (eigene Datei; OS-Notification folgt der globalen Einstellung).
