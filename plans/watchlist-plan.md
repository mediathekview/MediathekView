# Watchlist mit Benachrichtigungs-Glocke im Tab „Filme"

Stand: implementiert und verifiziert (JDK 25, `./mvnw test` — 1598 Tests grün, davon 35 neue Watchlist-Tests).

## Zielbild

- Sendung per Kontextmenü (Filme-Tabelle) auf eine Watchlist setzen: Sender+Thema, optional mit Titel (analog Abo-Kontextmenü).
- Nach jeder Filmlisten-Aktualisierung werden neue Folgen erkannt → roter Punkt an der Glocke in der FilmToolBar + eine OS-Sammelnotification (`NotificationService`).
- Klick auf die Glocke → `JidePopup`-Fenster (Muster: Audiothek-Download-Manager): gestapelte Zeilen-Karten der neuen Folgen. Der rote Punkt verschwindet beim Öffnen, die Zeilen bleiben bis manuell entfernt.
- Pro Zeile: `x`-Button rechts (entfernt nur die Benachrichtigung) und Kontextmenü mit „In Filmliste anzeigen", „Film aufzeichnen...", „Sendung von Watchlist entfernen".
- Fenster schließt automatisch, wenn die letzte Zeile entfernt wird; bei Klick ohne Einträge erscheint der Hinweis „Keine neuen Folgen".
- Verwaltung der Einträge: Kontextmenü-Toggle am Film + Verwaltungsdialog (Filme-Menü „Watchlist verwalten...").

## Architektur-Entscheidung

Eigenständiges Modul — kein Eingriff in die Abo-/Download-Infrastruktur (Abos sind fest mit Download-Pipeline, `AboHistoryController`, Psets und CLI-Flows verknüpft). Die Integration erfolgt über einen neuen MessageBus-Event (`FilmsDownloadStartedEvent`), der beim Start jedes Downloads (Abo oder manuell) gefeuert wird — `WatchlistServices` abonniert diesen und markiert die betroffenen Filme automatisch als gesehen, sodass überflüssige Benachrichtigungen nicht entstehen. Wiederverwendete Muster:

- Versionierte SQLite-Persistenz wie `AboHistoryController` (`PRAGMA user_version`, WAL, transaktionale Writes)
- Service-Fassade wie `BookmarkServices` (speichert bei jeder Mutation selbst)
- MessageBus-Events (`BaseEvent`), `SwingDispatch` für EDT-Übergänge
- `JidePopup`-Mechanik wie `AudiothekPanel` (Anchor-Button, `AWTEventListener` für Outside-Click, `HIDE_ON_MOVED`, Auto-Hide bei leer)
- Ikonli-Icons via `IconUtils` (`MaterialDesignB.BELL_OUTLINE`, `MaterialDesignC.CLOSE_CIRCLE_OUTLINE`) — keine neue SVG-Ressource nötig

## Domänen-Logik

- **Erkennung:** `DatenFilm.isNew` wird bei jedem Import durch `FilmListImportApplier` gesetzt. `WatchlistServices` abonniert `FilmListReadStopEvent` (feuert nach Import, beim Startup-Lesen der gespeicherten Liste und im CLI-Pfad) und matcht asynchron auf `Dispatchers.IO` (Mutex-serialisiert, Frühausstieg bei leerer Watchlist, Iteration über `allFilms.snapshot()`).
- **Dedup:** `seenFilmIds` pro Eintrag über `film.sha256`. Diese Identität ist inhaltsbasiert und damit über Prozessgrenzen stabil; die komprimierten URL-Keys aus `DatenFilm` sind es nicht (prozesslokale Host-IDs) und dürfen nicht persistiert werden.
- **Kein Flood beim Anlegen:** Beim Setzen eines Eintrags werden alle aktuell matchenden Film-IDs in `seenFilmIds` vorbefüllt (asynchron auf IO, nicht auf dem EDT).
- **Matching:** sender/thema equals-ignore-case; `title` optional contains-ignore-case.
- **Punkt-Zustand:** `hasUnseenNotifications` (persistiert) — `true` bei neuen Treffern, `false` beim Öffnen des Fensters über `acknowledgeNotifications()`. Diese Methode quittiert und liefert genau die Menge zurück, die angezeigt wird, damit nie eine ungesehene Meldung stillschweigend quittiert wird. Wird die Liste leer, verschwindet der Punkt ebenfalls (kein Phantom-Badge).
- **Serialisierung:** Ein Operations-Mutex umfasst Mutation *und* Schreiben; jeder vollständige SQLite-Snapshot wird in einer Transaktion ersetzt. Ein fehlgeschlagener Write rollt zurück und hält den Dirty-Zustand für den nächsten Versuch bzw. den Shutdown-Flush.
- **Überlappende Regeln:** Passt eine Folge auf mehrere Einträge, entsteht genau eine Benachrichtigung; alle betroffenen Einträge merken die Folge.
- **Fehlerfälle:** Unbekannte Datenbankversionen und nicht sicher lesbare Datenbanken bleiben unverändert und deaktivieren Writes. Eine neue Datenbank wird zuerst vollständig in einer validierten temporären Datei aufgebaut und danach atomar installiert.
- **Lifecycle:** `close()` deabonniert den MessageBus, wartet laufende Operationen ab und flusht ausstehende Änderungen; verdrahtet in `MediathekGui.closeNotificationCenter()` und im CLI-Pfad.
- **Verknüpfung:** Notifications hängen über eine stabile `entryId` (UUID) am Eintrag — nicht am Namen, da Sender+Thema- und Titel-Variante derselben Sendung denselben Namen teilen können.
- **Notification-Payload:** entryId, entryName, sender, thema, title, sendeDatum, urlNormalQuality (für Film-Lookup via `allFilms.getFilmByAnyUrl(url)` bei „In Filmliste anzeigen"/„Film aufzeichnen...").
- **Abo/Watchlist-Integration:** Wenn ein Film heruntergeladen wird (egal ob Abo oder manuell), markiert `DownloadStartActions.startAll()` den Film über einen `FilmsDownloadStartedEvent` automatisch als gesehen auf der Watchlist. `WatchlistServices.performMarkFilmsSeenByDownload()` fügt die Film-IDs (sha256) zu den `seenFilmIds` aller passenden Einträge hinzu und entfernt ausstehende Benachrichtigungen — das Badge wird geleert, wenn keine Benachrichtigungen mehr vorhanden sind.

## Dateien

### Neu: Domäne (`mediathek/daten/watchlist/`)
- `DatenWatchlistEntry.kt` — immutable `data class`: id (UUID), name, sender, thema, title, `seenFilmIds`; `matches(film)`, `hasSameCriteriaAs(other)`
- `WatchlistNotification.kt` — Payload wie oben
- `WatchlistServices.kt` — `AutoCloseable`; alle Mutationen laufen asynchron über einen Operations-Mutex und sind EDT-sicher: `addEntryFromFilm` (Prefill-Snapshot), `removeEntry(entryId)`, `removeNotification`, `acknowledgeNotifications()`, `hasUnseenNotifications`, `markFilmsSeenByDownloadAndWait()` (für Tests); Matching + `WatchlistChangedEvent` (async) + OS-Sammelnotification (`MessageType.INFO`, folgt der globalen Notification-Einstellung); `handleFilmsDownloadStartedEvent()` reagiert auf Download-Starts und markiert Filme als gesehen; Persistenz mit Dirty-Retry
- `WatchlistPersistence.kt` — schmale Persistenz-Schnittstelle, Snapshot-Typen und geschützte Ladefehler
- `WatchlistDatabaseStorage.kt` — normalisierte SQLite-Datenbank `watchlist.db`: geordnete Einträge/Notifications, separate Seen-Film-IDs und Badge-State; Schema-Version 1, WAL, `synchronous=NORMAL`, Foreign Keys und transaktionale Snapshot-Writes

### Neu: UI
- `mediathek/gui/messages/WatchlistChangedEvent.kt` — `BaseEvent`
- `mediathek/gui/messages/FilmsDownloadStartedEvent.kt` — `BaseEvent` mit den gestarteten `DatenFilm`-Instanzen; wird von `DownloadStartActions.startAll()` synchron veröffentlicht, damit die asynchrone Watchlist-Operation vor einem möglichen Shutdown sicher eingereiht ist
- `mediathek/gui/watchlist/WatchlistBellButton.kt` — JButton, `BELL_OUTLINE` via `IconUtils.toolbarIcon(...)`; gemalter roter Punkt oben rechts gesteuert via `setNotificationState(hasUnseen, pendingCount)`; Tooltip mit Zustand/Anzahl
- `mediathek/gui/watchlist/WatchlistNotificationPanel.kt` — Muster `AudioDownloadManagerPanel`: ScrollPane + BoxLayout-Y, Zeilen-Karten (Titel fett, „Sender · Thema · Datum"), `x` als fokussierbarer `JButton`; Kontextmenü via `componentPopupMenu` plus `inheritsPopupMenu` an den Kindern — ein reiner MouseListener am Row-Panel würde Rechtsklicks auf die Labels nie sehen, da Tooltips dort MouseListener registrieren; Row-Diffing in `setNotifications(...)`; `emptyListener` nur beim Übergang nicht-leer → leer; Hinweis „Keine neuen Folgen" bei leer; `fitToScreen(owner)` berechnet Größe und Bildschirmposition bei jedem Öffnen neu, verschiebt bei Bedarf nach links/oben und hält 10 Pixel Abstand zu allen Bildschirmrändern
- `mediathek/gui/watchlist/ManageWatchlistDialog.kt` — modal (Muster `ManageAboDialog`), Tabelle Sender/Thema/Titel via einfachem `AbstractTableModel`, „Löschen" (nur bei Selektion, View→Model-Indexumrechnung), „Schließen"; Refresh auf `WatchlistChangedEvent`, `unsubscribe` in `dispose`
- `mediathek/gui/tabs/tab_film/context/FilmWatchlistContextActions.kt` — Untermenü „Watchlist" („Sendung auf Watchlist setzen" / „...mit Titel..." bzw. „...von Watchlist entfernen"), Muster `FilmAboAndBlacklistContextActions`; deaktiviert ohne Selektion

### Geändert (minimal, additiv)
- `DownloadStartActions.kt` — veröffentlicht `FilmsDownloadStartedEvent(films)` nach jedem Download-Start synchron; die Watchlist-Operation selbst bleibt asynchron und wird beim Shutdown abgewartet
- `StandardLocations.kt` — `getWatchlistDatabasePath()`
- `Daten.kt` — `val watchlist = WatchlistServices(filmCatalog.allFilms, notifications)`
- `Main.kt` — `daten.watchlist.load()` neben `bookmarks.loadFromFile()` (GUI- und CLI-Pfad)
- `MediathekGui.createTabFilme` — reicht `daten.watchlist` an `GuiFilme` durch
- `GuiFilme.kt` — Konstruktor-Parameter `watchlist`; `WatchlistBellButton` + `JidePopup` (lazy, Anchor = Bell-Button, Audiothek-Flags); `fitToScreen(watchlistBellButton)` liefert vor jedem Öffnen die geklemmte Größe und Bildschirmposition für `showPopup(x, y, owner)`; `AWTEventListener` für Outside-Click, der auch heavyweight Zeilen-Kontextmenüs über die Owner-Kette als „innen" erkennt (Registrierung in `init`, Entfernung in `disposePanel`); `@Handler handleWatchlistChangedEvent` → Bell-Status + sichtbares Popup via `SwingDispatch`; eigener Swing-Coroutine-Scope für alle Watchlist-Aktionen: Öffnen quittiert atomar, `showInFilmTable` und `recordFilm` suchen den Film auf `Dispatchers.IO` (Katalog-Scan nie auf dem EDT) und melden fehlende Filme per Dialog; Menüeintrag „Watchlist verwalten..." im Filme-Menü
- `FilmToolBar.kt` — Bell-Button am rechten Ende (mit Separator)
- `TableContextMenuHandler.kt` / `FilmContextMenuBuilder.kt` / `FilmTableHostAdapters.kt` — Watchlist-Untermenü eingehängt (Host um `watchlist()` erweitert)

## Ablauf „neue Folge"

1. Filmlisten-Update → `FilmListImportApplier` markiert `isNew` → `FilmListReadStopEvent`
2. `WatchlistServices` matcht auf IO, dedupliziert via `seenFilmIds`, legt Notifications an, speichert, publiziert `WatchlistChangedEvent` + OS-Sammelnotification
3. `GuiFilme`-Handler setzt roten Punkt an der Glocke (EDT)
4. Klick auf Glocke → `fitToScreen(bellButton)` klemmt Größe und Position mit 10 Pixel Randabstand, Popup öffnet, `acknowledgeNotifications()` löscht den Punkt (Zeilen bleiben)
5. `x`/Kontextmenü-Aktionen entfernen Zeilen; letzte Zeile → Auto-Hide
6. Download startet (Abo oder manuell) → `DownloadStartActions.startAll()` feuert `FilmsDownloadStartedEvent` → `WatchlistServices` markiert Filme als gesehen, entfernt Benachrichtigungen
7. Zustand überlebt Neustarts (`watchlist.db`)

## Tests & Validierung

- `WatchlistDatabaseStorageTest` (5): geordneter Roundtrip, Schema-Version und `quick_check`, transaktionales Rollback, Schutz unbekannter DB-Versionen, geschützte unlesbare Datenbank und leere Erstinitialisierung
- `WatchlistServicesTest` (27): vollständige Service-Suite gegen SQLite; Prefill, Matching/Dedup, Neustart, Concurrency, Dirty-Shutdown-Retry, Schutz unbekannter DB-Versionen und Download-Integration
- Swing: `WatchlistNotificationPanelTest` (8), `WatchlistBellButtonTest` (4), `FilmWatchlistContextActionsTest` (4) — Kontextmenü-Erreichbarkeit inkl. `inheritsPopupMenu`, `x`-Button-Fokussierbarkeit, Aktions-Callbacks, Leer-Hinweis, Popup-Randabstand/-Position/-Größenwiederherstellung, Tooltip-Zustände, Menü-Toggle und Zielfilm-Stabilität
- Validierung: `./mvnw -q -DskipTests process-sources compile` + `./mvnw test` (1598 Tests, 0 Failures)

## Bekannte Grenzen / Hinweise

- Roter Punkt ohne Zähler (Anzahl im Tooltip).
- „In Filmliste anzeigen" zeigt gezielt die gemeldete Folge in der Tabelle und setzt den Suchtext auf ihren Titel; ein anschließender Reload stellt die normale Filterung wieder her.
- `JidePopup` (jide-oss 3.7.15) referenziert `java.applet.Applet` und ist daher nur bis JDK 25 lauffähig (Entfernung in JDK 26). Das Projekt läuft mit JDK 25; bei einem künftigen JDK-26-Umstieg muss auch der bestehende Audiothek-Download-Manager ersetzt werden.
- CLI-Modus sammelt Notifications in `watchlist.db` mit (kein OS-Popup headless — Backend dort deaktiviert).
- Kein neuer Config-Key nötig (eigene Datei; OS-Notification folgt der globalen Einstellung).
- Popup-Größe und -Position werden bei jedem Öffnen aus der ursprünglichen Zielgröße neu berechnet (`fitToScreen`), sodass das Popup auf kleinen oder gewechselten Bildschirmen 10 Pixel Randabstand hält und auf größeren Bildschirmen wieder seine volle Größe erhält.
