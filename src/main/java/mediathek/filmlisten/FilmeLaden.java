package mediathek.filmlisten;

import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.StandardLocations;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.reader.FilmListReader;
import mediathek.gui.duplicates.CommonStatsEvaluationTask;
import mediathek.gui.duplicates.FilmDuplicateEvaluationTask;
import mediathek.gui.messages.FilmListReadStopEvent;
import mediathek.gui.tasks.BlacklistFilterWorker;
import mediathek.gui.tasks.FilmlistWriterWorker;
import mediathek.gui.tasks.LuceneIndexWorker;
import mediathek.gui.tasks.RefreshAboWorker;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.http.MVHttpClient;
import okhttp3.HttpUrl;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.awt.*;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.UnknownHostException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;

public class FilmeLaden {
    private record StatusBarWidgets(JLabel label, JProgressBar progressBar) {
    }

    private static final Logger logger = LogManager.getLogger(FilmeLaden.class);
    private static final String NETWORK_NOT_AVAILABLE = "Netzwerk nicht verfügbar";
    private static final String NO_UPDATE_AVAILABLE = "Es ist keine aktuellere Filmliste verfügbar.";
    /**
     * HTTP error code for not found.
     */
    private static final int HTTP_NOT_FOUND = 404;
    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();
    private final Daten daten;
    private final FilmListReader filmListReader = new FilmListReader();
    private final EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen;
    private boolean onlyOne;

    public FilmeLaden(Daten daten) {
        this.daten = daten;
        filmListReader.addAdListener(new ListenerFilmeLaden() {
            @Override
            public synchronized void start(ListenerFilmeLadenEvent event) {
                notifyStart(event);
            }

            @Override
            public synchronized void progress(ListenerFilmeLadenEvent event) {
                notifyProgress(event);
            }

            @Override
            public synchronized void fertig(ListenerFilmeLadenEvent event) {
                // handled by the async import methods below
            }
        });
    }

    private void showNoUpdateAvailableDialog() {
        final var ui = MediathekGui.ui();
        if (ui != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()) {
            JOptionPane.showMessageDialog(ui,
                    NO_UPDATE_AVAILABLE,
                    Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
        } else {
            logger.info(NO_UPDATE_AVAILABLE);
        }
    }

    private boolean hasNewRemoteFilmlist(String sourceUrl) {
        boolean result = false;
        logger.trace("hasNewRemoteFilmList()");
        boolean showDialogs = GuiFunktionen.getFilmListUpdateType() != FilmListUpdateType.AUTOMATIC;

        HttpUrl filmListUrl = HttpUrl.get(sourceUrl);
        final String storedEtag = FilmListMetadataStore.readEtag(sourceUrl);
        final Request.Builder requestBuilder = new Request.Builder()
                .url(Objects.requireNonNull(filmListUrl))
                .head();
        if (storedEtag != null && !storedEtag.isBlank()) {
            requestBuilder.header("If-None-Match", storedEtag);
        }

        final Request request = requestBuilder.build();
        try (Response response = MVHttpClient.INSTANCE.getHttpClient().newCall(request).execute();
             ResponseBody _ = response.body()) {
            if (response.code() == 304) {
                result = false;
            } else if (response.isSuccessful()) {
                final String remoteEtag = response.header("ETag");
                result = storedEtag == null || storedEtag.isBlank() || !storedEtag.equals(remoteEtag);
            } else {
                logger.warn("hasNewRemoteFilmlist HTTP Response Code: {} for {}", response.code(), response.request().url());
                if (response.code() == HTTP_NOT_FOUND || response.code() == 405)
                    result = true;
            }

            if (!result) {
                if (showDialogs) {
                    showNoUpdateAvailableDialog();
                } else
                    logger.info(NO_UPDATE_AVAILABLE);
            }
        } catch (UnknownHostException ex) {
            logger.debug(ex);
            if (showDialogs && MediathekGui.ui() != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()) {
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), NETWORK_NOT_AVAILABLE, ex);
            } else
                logger.warn(NETWORK_NOT_AVAILABLE);

        } catch (IOException ex) {
            logger.error("IOxception:", ex);
            if (MediathekGui.ui() != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()) {
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), "Netzwerkfehler aufgetreten!", ex);
            }
        } catch (Exception ex) {
            logger.error("Filmlist update check failed", ex);
            if (showDialogs && MediathekGui.ui() != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()) {
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), "Ein unbekannter Fehler ist aufgetreten.", ex);
            }
        }

        return result;
    }

    /**
     * Determine whether we want to perform a remote update check.
     * This will be done if:
     * 1. don´t have film entries
     * 2. dateiUrl is either empty or string starts with http
     * 3. our filmlist is old enough that we dont use diff list - we dont check them.
     *
     * @return true if we need to load a new list, false if we should not load a remote list
     */
    private boolean performUpdateCheck(ListeFilme listeFilme, String dateiUrl) {
        if (listeFilme.isEmpty()) {
            return true;
        }

        //remote download is using an empty file name!...
        //or somebody put a web adress into the text field
        if (dateiUrl.isEmpty() || dateiUrl.startsWith("http")) {
            final String remoteSource = dateiUrl.isEmpty()
                    ? StandardLocations.getFilmListUrl(
                    listeFilme.getMetaData().canUseDiffList() ? FilmListDownloadType.DIFF_ONLY : FilmListDownloadType.FULL)
                    : dateiUrl;
            return hasNewRemoteFilmlist(remoteSource);
        }

        return true;
    }

    private void prepareHashTable() {
        hashSet.clear();
        fillHash(daten.getListeFilme());
    }

    private void displayLogInfo(@NonNull ListeFilme listeFilme) {
        logger.info("Alte Liste erstellt am: {}", listeFilme.getMetaData().getGenerationDateTimeAsString());
        logger.info("  Anzahl Filme: {}", listeFilme.size());
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms());
    }

    public boolean loadFilmlist(String dateiUrl, boolean immerNeuLaden) {
        return loadFilmlist(dateiUrl, immerNeuLaden, FilmListLoadOptions.normal());
    }

    public boolean loadFilmlist(String dateiUrl, boolean immerNeuLaden, FilmListLoadOptions options) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        options = Objects.requireNonNull(options);
        final var listeFilme = daten.getListeFilme();

        if (!performUpdateCheck(listeFilme, dateiUrl))
            return false;

        logger.trace("loadFilmlist(String,boolean,FilmListLoadOptions)");
        logger.info("");
        displayLogInfo(listeFilme);

        if (!canStartLoad()) {
            return true;
        }

        beginLoad();

        if (immerNeuLaden) {
            // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
            listeFilme.clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
        }

        final var days = getLoadNumDays();
        if (dateiUrl.isEmpty()) {
            logger.info("Filmliste laden (Netzwerk)");
            importFromUrl(listeFilme, diffListe, days, options);
        } else {
            logger.info("Filmliste laden von: {}", dateiUrl);
            listeFilme.clear();
            importFromFile(dateiUrl, listeFilme, days, options);
        }
        return true;
    }

    public void updateFilmlist(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start");
        logger.info("");
        displayLogInfo(daten.getListeFilme());

        if (!canStartLoad()) {
            return;
        }

        beginLoad();

        logger.info("Filmliste laden von: {}", dateiUrl);
        final var sourceUrl = dateiUrl.isEmpty()
                ? StandardLocations.getFilmListUrl(FilmListDownloadType.FULL)
                : dateiUrl;
        importFromFile(sourceUrl, diffListe, getLoadNumDays(), FilmListLoadOptions.normal());
    }

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    public void removeAdListener(ListenerFilmeLaden listener) {
        listeners.remove(ListenerFilmeLaden.class, listener);
    }

    private boolean canStartLoad() {
        return !istAmLaufen;
    }

    private void beginLoad() {
        istAmLaufen = true;
        prepareHashTable();
        daten.getListeFilmeNachBlackList().clear();
    }

    private int getLoadNumDays() {
        return ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
    }

    private void importFromUrl(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days, FilmListLoadOptions options) {
        runImportAsync(() -> importFromUrlSynchronously(listeFilme, listeFilmeDiff, days), "importFromUrl", options);
    }

    private void importFromFile(String pfad, ListeFilme listeFilme, int days, FilmListLoadOptions options) {
        runImportAsync(() -> urlLaden(pfad, listeFilme, days), "importFromFile", options);
    }

    private boolean importFromUrlSynchronously(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days) {
        if (listeFilme.isEmpty() || !listeFilme.getMetaData().canUseDiffList()) {
            return ladeKompletteListe(listeFilme, days);
        }

        if (ladeDiffListe(listeFilmeDiff, days)) {
            return true;
        }

        listeFilmeDiff.clear();
        return ladeKompletteListe(listeFilme, days);
    }

    private boolean ladeKompletteListe(ListeFilme listeFilme, int days) {
        listeFilme.clear();
        return urlLaden(StandardLocations.getFilmListUrl(FilmListDownloadType.FULL), listeFilme, days);
    }

    private boolean ladeDiffListe(ListeFilme listeFilmeDiff, int days) {
        return urlLaden(StandardLocations.getFilmListUrl(FilmListDownloadType.DIFF_ONLY), listeFilmeDiff, days)
                && !listeFilmeDiff.isEmpty();
    }

    private boolean urlLaden(String dateiUrl, ListeFilme listeFilme, int days) {
        boolean ret = false;
        try {
            if (!dateiUrl.isEmpty()) {
                logger.trace("Filmliste laden von: {}", dateiUrl);
                filmListReader.readFilmListe(dateiUrl, listeFilme, days);
                if (!listeFilme.isEmpty()) {
                    ret = true;
                }
            }
        } catch (Exception ex) {
            logger.error("urlLaden", ex);
        }
        return ret;
    }

    private void runImportAsync(Supplier<Boolean> importAction, String operationName, FilmListLoadOptions options) {
        CompletableFuture.supplyAsync(importAction)
                .exceptionally(throwable -> {
                    logger.error(operationName, throwable);
                    return false;
                })
                .thenAccept(ok -> {
                    logger.trace("Filme laden, ende");
                    undEnde(new ListenerFilmeLadenEvent("", "", 0, 0, !ok), options);
                });
    }

    private void undEnde(ListenerFilmeLadenEvent event, FilmListLoadOptions options) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.debug("undEnde()");
        final var listeFilme = daten.getListeFilme();
        final var readDate = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()));

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            logger.info("Liste Diff gelesen am: {}", readDate);
            logger.info("  Liste Diff erstellt am: {}", diffListe.getMetaData().getGenerationDateTimeAsString());
            logger.info("  Anzahl Filme: {}", diffListe.size());

            listeFilme.updateFromFilmList(diffListe);
            listeFilme.setMetaData(diffListe.getMetaData());
            Collections.sort(listeFilme);
            diffListe.clear();
        } else {
            logger.info("Liste Kompl. gelesen am: {}", readDate);
            logger.info("  Liste Kompl erstellt am: {}", listeFilme.getMetaData().getGenerationDateTimeAsString());
            logger.info("  Anzahl Filme: {}", listeFilme.size());
        }

        findAndMarkNewFilms(daten.getListeFilme());

        final boolean writeFilmList;
        final var ui = MediathekGui.ui();

        istAmLaufen = false;
        if (event.fehler) {
            logger.info("");
            logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen");
            if (ui != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()) {
                SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(MediathekGui.ui(),
                        "Das Laden der Filmliste hat nicht geklappt!",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE));
            }

            // dann die alte Liste wieder laden
            listeFilme.clear();

            try (FilmListReader reader = new FilmListReader()) {
                final int num_days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
                reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), listeFilme, num_days);
            }
            logger.info("");

            writeFilmList = false;
        } else {
            writeFilmList = options.writeAfterLoad();
        }

        logger.info("");
        logger.info("Jetzige Liste erstellt am: {}", listeFilme.getMetaData().getGenerationDateTimeAsString());
        logger.info("  Anzahl Filme: {}", listeFilme.size());
        logger.info("  Anzahl Neue:  {}", listeFilme.countNewFilms());
        logger.info("");

        MessageBus.getMessageBus().publishAsync(new FilmListReadStopEvent());
        final var statusBarWidgets = attachStatusBarWidgets(ui);
        startPostLoadWork(writeFilmList, statusBarWidgets, ui);
    }

    private void fillHash(ListeFilme listeFilme) {
        hashSet.addAll(listeFilme.parallelStream().map(DatenFilm::getUrlNormalQuality).toList());
    }

    /**
     * Search through history and mark new films.
     */
    private void findAndMarkNewFilms(@NonNull ListeFilme listeFilme) {
        //reset all current new films to false
        listeFilme.parallelStream()
                .filter(DatenFilm::isNew)
                .forEach(film -> film.setNew(false));
        // mark new entries
        listeFilme.parallelStream()
                .filter(film -> !hashSet.contains(film.getUrlNormalQuality()))
                .forEach(film -> film.setNew(true));

        hashSet.clear();
    }

    public void notifyStart(ListenerFilmeLadenEvent e) {
        try {
            notifyListenersAsync(listener -> listener.start(e));
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyProgress(ListenerFilmeLadenEvent e) {
        try {
            notifyListenersAsync(listener -> listener.progress(e));
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyFertig(ListenerFilmeLadenEvent e) {
        try {
            notifyListenersAsync(listener -> listener.fertig(e));

            if (!onlyOne) {
                onlyOne = true;
                notifyListenersAsync(listener -> listener.fertigOnlyOne(e));
            }
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    private StatusBarWidgets attachStatusBarWidgets(MediathekGui ui) {
        if (ui == null) {
            return new StatusBarWidgets(new JLabel(), new JProgressBar());
        }
        final var widgets = new StatusBarWidgets(ui.progressLabel, ui.progressBar);
        invokeOnEdtAndWait(() -> {
            ui.swingStatusBar.add(widgets.label());
            ui.swingStatusBar.add(widgets.progressBar());
        });
        return widgets;
    }

    private void detachStatusBarWidgets(MediathekGui ui, StatusBarWidgets widgets) {
        if (ui == null) {
            return;
        }
        invokeOnEdtAndWait(() -> {
            ui.swingStatusBar.remove(widgets.progressBar());
            ui.swingStatusBar.remove(widgets.label());
        });
    }

    private void startPostLoadWork(boolean writeFilmList, StatusBarWidgets widgets, MediathekGui ui) {
        buildPostLoadWorkerChain(writeFilmList, widgets)
                .thenRun(() -> SwingUtilities.invokeLater(() ->
                        Daten.getInstance().getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, false))))
                .thenRun(() -> detachStatusBarWidgets(ui, widgets));
    }

    private CompletableFuture<Void> buildPostLoadWorkerChain(boolean writeFilmList, StatusBarWidgets widgets) {
        var workerTask = CompletableFuture.runAsync(new RefreshAboWorker(widgets.label(), widgets.progressBar()))
                .thenRun(new BlacklistFilterWorker(widgets.label(), widgets.progressBar()));

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true)) {
            workerTask = workerTask.thenRun(new FilmDuplicateEvaluationTask());
        }

        workerTask = workerTask.thenRun(new CommonStatsEvaluationTask());

        if (writeFilmList) {
            workerTask = workerTask.thenRun(new FilmlistWriterWorker(widgets.label(), widgets.progressBar()));
        }
        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList) {
            workerTask = workerTask.thenRun(new LuceneIndexWorker(widgets.label(), widgets.progressBar()));
        }

        return workerTask;
    }

    private void invokeOnEdtAndWait(Runnable action) {
        try {
            SwingUtilities.invokeAndWait(action);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    private void notifyListenersAsync(ListenerAction action) {
        final var currentListeners = listeners.getListeners(ListenerFilmeLaden.class);
        final var runnable = (Runnable) () -> {
            for (var listener : currentListeners) {
                action.accept(listener);
            }
        };
        if (Config.isDownloadAndQuit() || GraphicsEnvironment.isHeadless()) {
            runnable.run();
        } else {
            SwingUtilities.invokeLater(runnable);
        }
    }

    @FunctionalInterface
    private interface ListenerAction {
        void accept(ListenerFilmeLaden listener);
    }
}
