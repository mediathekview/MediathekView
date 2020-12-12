package mediathek.filmlisten;

import com.google.common.base.Stopwatch;
import javafx.application.Platform;
import javafx.scene.control.Alert;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.reader.FilmListReader;
import mediathek.gui.actions.FilmListWriteWorkerTask;
import mediathek.javafx.FilmListFilterTask;
import mediathek.javafx.tool.FXProgressPane;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.FilmListUpdateType;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVHttpClient;
import mediathek.tool.javafx.FXErrorDialog;
import okhttp3.HttpUrl;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.io.IOException;
import java.net.UnknownHostException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class FilmeLaden {

    private static final Logger logger = LogManager.getLogger(FilmeLaden.class);
    private static final String NETWORK_NOT_AVAILABLE = "Netzwerk nicht verfügbar";
    private static final String DIALOG_TITLE = "Filmliste laden";
    private static final String NO_UPDATE_AVAILABLE = "Keine aktuellere Liste verfügbar";
    /**
     * HTTP error code for not found.
     */
    private static final int HTTP_NOT_FOUND = 404;
    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();
    private final Daten daten;
    private final ImportFilmliste importFilmliste;
    private final EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen;
    private boolean onlyOne;

    public FilmeLaden(Daten aDaten) {
        daten = aDaten;
        importFilmliste = new ImportFilmliste();
        importFilmliste.addAdListener(new ListenerFilmeLaden() {
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
                // Ergebnisliste listeFilme eintragen -> Feierabend!
                logger.trace("Filme laden, ende");
                undEnde(event);
            }
        });
    }

    private void showNoUpdateAvailableDialog() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText(DIALOG_TITLE);
            alert.setContentText(NO_UPDATE_AVAILABLE);
            alert.showAndWait();
        });
    }

    /**
     * Check if a newer filmlist id is available on the remote server in order to prevent unnecessary filmlist downloads...
     *
     * @return true if newer is availble, otherwise false.
     */
    private boolean hasNewRemoteFilmlist() {
        boolean result = false;
        logger.trace("hasNewRemoteFilmList()");

        final String id = Daten.getInstance().getListeFilme().metaData().getId();

        boolean showDialogs = true;

        if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC)
            showDialogs = false;

        HttpUrl filmListUrl = Konstanten.ROUTER_BASE_URL.resolve("filmliste.id");
        final Request request = new Request.Builder().url(Objects.requireNonNull(filmListUrl)).build();
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (body != null && response.isSuccessful()) {
                String remoteId = body.string();
                if (!remoteId.isEmpty() && !remoteId.equalsIgnoreCase(id))
                    result = true; // we have an update...
            } else {
                //we were not successful to load the id file
                //if not found, pretend we need to update
                logger.warn("hasNewRemoteFilmlist HTTP Response Code: {} for {}", response.code(), response.request().url());
                if (response.code() == HTTP_NOT_FOUND)
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
            if (showDialogs)
                Platform.runLater(() ->
                        FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, DIALOG_TITLE, NETWORK_NOT_AVAILABLE, ex));
            else
                logger.warn(NETWORK_NOT_AVAILABLE);

        } catch (IOException ex) {
            logger.error("IOxception:", ex);
            Platform.runLater(() ->
                    FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, DIALOG_TITLE,
                            "Netzwerkfehler aufgetreten!", ex));
        } catch (Exception ex) {
            logger.error("check for filmliste.id failed", ex);
            if (showDialogs)
                Platform.runLater(() ->
                        FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, DIALOG_TITLE,
                                "Ein unbekannter Fehler ist aufgetreten.", ex));
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
        boolean result = true;

        //always perform update when list is empty
        if (listeFilme.isEmpty()) {
            return true;
        } else {
            //remote download is using an empty file name!...
            //or somebody put a web adress into the text field
            if (dateiUrl.isEmpty() || dateiUrl.startsWith("http")) {
                //perform check only if we don´t want to use DIFF list...
                if (!listeFilme.metaData().canUseDiffList()) {
                    if (!hasNewRemoteFilmlist())
                        result = false;
                }
            }

        }

        return result;
    }

    private void prepareHashTable() {
        hashSet.clear();
        fillHash(daten.getListeFilme());
    }

    public boolean loadFilmlist(String dateiUrl, boolean immerNeuLaden) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        ListeFilme listeFilme = daten.getListeFilme();

        if (!performUpdateCheck(listeFilme, dateiUrl))
            return false;

        logger.trace("loadFilmlist(String,boolean)");
        logger.info("");
        logger.info("Alte Liste erstellt am: {}", listeFilme.metaData().getGenerationDateTimeAsString());
        logger.info("  Anzahl Filme: {}", listeFilme.size());
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;

            // Hash mit URLs füllen
            prepareHashTable();

            if (immerNeuLaden) {
                // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                listeFilme.clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
            }

            daten.getListeFilmeNachBlackList().clear();

            final int days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
            if (dateiUrl.isEmpty()) {
                // Filme als Liste importieren, Url automatisch ermitteln
                logger.info("Filmliste laden (Netzwerk)");
                importFilmliste.importFromUrl(listeFilme, diffListe, days);
            } else {
                // Filme als Liste importieren, feste URL/Datei
                logger.info("Filmliste laden von: {}", dateiUrl);
                listeFilme.clear();
                importFilmliste.importFromFile(dateiUrl, listeFilme, days);
            }
        }
        return true;
    }

    public void updateFilmlist(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start");
        logger.info("");
        logger.info("Alte Liste erstellt am: {}", daten.getListeFilme().metaData().getGenerationDateTimeAsString());
        logger.info("  Anzahl Filme: {}", daten.getListeFilme().size());
        logger.info("  Anzahl Neue: {}", daten.getListeFilme().countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;

            // Hash mit URLs füllen
            prepareHashTable();

            daten.getListeFilmeNachBlackList().clear();
            // Filme als Liste importieren, feste URL/Datei
            logger.info("Filmliste laden von: " + dateiUrl);
            final int num_days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
            if (dateiUrl.isEmpty()) {
                dateiUrl = GuiFunktionen.getFilmListUrl(FilmListDownloadType.FULL);
            }
            importFilmliste.importFromFile(dateiUrl, diffListe, num_days);
        }
    }

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    private void undEnde(ListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.debug("undEnde()");
        final var listeFilme = daten.getListeFilme();
        final var readDate = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()));

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            logger.info("Liste Diff gelesen am: {}", readDate);
            logger.info("  Liste Diff erstellt am: {}", diffListe.metaData().getGenerationDateTimeAsString());
            logger.info("  Anzahl Filme: {}", diffListe.size());

            listeFilme.updateFromFilmList(diffListe);
            listeFilme.setMetaData(diffListe.metaData());
            Collections.sort(listeFilme);
            diffListe.clear();
        } else {
            logger.info("Liste Kompl. gelesen am: {}", readDate);
            logger.info("  Liste Kompl erstellt am: {}", listeFilme.metaData().getGenerationDateTimeAsString());
            logger.info("  Anzahl Filme: {}", listeFilme.size());
        }

        findAndMarkNewFilms(daten.getListeFilme());

        final boolean writeFilmList;
        final var ui = MediathekGui.ui();

        istAmLaufen = false;
        if (event.fehler) {
            logger.info("");
            logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen");
            Platform.runLater(() -> {
                Alert alert = new Alert(Alert.AlertType.ERROR);
                alert.setHeaderText("Fehler");
                alert.setContentText("Das Laden der Filmliste hat nicht geklappt!");
                JFXHiddenApplication.showAlert(alert, ui);
            });

            // dann die alte Liste wieder laden
            listeFilme.clear();

            try (FilmListReader reader = new FilmListReader()) {
                final int num_days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
                reader.readFilmListe(Daten.getDateiFilmliste(), listeFilme, num_days);
            }
            logger.info("");

            writeFilmList = false;
        } else {
            writeFilmList = !Daten.dontWriteFilmlistOnStartup.get();
        }

        logger.info("");
        logger.info("Jetzige Liste erstellt am: {}", listeFilme.metaData().getGenerationDateTimeAsString());
        logger.info("  Anzahl Filme: {}", listeFilme.size());
        logger.info("  Anzahl Neue:  {}", listeFilme.countNewFilms());
        logger.info("");

        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            FXProgressPane hb = new FXProgressPane();

            FilmListFilterTask task = new FilmListFilterTask(true);
            task.setOnRunning(e -> {
                ui.getStatusBarController().getStatusBar().getRightItems().add(hb);
                hb.lb.textProperty().bind(task.messageProperty());
                hb.prog.progressProperty().bind(task.progressProperty());
            });

            CompletableFuture<Void> workerTask = CompletableFuture.runAsync(task);
            if (writeFilmList) {
                FilmListWriteWorkerTask writerTask = new FilmListWriteWorkerTask(Daten.getInstance());
                writerTask.setOnRunning(e -> {
                    hb.lb.textProperty().bind(writerTask.messageProperty());
                    hb.prog.progressProperty().bind(writerTask.progressProperty());
                });
                workerTask = workerTask.thenRun(writerTask);
            }

            workerTask.thenRun(() -> JavaFxUtils.invokeInFxThreadAndWait(() -> {
                ui.getStatusBarController().getStatusBar().getRightItems().remove(hb);
            }));
        });
    }

    private void fillHash(ListeFilme listeFilme) {
        hashSet.addAll(listeFilme.parallelStream().map(DatenFilm::getUrl).collect(Collectors.toList()));
    }

    /**
     * Search through history and mark new films.
     */
    private void findAndMarkNewFilms(ListeFilme listeFilme) {
        listeFilme.neueFilme = false;

        Stopwatch stopwatch = Stopwatch.createStarted();
        listeFilme.parallelStream()
                .peek(film -> film.setNew(false))
                .filter(film -> !hashSet.contains(film.getUrl()))
                .forEach(film -> {
                    film.setNew(true);
                    listeFilme.neueFilme = true;
                });
        stopwatch.stop();
        logger.debug("findAndMarkNewFilms() took: {}", stopwatch);

        hashSet.clear();
    }

    public void notifyStart(ListenerFilmeLadenEvent e) {
        try {
            SwingUtilities.invokeLater(() -> {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.start(e);
                }
            });
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyProgress(ListenerFilmeLadenEvent e) {
        try {
            SwingUtilities.invokeLater(() -> {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.progress(e);
                }
            });
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyFertig(ListenerFilmeLadenEvent e) {
        final var listListeners = listeners.getListeners(ListenerFilmeLaden.class);

        try {
            SwingUtilities.invokeLater(() -> {
                for (ListenerFilmeLaden lst : listListeners) {
                    lst.fertig(e);
                }
            });

            if (!onlyOne) {
                onlyOne = true;
                SwingUtilities.invokeLater(() -> {
                    for (ListenerFilmeLaden lst : listListeners) {
                        lst.fertigOnlyOne(e);
                    }
                });
            }
        } catch (Exception ex) {
            logger.error(ex);
        }
    }
}
