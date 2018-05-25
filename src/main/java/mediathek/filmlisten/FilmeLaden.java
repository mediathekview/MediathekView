/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.filmlisten;

import mSearch.Config;
import mSearch.Const;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlistenSuchen;
import mSearch.filmlisten.ListeFilmlistenUrls;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.MVHttpClient;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.filmlisten.reader.FilmListReader;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.javafx.FXProgressPanel;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.awt.*;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.stream.Collectors;

public class FilmeLaden {

    private static final Logger logger = LogManager.getLogger(FilmeLaden.class);
    private static final String NETWORK_NOT_AVAILABLE = "Netzwerk nicht verfügbar";
    private static final String DIALOG_TITLE = "Filmliste laden";
    private static final String NO_UPDATE_AVAILABLE = "Keine aktuellere Liste verfügbar";
    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();
    private final Daten daten;
    private final ImportFilmliste importFilmliste;
    private final EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;
    private boolean onlyOne = false;

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
                logger.debug("Filme laden, ende");
                undEnde(event);
            }
        });
    }


    public void loadFilmlistDialog(Daten daten, boolean manuell) {
        if (manuell || GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(daten.getMediathekGui(), true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(daten, daten.getMediathekGui()));
            dialog.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            loadFilmlist("");
        }
    }

    public void loadFilmlist(String dateiUrl) {
        loadFilmlist(dateiUrl, false);
    }

    /**
     * Check if a newer filmlist id is available on the remote server in order to prevent unnecessary filmlist downloads...
     *
     * @return true if newer is availble, otherwise false.
     */
    private boolean hasNewRemoteFilmlist() {
        boolean result = false;

        final String id = Daten.getInstance().getListeFilme().getId();

        boolean showDialogs = true;
        //we might be CLI...this is used in -auto as well
        if (GraphicsEnvironment.isHeadless() || GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO)
            showDialogs = false;

        final Request request = new Request.Builder().url("https://verteiler1.mediathekview.de/filmliste.id").get().build();
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (body != null && response.isSuccessful()) {
                String remoteId = body.string();
                if (!remoteId.isEmpty() && !remoteId.equalsIgnoreCase(id))
                    result = true; // we have an update...
            } else {
                //we were not successful to load the id file
                //if not found, pretend we need to update
                if (response.code() == HttpURLConnection.HTTP_NOT_FOUND)
                    result = true;
            }

            if (!result) {
                if (showDialogs)
                    SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(null,
                            NO_UPDATE_AVAILABLE, DIALOG_TITLE, JOptionPane.INFORMATION_MESSAGE));
                else
                    logger.info(NO_UPDATE_AVAILABLE);
            }
        } catch (UnknownHostException ex) {
            logger.debug(ex);
            if (showDialogs)
                SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(null,
                        NETWORK_NOT_AVAILABLE, DIALOG_TITLE, JOptionPane.ERROR_MESSAGE));
            else
                logger.warn(NETWORK_NOT_AVAILABLE);

        } catch (IOException ex) {
            logger.error("IOxception:", ex);
        } catch (Exception ex) {
            logger.error("check for filmliste.id failed", ex);
        }

        return result;
    }

    /**
     * Determin whether we want to perform a remote update check.
     * This will be done if we are:
     * 1. dont have film entries
     * 2. dateiUrl is either empty or string starts with http
     * 3. our filmlist is old enough that we dont use diff list - we dont check them.
     *
     * @return true if we need to load a new list, false if we should not load a remote list
     */
    private boolean performUpdateCheck(ListeFilme listeFilme, String dateiUrl) {
        boolean result = true;

        //always perform update when list is empty
        if (!listeFilme.isEmpty()) {
            //remote download is using an empty file name!...
            //or somebody put a web adress into the text field
            if (dateiUrl.isEmpty() || dateiUrl.startsWith("http")) {
                //perform check only if we dont want to use DIFF list...
                if (listeFilme.isTooOldForDiff()) {
                    if (!hasNewRemoteFilmlist())
                        result = false;
                }
            }
        }

        return result;
    }

    public void loadFilmlist(String dateiUrl, boolean immerNeuLaden) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert

        ListeFilme listeFilme = daten.getListeFilme();

        if (!performUpdateCheck(listeFilme, dateiUrl))
            return;

        logger.debug("Filme laden, start");
        logger.info("");
        logger.info("Alte Liste erstellt am: {}", listeFilme.genDate());
        logger.info("  Anzahl Filme: {}", listeFilme.size());
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(listeFilme);
            if (immerNeuLaden) {
                // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                listeFilme.clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
            }
            daten.getListeFilmeNachBlackList().clear();
            if (dateiUrl.isEmpty()) {
                // Filme als Liste importieren, Url automatisch ermitteln
                logger.info("Filmliste laden (auto)");
                importFilmliste.importFromUrl(listeFilme, diffListe, Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            } else {
                // Filme als Liste importieren, feste URL/Datei
                logger.info("Filmliste laden von: {}", dateiUrl);
                listeFilme.clear();
                importFilmliste.importFromFile(dateiUrl, listeFilme, Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            }
        }
    }

    public void updateFilmlist(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start");
        logger.info("");
        logger.info("Alte Liste erstellt am: {}", daten.getListeFilme().genDate());
        logger.info("  Anzahl Filme: {}", daten.getListeFilme().size());
        logger.info("  Anzahl Neue: {}", daten.getListeFilme().countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(daten.getListeFilme());
            //daten.getListeFilme().clear();
            daten.getListeFilmeNachBlackList().clear();
            // Filme als Liste importieren, feste URL/Datei
            logger.info("Filmliste laden von: " + dateiUrl);
            importFilmliste.importFromFile(dateiUrl, diffListe, Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
        }
    }

    // #######################################
    // #######################################
    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    public synchronized void setStop(boolean set) {
        Config.setStop(set);
    }

    public String[] getSenderNamen() {
        return Const.SENDER;
        //return FilmeSuchen.getNamenSender();
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_akt() {
        return importFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_akt;
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_diff() {
        return importFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_diff;
    }

    public FilmlistenSuchen getFilmlistenSuchen() {
        return importFilmliste.msFilmlistenSuchen;
    }

    public String getDownloadUrl_akt() {
        return importFilmliste.msFilmlistenSuchen.suchenAkt(new ArrayList<>());
    }

    // #######################################
    // #######################################
    private void undEnde(ListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.info("");

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            logger.info("Liste Diff gelesen am: {}", new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            logger.info("  Liste Diff erstellt am: {}", diffListe.genDate());
            logger.info("  Anzahl Filme: {}", diffListe.size());

            final ListeFilme listeFilme = daten.getListeFilme();
            listeFilme.updateListe(diffListe, true/* Vergleich über Index, sonst nur URL */, true /*ersetzen*/);
            listeFilme.setMetaDaten(diffListe.metaDaten);
            Collections.sort(listeFilme);
            diffListe.clear();
        } else {
            logger.info("Liste Kompl. gelesen am: {}", new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            logger.info("  Liste Kompl erstellt am: {}", daten.getListeFilme().genDate());
            logger.info("  Anzahl Filme: {}", daten.getListeFilme().size());
        }

        findAndMarkNewFilms(daten.getListeFilme());

        istAmLaufen = false;
        if (event.fehler) {
            logger.info("");
            logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen");
            MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            // dann die alte Liste wieder laden
            daten.getListeFilme().clear();
            Config.setStop(false);
            try (FilmListReader reader = new FilmListReader()) {
                reader.readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            }
            logger.info("");
        } else {
            daten.filmlisteSpeichern();
        }
        logger.info("");
        logger.info("Jetzige Liste erstellt am: {}", daten.getListeFilme().genDate());
        logger.info("  Anzahl Filme: {}", daten.getListeFilme().size());
        logger.info("  Anzahl Neue:  {}", daten.getListeFilme().countNewFilms());
        logger.info("");

        //update filmlist size for startup Progress panel...
        ApplicationConfiguration.getConfiguration().setProperty(FXProgressPanel.CONFIG_STRING, daten.getListeFilme().size());

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Themen suchen", 0, 0, 0, false/*Fehler*/));
        daten.getListeFilme().themenLaden();

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Abos eintragen", 0, 0, 0, false/*Fehler*/));
        daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false/*aboLoeschen*/);

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Blacklist filtern", 0, 0, 0, false/*Fehler*/));
        daten.getListeBlacklist().filterListe();

        notifyFertig(event);
    }

    private void fillHash(ListeFilme listeFilme) {
        hashSet.addAll(listeFilme.parallelStream().map(DatenFilm::getUrlHistory).collect(Collectors.toList()));
    }

    /**
     * Search through history and mark new films.
     */
    private void findAndMarkNewFilms(ListeFilme listeFilme) {
        listeFilme.neueFilme = false;

        listeFilme.parallelStream().peek(film -> film.setNew(false)).filter(film -> !hashSet.contains(film.getUrlHistory()))
                .forEach(film
                        -> {
                    film.setNew(true);
                    listeFilme.neueFilme = true;
                });

        hashSet.clear();
    }

    public void notifyStart(ListenerFilmeLadenEvent event) {
        final ListenerFilmeLadenEvent e = event;
        try {
            SwingUtilities.invokeLater(()
                    -> {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.start(e);
                }
            });
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyProgress(ListenerFilmeLadenEvent event) {
        final ListenerFilmeLadenEvent e = event;
        try {
            SwingUtilities.invokeLater(()
                    -> {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.progress(e);
                }
            });
        } catch (Exception ex) {
            logger.error(ex);
        }
    }

    public void notifyFertig(ListenerFilmeLadenEvent event) {
        final ListenerFilmeLadenEvent e = event;
        try {
            SwingUtilities.invokeLater(()
                    -> {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.fertig(e);
                }
            });
        } catch (Exception ex) {
            logger.error(ex);
        }
        try {
            if (!onlyOne) {
                onlyOne = true;
                SwingUtilities.invokeLater(()
                        -> {
                    for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                        l.fertigOnlyOne(e);
                    }
                });
            }
        } catch (Exception ex) {
            logger.error(ex);
        }
    }
}
