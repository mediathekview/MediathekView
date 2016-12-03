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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;
import mSearch.Config;
import mSearch.Const;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.filmlisten.ListeFilmlistenUrls;
import mSearch.tool.Duration;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;

public class FilmeLaden {

    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();

    private enum ListenerMelden {

        START, PROGRESS, FINISHED
    }

    // private
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
                Duration.staticPing("Filme laden, ende");
                undEnde(event);
            }
        });
    }

    //    // #########################################################
    //    // Filmliste beim Programmstart!! laden
    //    // #########################################################
    //    public void loadFilmlistProgStart() {
    //        // Gui startet ein wenig flüssiger
    //        new Thread(new loadFilmlistProgStart_()).start();
    //    }
    //    private class loadFilmlistProgStart_ implements Runnable {
    //
    //        @Override
    //        public synchronized void run() {
    //            Duration.staticPing("Thread: Filmliste laden");
    //            new FilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
    //
    //            SysMsg.sysMsg("Liste Filme gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
    //            SysMsg.sysMsg("  erstellt am: " + daten.getListeFilme().genDate());
    //            SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
    //            SysMsg.sysMsg("  Anzahl Neue: " + daten.getListeFilme().countNewFilms());
    //
    //            daten.getListeFilme().themenLaden();
    //            Daten.listeAbo.setAboFuerFilm(daten.getListeFilme(), false /*aboLoeschen*/);
    //            daten.getListeDownloads().filmEintragen(); // Filme bei einmalDownloads eintragen
    //            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)); // Zustand Blacklist beim Start setzen
    //
    //            if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && daten.getListeFilme().isTooOld()) {
    //                SysMsg.sysMsg("Neue Filmliste laden");
    //                loadFilmlist("", true);
    //            } else {
    //                // entweder neue Liste laden oder es ist schon fertig, dann melden
    //                daten.getListeBlacklist().filterListe(); // beim Neuladen wird es dann erst gemacht
    //                notifyFertig(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));
    //            }
    //        }
    //
    //    }
    // #########################################################
    // Filmliste importieren
    // #########################################################
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

    public void loadFilmlist(String dateiUrl, boolean immerNeuLaden) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        Duration.staticPing("Filme laden, start");
        SysMsg.sysMsg("");
        SysMsg.sysMsg("Alte Liste erstellt am: " + Daten.getInstance().getListeFilme().genDate());
        SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
        SysMsg.sysMsg("  Anzahl Neue: " + daten.getListeFilme().countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(daten.getListeFilme());
            if (immerNeuLaden) {
                // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                daten.getListeFilme().clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
            }
            daten.getListeFilmeNachBlackList().clear();
            if (dateiUrl.equals("")) {
                // Filme als Liste importieren, Url automatisch ermitteln
                SysMsg.sysMsg("Filmliste laden (auto)");
                importFilmliste.filmeImportierenAuto(daten.getListeFilme(), diffListe, Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            } else {
                // Filme als Liste importieren, feste URL/Datei
                SysMsg.sysMsg("Filmliste laden von: " + dateiUrl);
                daten.getListeFilme().clear();
                importFilmliste.filmeImportierenDatei(dateiUrl, daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            }
        }
    }

    public void updateFilmlist(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        Duration.staticPing("Filme laden (Update), start");
        SysMsg.sysMsg("");
        SysMsg.sysMsg("Alte Liste erstellt am: " + daten.getListeFilme().genDate());
        SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
        SysMsg.sysMsg("  Anzahl Neue: " + daten.getListeFilme().countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(daten.getListeFilme());
            //daten.getListeFilme().clear();
            daten.getListeFilmeNachBlackList().clear();
            // Filme als Liste importieren, feste URL/Datei
            SysMsg.sysMsg("Filmliste laden von: " + dateiUrl);
            importFilmliste.filmeImportierenDatei(dateiUrl, diffListe, Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
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

    public void updateDownloadUrlsFilmlisten(boolean akt) {
        importFilmliste.updateDownloadUrlsFilmlisten(akt);
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_akt() {
        return importFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_akt;
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_diff() {
        return importFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_diff;
    }

    public String getDownloadUrl_akt() {
        return importFilmliste.msFilmlistenSuchen.suchenAkt(new ArrayList<>());
    }

    // #######################################
    // #######################################
    private void undEnde(ListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        SysMsg.sysMsg("");

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            SysMsg.sysMsg("Liste Diff gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            SysMsg.sysMsg("  Liste Diff erstellt am: " + diffListe.genDate());
            SysMsg.sysMsg("  Anzahl Filme: " + diffListe.size());

            daten.getListeFilme().updateListe(diffListe, true/* Vergleich über Index, sonst nur URL */, true /*ersetzen*/);
            daten.getListeFilme().metaDaten = diffListe.metaDaten;
            daten.getListeFilme().sort(); // jetzt sollte alles passen
            diffListe.clear();
        } else {
            SysMsg.sysMsg("Liste Kompl. gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            SysMsg.sysMsg("  Liste Kompl erstellt am: " + daten.getListeFilme().genDate());
            SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
        }

        findAndMarkNewFilms(daten.getListeFilme());

        istAmLaufen = false;
        if (event.fehler) {
            SysMsg.sysMsg("");
            SysMsg.sysMsg("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen");
            MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            // dann die alte Liste wieder laden
            daten.getListeFilme().clear();
            Config.setStop(false);
            new FilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            SysMsg.sysMsg("");
        } else {
            daten.filmlisteSpeichern();
        }
        SysMsg.sysMsg("");
        SysMsg.sysMsg("Jetzige Liste erstellt am: " + daten.getListeFilme().genDate());
        SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
        SysMsg.sysMsg("  Anzahl Neue:  " + daten.getListeFilme().countNewFilms());
        SysMsg.sysMsg("");

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Themen suchen", 0, 0, 0, false/*Fehler*/));
        daten.getListeFilme().themenLaden();

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Abos eintragen", 0, 0, 0, false/*Fehler*/));
        daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false/*aboLoeschen*/);

        daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Blacklist filtern", 0, 0, 0, false/*Fehler*/));
        daten.getListeBlacklist().filterListe();

        notifyFertig(event);
    }

    private void fillHash(ListeFilme listeFilme) {
        hashSet.addAll(listeFilme.stream().map(DatenFilm::getUrlHistory).collect(Collectors.toList()));
    }

    /**
     * Search through history and mark new films.
     *
     * @param listeFilme
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
            Log.errorLog(765213654, ex);
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
            Log.errorLog(201020369, ex);
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
            Log.errorLog(945120303, ex);
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
            Log.errorLog(912045120, ex);
        }
    }
}
