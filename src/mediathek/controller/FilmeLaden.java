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
package mediathek.controller;

import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.tool.*;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;
import msearch.filmeSuchen.MSFilmeSuchen;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;
import msearch.filmlisten.ListeFilmlistenUrls;
import msearch.filmlisten.MSFilmlisteLesen;
import msearch.filmlisten.MSImportFilmliste;
import msearch.tool.MSConfig;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.stream.Collectors;

public class FilmeLaden {

    private Duration duration = new Duration(FilmeLaden.class.getSimpleName());
    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();

    private enum ListenerMelden {

        START, PROGRESS, FINISHED
    }
    // private
    private final MSImportFilmliste msImportFilmliste;
    private final EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;

    public FilmeLaden() {
        msImportFilmliste = new MSImportFilmliste();
        msImportFilmliste.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public synchronized void start(MSListenerFilmeLadenEvent event) {
                notifyStart(event);
            }

            @Override
            public synchronized void progress(MSListenerFilmeLadenEvent event) {
                notifyProgress(event);
            }

            @Override
            public synchronized void fertig(MSListenerFilmeLadenEvent event) {
                // Ergebnisliste listeFilme eintragen -> Feierabend!
                duration.stop("Filme laden, ende");
                undEnde(event);
            }
        });
    }

    // #########################################################
    // Filmliste importieren
    // #########################################################
    public void filmeLaden(Daten daten, boolean manuell) {
        if (manuell || GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(daten, daten.mediathekGui));
            dialog.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            importFilmliste("");
        }
    }

    public void importFilmliste(String dateiUrl) {
        importFilmliste(dateiUrl, false);
    }

    public void importFilmliste(String dateiUrl, boolean immerNeuLaden) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        duration.start("Filme laden, start");
        Log.systemMeldung("");
        Log.systemMeldung("Alte Liste erstellt am: " + Daten.listeFilme.genDate());
        Log.systemMeldung("  Anzahl Filme: " + Daten.listeFilme.size());
        Log.systemMeldung("  Anzahl Neue: " + Daten.listeFilme.countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(Daten.listeFilme);
            if (immerNeuLaden) {
                // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                Daten.listeFilme.clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
            }
            Daten.listeFilmeNachBlackList.clear();
            if (dateiUrl.equals("")) {
                // Filme als Liste importieren, Url automatisch ermitteln
                Log.systemMeldung("Filmliste laden (auto)");
                msImportFilmliste.filmeImportierenAuto(Daten.listeFilme, diffListe, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            } else {
                // Filme als Liste importieren, feste URL/Datei
                Log.systemMeldung("Filmliste laden von: " + dateiUrl);
                Daten.listeFilme.clear();
                msImportFilmliste.filmeImportierenDatei(dateiUrl, Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            }
        }
    }

    public void updateFilmliste(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        duration.start("Filme laden (Update), start");
        Log.systemMeldung("");
        Log.systemMeldung("Alte Liste erstellt am: " + Daten.listeFilme.genDate());
        Log.systemMeldung("  Anzahl Filme: " + Daten.listeFilme.size());
        Log.systemMeldung("  Anzahl Neue: " + Daten.listeFilme.countNewFilms());
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(Daten.listeFilme);
            //Daten.listeFilme.clear();
            Daten.listeFilmeNachBlackList.clear();
            // Filme als Liste importieren, feste URL/Datei
            Log.systemMeldung("Filmliste laden von: " + dateiUrl);
            msImportFilmliste.filmeImportierenDatei(dateiUrl, diffListe, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
        }
    }

    // #######################################
    // #######################################
    public void addAdListener(MSListenerFilmeLaden listener) {
        listeners.add(MSListenerFilmeLaden.class, listener);
    }

    public synchronized void setStop(boolean set) {
        MSConfig.setStop(set);
    }

    public String[] getSenderNamen() {
        return MSFilmeSuchen.getNamenSender();
    }

    public void updateDownloadUrlsFilmlisten(boolean akt) {
        msImportFilmliste.updateDownloadUrlsFilmlisten(akt);
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_akt() {
        return msImportFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_akt;
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_diff() {
        return msImportFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_diff;
    }

    public String getDownloadUrl_akt() {
        return msImportFilmliste.msFilmlistenSuchen.suchenAkt(new ArrayList<>());
    }

    // #######################################
    // #######################################
    private void undEnde(MSListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        Log.systemMeldung("");

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            Log.systemMeldung("Liste Diff gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            Log.systemMeldung("  Liste Diff erstellt am: " + diffListe.genDate());
            Log.systemMeldung("  Anzahl Filme: " + diffListe.size());

            Daten.listeFilme.updateListe(diffListe, true/* Vergleich über Index, sonst nur URL */, true /*ersetzen*/);
            Daten.listeFilme.metaDaten = diffListe.metaDaten;
            Daten.listeFilme.sort(); // jetzt sollte alles passen
            diffListe.clear();
        } else {
            Log.systemMeldung("Liste Kompl. gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            Log.systemMeldung("  Liste Kompl erstellt am: " + Daten.listeFilme.genDate());
            Log.systemMeldung("  Anzahl Filme: " + Daten.listeFilme.size());
        }

        findAndMarkNewFilms(Daten.listeFilme);
        Daten.listeFilme.themenLaden();
        Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false/*aboLoeschen*/);
        istAmLaufen = false;
        if (event.fehler) {
            Log.systemMeldung("");
            Log.systemMeldung("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen");
            MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            // dann die alte Liste wieder laden
            Daten.listeFilme.clear();
            MSConfig.setStop(false);
            new MSFilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            Log.systemMeldung("");
        } else {
            Daten.filmlisteSpeichern();
        }
        Log.systemMeldung("");

        Log.systemMeldung("Jetzige Liste erstellt am: " + Daten.listeFilme.genDate());
        Log.systemMeldung("  Anzahl Filme: " + Daten.listeFilme.size());
        Log.systemMeldung("  Anzahl Neue:  " + Daten.listeFilme.countNewFilms());
        Log.systemMeldung("");

        MVListeFilme.checkBlacklist();
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
                .forEach(film -> {
                    film.setNew(true);
                    listeFilme.neueFilme = true;
                });

        hashSet.clear();
    }

    private void notifyStart(MSListenerFilmeLadenEvent event) {
        final MSListenerFilmeLadenEvent e = event;
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                    l.start(event);
                }
            } else {
                SwingUtilities.invokeLater(() -> {
                    for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                        l.start(e);
                    }
                });
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(765213654, ex);
        }
    }

    private void notifyProgress(MSListenerFilmeLadenEvent event) {
        final MSListenerFilmeLadenEvent e = event;
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                    l.progress(e);
                }
            } else {
                SwingUtilities.invokeLater(() -> {
                    for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                        l.progress(e);
                    }
                });
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(201020369, ex);
        }
    }

    private void notifyFertig(MSListenerFilmeLadenEvent event) {
        final MSListenerFilmeLadenEvent e = event;
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                    l.fertig(e);
                }
            } else {
                SwingUtilities.invokeLater(() -> {
                    for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
                        l.fertig(e);
                    }
                });
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(945120303, ex);
        }
    }
}
