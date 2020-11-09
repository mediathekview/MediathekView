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
package mediathek.daten;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.messages.ButtonStartEvent;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.gui.messages.DownloadQueueRankChangedEvent;
import mediathek.gui.messages.StartEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.models.TModelDownload;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("serial")
public class ListeDownloads extends LinkedList<DatenDownload> {
    private final Daten daten;

    public ListeDownloads(Daten daten_) {
        this.daten = daten_;
    }

    public synchronized void addMitNummer(DatenDownload e) {
        add(e);
        listeNummerieren();
    }

    private static final Logger logger = LogManager.getLogger(ListeDownloads.class);

    public synchronized void filmEintragen() {
        // bei einmal Downloads nach einem Programmstart/Neuladen der Filmliste
        // den Film wieder eintragen
        logger.info("Filme in Downloads eintragen");
        var listeFilme = daten.getListeFilme();
        this.stream().filter(d -> d.film == null)
                .forEach(d ->
                {
                    d.film = listeFilme.getFilmByUrl_klein_hoch_hd(d.arr[DatenDownload.DOWNLOAD_URL]);
                    d.setGroesse("");
                });
    }

    public synchronized void listePutzen() {
        // fertige Downloads löschen
        // fehlerhafte zurücksetzen
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.start == null) {
                continue;
            }
            if (d.start.status == Start.STATUS_FERTIG) {
                // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                it.remove();
                gefunden = true;
            } else if (d.start.status == Start.STATUS_ERR) {
                // fehlerhafte werden zurückgesetzt
                d.resetDownload();
                gefunden = true;
            }
        }
        if (gefunden) {
            daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
        }
    }

    public synchronized void listePutzen(DatenDownload datenDownload) {
        // fertigen Download löschen
        boolean gefunden = false;
        if (datenDownload.start != null) {
            if (datenDownload.start.status == Start.STATUS_FERTIG) {
                // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                remove(datenDownload);
                gefunden = true;
            } else if (datenDownload.start.status == Start.STATUS_ERR) {
                // fehlerhafte werden zurückgesetzt
                datenDownload.resetDownload();
                gefunden = true;
            }
        }
        if (gefunden) {
            daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
        }
    }

    public synchronized void abosAuffrischen() {
        // fehlerhafte und nicht gestartete löschen, wird nicht gemeldet ob was gefunden wurde
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.isInterrupted()) {
                // guter Rat teuer was da besser wäre??
                // wird auch nach dem Neuladen der Filmliste aufgerufen: also Finger weg
                d.setGroesseFromFilm();//bei den Abgebrochenen wird die tatsächliche Dateigröße angezeigt
                continue;
            }
            if (!d.isFromAbo()) {
                continue;
            }
            if (d.start == null) {
                // noch nicht gestartet
                it.remove();
            } else if (d.start.status == Start.STATUS_ERR) {
                // fehlerhafte
                d.resetDownload();
            }
        }

        this.forEach(d -> d.arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT] = Boolean.FALSE.toString());
    }

    /**
     * Get the number of unfinished download tasks.
     * @return number of unfinished tasks
     */
    public synchronized long unfinishedDownloads() {
        return stream().filter(DatenDownload::runNotFinished).count();
    }

    public synchronized void downloadsVorziehen(ArrayList<DatenDownload> download) {
        for (DatenDownload datenDownload : download) {
            this.remove(datenDownload);
            this.addFirst(datenDownload);
        }

        daten.getMessageBus().publishAsync(new DownloadQueueRankChangedEvent());
    }

    public synchronized void delDownloadButton(String url) {
        for (DatenDownload datenDownload : this) {
            if (datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(url)) {
                if (datenDownload.start != null) {
                    if (datenDownload.start.status < Start.STATUS_FERTIG) {
                        datenDownload.start.stoppen = true;
                    }
                }
                datenDownload.mVFilmSize.reset();
                datenDownload.start = null;
                daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
                break;
            }
        }
    }

    public synchronized void downloadAbbrechen(ArrayList<DatenDownload> download) {
        boolean gefunden = false;
        if (download != null) {
            for (DatenDownload down : download) {
                if (this.contains(down)) {
                    // nur dann ist er in der Liste
                    if (down.start != null) {
                        if (down.start.status < Start.STATUS_FERTIG) {
                            down.start.stoppen = true;
                        }
                        if (down.start.status == Start.STATUS_RUN) {
                            down.interrupt();
                        }
                    }
                    down.resetDownload();
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            daten.getMessageBus().publishAsync(new StartEvent());
        }
    }

    public synchronized void downloadLoeschen(ArrayList<DatenDownload> download) {
        boolean gefunden = false;
        if (download != null) {
            for (DatenDownload down : download) {
                if (down.start != null) {
                    if (down.start.status < Start.STATUS_FERTIG) {
                        down.start.stoppen = true;
                    }
                }
                if (remove(down)) {
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
        }
    }

    public synchronized DatenDownload getDownloadUrlFilm(String urlFilm) {
        for (DatenDownload datenDownload : this) {
            if (datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL].equals(urlFilm)) {
                return datenDownload;
            }
        }
        return null;
    }

    public synchronized void getModel(TModelDownload tModel, boolean onlyAbos, boolean onlyDownloads,
                                      boolean onlyNotStarted, boolean onlyStarted, boolean onlyWaiting, boolean onlyRun, boolean onlyFinished) {
        Object[] object;
        tModel.setRowCount(0);
        for (DatenDownload download : this) {
            if (download.istZurueckgestellt()) {
                continue;
            }

            final boolean istAbo = download.isFromAbo();
            if (onlyAbos && !istAbo) {
                continue;
            }
            if (onlyDownloads && istAbo) {
                continue;
            }

            final boolean notStarted = download.notStarted();
            if (onlyNotStarted && !notStarted) {
                continue;
            }
            if (onlyStarted && notStarted) {
                continue;
            }

            if (onlyWaiting && !download.isWaiting()) {
                continue;
            }

            if (onlyRun && !download.running()) {
                continue;
            }

            if (onlyFinished && !download.isFinished()) {
                continue;
            }

            object = new Object[DatenDownload.MAX_ELEM];
            for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
                if (i == DatenDownload.DOWNLOAD_NR) {
                    object[i] = download.nr;
                } else if (i == DatenDownload.DOWNLOAD_FILM_NR) {
                    if (download.film != null) {
                        object[i] = download.film.getFilmNr();
                    } else {
                        object[i] = 0;
                    }
                } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART
                        || i == DatenDownload.DOWNLOAD_UNTERBROCHEN
                        || i == DatenDownload.DOWNLOAD_SPOTLIGHT
                        || i == DatenDownload.DOWNLOAD_INFODATEI
                        || i == DatenDownload.DOWNLOAD_SUBTITLE
                        || i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT
                        || i == DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER) {
                    object[i] = "";
                } else if (i == DatenDownload.DOWNLOAD_DATUM) {
                    object[i] = download.datumFilm;
                } else if (i == DatenDownload.DOWNLOAD_RESTZEIT) {
                    object[i] = download.getTextRestzeit();
                } else if (i == DatenDownload.DOWNLOAD_BANDBREITE) {
                    object[i] = download.getTextBandbreite();
                } else if (i == DatenDownload.DOWNLOAD_PROGRESS) {
                    object[i] = setProgress(download);
                } else if (i == DatenDownload.DOWNLOAD_GROESSE) {
                    object[i] = download.mVFilmSize;
                } else if (i == DatenDownload.DOWNLOAD_REF) {
                    object[i] = download;
                } else if (i != DatenDownload.DOWNLOAD_URL && !DatenDownload.anzeigen(i)) {
                    // Filmnr und URL immer füllen, egal ob angezeigt
                    object[i] = "";
                } else {
                    object[i] = download.arr[i];
                }
            }
            tModel.addRow(object);
        }
    }

    private String setProgress(DatenDownload download) {
        if (download.start != null) {
            if (1 < download.start.percent && download.start.percent < Start.PROGRESS_FERTIG) {
                StringBuilder s = new StringBuilder(Double.toString(download.start.percent / 10.0) + '%');
                while (s.length() < 5) {
                    s.insert(0, '0');
                }
                return s.toString();
            } else {
                return Start.getTextProgress(download.isDownloadManager(), download.start);
            }
        } else {
            return "";
        }
    }

    public synchronized void setModelProgress(TModelDownload tModel) {
        int row = 0;

        for (var item : tModel.getDataVector()) {
            DatenDownload datenDownload = (DatenDownload) item.get(DatenDownload.DOWNLOAD_REF);
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    tModel.setValueAt(datenDownload.getTextRestzeit(), row, DatenDownload.DOWNLOAD_RESTZEIT);
                    tModel.setValueAt(datenDownload.getTextBandbreite(), row, DatenDownload.DOWNLOAD_BANDBREITE);
                    tModel.setValueAt(setProgress(datenDownload), row, DatenDownload.DOWNLOAD_PROGRESS);
                    tModel.setValueAt(datenDownload.mVFilmSize, row, DatenDownload.DOWNLOAD_GROESSE);
                }
            }
            ++row;
        }
    }

    public synchronized void abosSuchen(JFrame parent) {
        // in der Filmliste nach passenden Filmen suchen und 
        // in die Liste der Downloads eintragen
        final HashSet<String> listeUrls = new HashSet<>();
        // mit den bereits enthaltenen URL füllen
        this.forEach((download) -> listeUrls.add(download.arr[DatenDownload.DOWNLOAD_URL]));

        boolean gefunden = false;

        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        boolean checkWithBlackList = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO));
        DatenPset pSet_ = Daten.listePset.getPsetAbo("");
        final var sdf = new SimpleDateFormat("dd.MM.yyyy");
        final var todayDateStr = sdf.format(new Date());

        final var listeAbo = daten.getListeAbo();
        final var listeBlacklist = daten.getListeBlacklist();
        final var aboHistoryController = daten.getAboHistoryController();
        final var listeFilme = daten.getListeFilme();

        for (DatenFilm film : listeFilme) {
            DatenAbo abo = listeAbo.getAboFuerFilm_schnell(film, true);
            if (abo == null) {
                // dann gibts dafür kein Abo
                continue;
            }
            if (!abo.aboIstEingeschaltet()) {
                continue;
            }
            if (checkWithBlackList) {
                //Blacklist auch bei Abos anwenden
                if (!listeBlacklist.checkBlackOkFilme_Downloads(film)) {
                    continue;
                }
            }
            if (aboHistoryController.urlPruefen(film.getUrl())) {
                // ist schon mal geladen worden
                continue;
            }

            DatenPset pSet = abo.arr[DatenAbo.ABO_PSET].isEmpty() ? pSet_ : Daten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET]);
            if (pSet != null) {
                // mit der tatsächlichen URL prüfen, ob die URL schon in der Downloadliste ist
                String urlDownload = film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]));
                if (listeUrls.contains(urlDownload)) {
                    continue;
                }
                listeUrls.add(urlDownload);

                //diesen Film in die Downloadliste eintragen
                abo.arr[DatenAbo.ABO_DOWN_DATUM] = todayDateStr;
                if (!abo.arr[DatenAbo.ABO_PSET].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME])) {
                    // nur den Namen anpassen, falls geändert
                    abo.arr[DatenAbo.ABO_PSET] = pSet.arr[DatenPset.PROGRAMMSET_NAME];
                }

                //dann in die Liste schreiben
                add(new DatenDownload(pSet, film, DatenDownload.QUELLE_ABO, abo, "", "", "" /*Aufloesung*/));
                gefunden = true;
            } else {
                new DialogAboNoSet(parent).setVisible(true);
                break;
            }
        }

        if (gefunden) {
            listeNummerieren();
        }
        listeUrls.clear();
    }

    public synchronized void listeNummerieren() {
        int i = 1;
        for (DatenDownload datenDownload : this) {
            datenDownload.nr = i++;
        }
    }

    public synchronized DownloadStartInfo getStarts() {
        final DownloadStartInfo info = new DownloadStartInfo();

        for (DatenDownload download : this) {
            if (!download.istZurueckgestellt()) {
                info.total_starts++;
            }
            if (download.isFromAbo()) {
                info.num_abos++;
            } else {
                info.num_downloads++;
            }
            if (download.start != null) {
                if (download.quelle == DatenDownload.QUELLE_ABO || download.quelle == DatenDownload.QUELLE_DOWNLOAD) {
                    switch (download.start.status) {
                        case Start.STATUS_INIT -> info.initialized++;
                        case Start.STATUS_RUN -> info.running++;
                        case Start.STATUS_FERTIG -> info.finished++;
                        case Start.STATUS_ERR -> info.error++;
                    }
                }
            }
        }

        return info;
    }

    /**
     * Return a List of all not yet finished downloads.
     *
     * @param quelle Use QUELLE_XXX constants from {@link mediathek.controller.starter.Start}.
     * @return A list with all download objects.
     */
    public synchronized List<DatenDownload> getListOfStartsNotFinished(int quelle) {
        final List<DatenDownload> activeDownloads;
        activeDownloads = this.stream()
                .filter(download -> download.start != null)
                .filter(download -> download.start.status < Start.STATUS_FERTIG)
                .filter(download -> quelle == DatenDownload.QUELLE_ALLE || download.quelle == quelle)
                .collect(Collectors.toList());

        return activeDownloads;
    }

    public synchronized void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        boolean gefunden = false;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.start != null) {
                if (d.quelle == DatenDownload.QUELLE_BUTTON) {
                    if (d.start.status >= Start.STATUS_FERTIG) {
                        // dann ist er fertig oder abgebrochen
                        it.remove();
                        gefunden = true;
                    }
                }
            }
        }
        if (gefunden)
            daten.getMessageBus().publishAsync(new ButtonStartEvent());
    }

    public synchronized DatenDownload getNextStart() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        final DatenDownload[] ret = new DatenDownload[1];

        final int maxNumDownloads = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1);
        if (this.size() > 0 && getDown(maxNumDownloads)) {
            nextPossibleDownload().ifPresent(datenDownload -> {
                if (datenDownload.start != null) {
                    if (datenDownload.start.status == Start.STATUS_INIT)
                        ret[0] = datenDownload;
                }
            });
        }

        return ret[0];
    }

    public DatenDownload getRestartDownload() {
        // Versuch einen Fehlgeschlagenen Download zu finden um ihn wieder zu starten
        // die Fehler laufen aber einzeln, vorsichtshalber
        if (!getDown(1)) {
            return null;
        }
        for (DatenDownload datenDownload : this) {
            if (datenDownload.start == null) {
                continue;
            }

            if (datenDownload.start.status == Start.STATUS_ERR
                    && datenDownload.start.countRestarted < Konstanten.MAX_DOWNLOAD_RESTARTS
                    && !maxSenderLaufen(datenDownload, 1)) {
                int restarted = datenDownload.start.countRestarted;
                if ( /*datenDownload.art == DatenDownload.ART_PROGRAMM && datenDownload.isRestart()   || */datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                    datenDownload.resetDownload();
                    datenDownload.startDownload();
                    datenDownload.start.countRestarted = ++restarted; //datenDownload.start ist neu!!!
                    return datenDownload;
                }
            }
        }
        return null;
    }

    private boolean getDown(int max) {
        int count = 0;
        for (DatenDownload datenDownload : this) {
            Start s = datenDownload.start;
            if (s != null) {
                if (s.status == Start.STATUS_RUN) {
                    ++count;
                    if (count >= max) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private Optional<DatenDownload> nextPossibleDownload() {
        //erster Versuch, Start mit einem anderen Sender
        for (DatenDownload datenDownload : this) {
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(datenDownload, 1)) {
                        return Optional.of(datenDownload);
                    }
                }
            }
        }

        //zweiter Versuch, Start mit einem passenden Sender
        for (DatenDownload datenDownload : this) {
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(datenDownload, Konstanten.MAX_SENDER_FILME_LADEN)) {
                        return Optional.of(datenDownload);
                    }
                }
            }
        }

        return Optional.empty();
    }

    /**
     * Check if host is part of a CDN server network.
     * Currently we only check for Akamai
     * @param host url
     * @return true if it belongs to a CDN
     */
    private boolean isCDN(final String host) {
        return host.contains("akamaihd.net") || host.contains("cdn-storage.br.de");
    }

    private boolean maxSenderLaufen(DatenDownload d, final int max) {
        //true wenn bereits die maxAnzahl pro Sender läuft
        try {
            int counter = 0;
            final String host = getHost(d);
            for (DatenDownload download : this) {
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN
                            && getHost(download).equalsIgnoreCase(host)) {
                        if (!isCDN(d.arr[DatenDownload.DOWNLOAD_FILM_URL])) {
                            counter++;
                            if (counter >= max) {
                                return true;
                            }
                        }
                    }
                }
            }
            return false;
        } catch (Exception ex) {
            return false;
        }
    }

    private String getHost(DatenDownload datenDownload) {
        String host = "";
        try {
            try {
                URL url = new URL(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
                String tmp = url.getHost();
                if (tmp.contains(".")) {
                    host = tmp.substring(tmp.lastIndexOf('.'));
                    tmp = tmp.substring(0, tmp.lastIndexOf('.'));
                    if (tmp.contains(".")) {
                        host = tmp.substring(tmp.lastIndexOf('.') + 1) + host;
                    } else if (tmp.contains("/")) {
                        host = tmp.substring(tmp.lastIndexOf('/') + 1) + host;
                    } else {
                        host = "host";
                    }
                }
            } catch (Exception ex) {
                host = "host";
            } finally {
                if (host.isEmpty()) {
                    host = "host";
                }
            }
        } catch (Exception ex) {
            host = "exception";
        }
        return host;
    }
}
