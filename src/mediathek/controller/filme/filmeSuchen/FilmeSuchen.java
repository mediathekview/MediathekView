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
package mediathek.controller.filme.filmeSuchen;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.event.EventListenerList;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.Main;
import mediathek.controller.filme.FilmListener;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.filme.filmeSuchen.sender.*;
import mediathek.controller.io.GetUrl;
import mediathek.daten.ListeFilme;
import mediathek.tool.DatumZeit;

public class FilmeSuchen {

    public LinkedList<MediathekReader> mediathekListe = new LinkedList<MediathekReader>();
    public boolean allesLaden = true;
    public ListeFilme listeFilmeNeu; // neu angelegte Lise und da kommen die neu gesuchten Filme rein
    private ListeFilme listeFilmeAlt = null; // ist nur eine Referenz auf die bestehende Liste und die bleibt unverändert!!!
    private EventListenerList listeners = new EventListenerList();
    private ListeRunSender listeSenderLaufen = new ListeRunSender();
    private LinkedList<String> fertigMeldung = new LinkedList<String>();

    /**
     *
     * @param ddaten
     */
    public FilmeSuchen() {
        //beobLaden = new BeobLaden();
        initMediathekReader();
    }
    // ###########################################################################################################
    // Ablauf:
    // die gefundenen Filme kommen in die "listeFilme"
    //  -> bei einem vollen Suchlauf: passiert nichts weiter
    //  -> bei einem Update: "listeFilme" mit alter Filmliste auffüllen, URLs die es schon gibt werden verworfen
    // "listeFilme" ist dann die neue komplette Liste mit Filmen
    // ##########################################################################################################

    public void filmeBeimSenderLaden(boolean aallesLaden, ListeFilme alteListe) {
        allesLaden = aallesLaden;
        initStart(alteListe);
        listeFilmeNeu.liveStreamEintragen();
        Iterator<MediathekReader> it;
        MediathekReader mr;
        it = mediathekListe.iterator();
        while (it.hasNext()) {
            mr = it.next();
            new Thread(mr).start();
        }
    }

    public void updateSender(String sender, ListeFilme alteListe) {
        // nur für den Mauskontext "Sender aktualisieren"
        allesLaden = false;
        initStart(alteListe);
        MediathekReader reader = getMediathekReader(sender);
        if (reader != null) {
            new Thread(reader).start();
        }
    }

    private void initStart(ListeFilme alteListe) {
        fertigMeldung.clear();
        listeFilmeAlt = alteListe;
        listeFilmeNeu = new ListeFilme();
        listeFilmeNeu.setInfo(alteListe.infos);
        GetUrl.resetSeitenZaehler();
    }

    public boolean senderAn(String sender) {
        MediathekReader r = getMediathekReader(sender);
        if (r != null) {
            if (r.istSenderAn()) {
                return true;
            }
        }
        return false;
    }

    public void addAdListener(FilmListener listener) {
        listeners.add(FilmListener.class, listener);
    }

    //===================================
    // private
    //===================================
    private void initMediathekReader() {
        //Reader laden Spaltenweises Laden
        mediathekListe.add(new MediathekArd(this));
        mediathekListe.add(new MediathekArdPodcast(this));
        mediathekListe.add(new MediathekZdf(this));
        mediathekListe.add(new MediathekArte7(this, true /* de */));
        mediathekListe.add(new MediathekArte7(this, false));
        mediathekListe.add(new Mediathek3Sat(this));
        mediathekListe.add(new MediathekSwr(this));
        mediathekListe.add(new MediathekNdr(this));
        // Spalte 2
        mediathekListe.add(new MediathekMdr(this));
        mediathekListe.add(new MediathekWdr(this));
        mediathekListe.add(new MediathekHr(this));
        mediathekListe.add(new MediathekRbb(this));
        mediathekListe.add(new MediathekBr(this));
        mediathekListe.add(new MediathekSf(this));
        mediathekListe.add(new MediathekSfPod(this));
        mediathekListe.add(new MediathekOrf(this));
    }

    private MediathekReader getMediathekReader(String sender) {
        MediathekReader ret = null;
        if (!sender.equals("")) {
            Iterator<MediathekReader> it = mediathekListe.iterator();
            while (it.hasNext()) {
                ret = it.next();
                if (ret.getSenderName().equalsIgnoreCase(sender)) {
                    break;
                }
            }
        }
        return ret;
    }

    public synchronized void melden(String sender, int max, int progress, String text) {
        RunSender runSender = listeSenderLaufen.getSender(sender);
        if (runSender != null) {
            runSender.max = max;
            runSender.progress = progress;
        } else {
            // Sender startet
            Log.systemMeldung("Starten " + sender + ": " + DatumZeit.getJetzt_HH_MM_SS());
            listeSenderLaufen.add(new RunSender(sender, max, progress));
            //wird beim Start des Senders aufgerufen, 1x
            if (listeSenderLaufen.size() <= 1 /* erster Aufruf */) {
                notifyStart(new FilmListenerElement(sender, text, listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
            }
        }
        notifyProgress(new FilmListenerElement(sender, text, listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        progressBar();
    }

    public void meldenFertig(String sender) {
        //wird ausgeführt wenn Sender beendet ist
        int MAX_SENDER = 25, MAX1 = 22, MAX2 = 15, MAX3 = 20;
        Log.systemMeldung("Fertig " + sender + ": " + DatumZeit.getJetzt_HH_MM_SS());
        RunSender run = listeSenderLaufen.senderFertig(sender);
        if (run != null) {
            String zeile = "";
            zeile += textLaenge(MAX_SENDER, "Sender:   " + run.sender);
            zeile += textLaenge(MAX1, " Laufzeit: " + run.getLaufzeitMinuten() + " Min.");
            zeile += textLaenge(MAX2, " Seiten: " + GetUrl.getSeitenZaehler(run.sender));
            zeile += textLaenge(MAX3, " Ladefehler: " + GetUrl.getSeitenZaehlerFehler(run.sender));
            zeile += textLaenge(MAX3, " Filme: " + listeFilmeNeu.countSender(run.sender));
            fertigMeldung.add(zeile);
        }
        // wird einmal aufgerufen, wenn der Sender fertig ist
        if (listeSenderLaufen.listeFertig()) {
            Log.systemMeldung("Alle Sender fertig: " + DatumZeit.getJetzt_HH_MM_SS());
            listeFilmeNeu.sort();
            if (!allesLaden) {
                // alte Filme eintragen
                listeFilmeNeu.updateListe(listeFilmeAlt, true /* über den Index vergleichen */);
            }
            listeFilmeAlt = null; // brauchmer nicht mehr
            metaDatenSchreiben();
            Log.systemMeldung(fertigMeldung.toArray(new String[0]));
            notifyFertig(new FilmListenerElement(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        } else {
            //nur ein Sender fertig
            notifyProgress(new FilmListenerElement(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        }
    }

    private void progressBar() {
        int max = listeSenderLaufen.getMax();
        int progress = listeSenderLaufen.getProgress();
        int proz = 0;
        String text;
        if (max != 0) {
            if (progress != 0) {
                proz = progress * 100 / max;
            }
            if (max > 0 && proz == 100) {
                proz = 99;
            }
            text = "  [ ";
            int a = proz / 10;
            for (int i = 0; i < a; ++i) {
                text += "#";
            }
            for (int i = 0; i < (10 - a); ++i) {
                text += "-";
            }
            text += " ]  " + GetUrl.getSeitenZaehler() + " Seiten  /  " + proz + "% von " + max + " Themen  /  Filme: " + listeFilmeNeu.size();
            Log.progress(text);
        }
    }

    private void metaDatenSchreiben() {
        // FilmlisteMetaDaten
        listeFilmeNeu.metaDaten = ListeFilme.newMetaDaten();
        if (!Daten.filmeLaden.getStop() /* löschen */) {
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_DATUM_NR] = DatumZeit.getJetzt_ddMMyyyy_HHmm();
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_NUR_ZEIT_NR] = DatumZeit.getJetzt_HH_MM_SS();
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_NUR_DATUM_NR] = DatumZeit.getHeute_dd_MM_yyyy();
        } else {
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_DATUM_NR] = "";
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_NUR_ZEIT_NR] = "";
            listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_NUR_DATUM_NR] = "";
        }
        listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_ANZAHL_NR] = String.valueOf(listeFilmeNeu.size());
        listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_VERSION_NR] = Konstanten.VERSION;
        listeFilmeNeu.metaDaten[ListeFilme.FILMLISTE_PRGRAMM_NR] = Log.getCompileDate();
    }

    private String textLaenge(int max, String text) {
        if (text.length() > max) {
            //text = text.substring(0, MAX);
            text = text.substring(0, max - 1);
        }
        while (text.length() < max) {
            text = text + " ";
        }
        return text;
    }

    private void notifyStart(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.start(filmListenerElement);
        }
    }

    private void notifyProgress(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.progress(filmListenerElement);
        }

    }

    private void notifyFertig(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.fertig(filmListenerElement);
        }

    }
}
