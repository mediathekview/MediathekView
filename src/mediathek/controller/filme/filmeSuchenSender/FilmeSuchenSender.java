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
package mediathek.controller.filme.filmeSuchenSender;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.event.EventListenerList;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.FilmListener;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.filme.filmeSuchen.sender.*;
import mediathek.controller.io.GetUrl;
import mediathek.daten.ListeFilme;
import mediathek.tool.DatumZeit;

public class FilmeSuchenSender {

    public LinkedList<MediathekReader> mediathekListe = new LinkedList<MediathekReader>();
    public boolean allesLaden = true;
    public ListeFilme listeFilmeNeu; // neu angelegte Liste und da kommen die neu gesuchten Filme rein
    private ListeFilme listeFilmeAlt = null; // ist nur eine Referenz auf die bestehende Liste und die bleibt unverändert!!!
    private EventListenerList listeners = new EventListenerList();
    private ListeRunSender listeSenderLaufen = new ListeRunSender();
    private LinkedList<String> fertigMeldung = new LinkedList<String>();
    private Date startZeit = null;
    private Date stopZeit = null;

    /**
     * ###########################################################################################################
     * Ablauf:
     * die gefundenen Filme kommen in die "listeFilme"
     * -> bei einem vollen Suchlauf: passiert nichts weiter
     * -> bei einem Update: "listeFilme" mit alter Filmliste auffüllen, URLs die es schon gibt werden verworfen
     * "listeFilme" ist dann die neue komplette Liste mit Filmen
     * ##########################################################################################################
     */
    public FilmeSuchenSender() {
        //Reader laden Spaltenweises Laden
        mediathekListe.add(new MediathekArd(this, 0));
        mediathekListe.add(new MediathekArdPodcast(this, 0));
        mediathekListe.add(new MediathekZdf(this, 0));
        mediathekListe.add(new MediathekArte7(this, 1));
        mediathekListe.add(new Mediathek3Sat(this, 2));
        mediathekListe.add(new MediathekSwr(this, 1));
        mediathekListe.add(new MediathekNdr(this, 1));
        // Spalte 2
        mediathekListe.add(new MediathekMdr(this, 1));
        mediathekListe.add(new MediathekWdr(this, 0));
        mediathekListe.add(new MediathekHr(this, 2));
        mediathekListe.add(new MediathekRbb(this, 1));
        mediathekListe.add(new MediathekBr(this, 2));
        mediathekListe.add(new MediathekSf(this, 1));
        mediathekListe.add(new MediathekSfPod(this, 2));
        mediathekListe.add(new MediathekOrf(this, 1));
    }

    public void addAdListener(FilmListener listener) {
        listeners.add(FilmListener.class, listener);
    }

    public synchronized void filmeBeimSenderLaden(boolean aallesLaden, ListeFilme alteListe) {
        allesLaden = aallesLaden;
        initStart(alteListe);
        listeFilmeNeu.liveStreamEintragen();
        // die mReader nach Prio starten
        mrStarten(0);
        mrStarten(1);
        mrStarten(2);
    }

    private synchronized void mrStarten(int prio) {
        MediathekReader mr;
        Iterator<MediathekReader> it = mediathekListe.iterator();
        // Prio 0 laden
        while (it.hasNext()) {
            mr = it.next();
            if (mr.getStartPrio() == prio) {
                new Thread(mr).start();
            }
        }
        try {
            this.wait(2 * 60 * 1000); // 2 Min. warten, Sender nach der Gesamtlaufzeit starten
        } catch (Exception ex) {
            Log.fehlerMeldung(952210369, "FilmeSuchenSender.mrStarten", ex);
        }
    }

    public void updateSender(String nameSenderFilmliste, ListeFilme alteListe) {
        // nur für den Mauskontext "Sender aktualisieren"
        allesLaden = false;
        initStart(alteListe);
        MediathekReader reader = getMReaderNameSenderFilmliste(nameSenderFilmliste);
        if (reader != null) {
            new Thread(reader).start();
        }
    }

    public MediathekReader getMReaderNameSenderFilmliste(String nameSenderFilmliste) {
        // liefert den MediathekReader für den Sender mit dem in der Filmliste angezeigten Namen
        Iterator<MediathekReader> it = mediathekListe.iterator();
        while (it.hasNext()) {
            MediathekReader reader = it.next();
            if (reader.checkNameSenderFilmliste(nameSenderFilmliste)) {
                return reader;
            }
        }
        return null;
    }

    public MediathekReader getMReaderNameSenderMreader(String nameSenderMreader) {
        // liefert den MediathekReader mit dem Namen
        Iterator<MediathekReader> it = mediathekListe.iterator();
        while (it.hasNext()) {
            MediathekReader reader = it.next();
            if (reader.getNameSenderMreader().equals(nameSenderMreader)) {
                return reader;
            }
        }
        return null;
    }

    public String[] getNamenSenderFilmliste() {
        // liefert eine Array mit den in der Filmliste angezeigten Sendernamen
        LinkedList<String> liste = new LinkedList<String>();
        Iterator<MediathekReader> it = mediathekListe.iterator();
        while (it.hasNext()) {
            String[] s = it.next().getNameSenderFilmliste();
            for (int i = 0; i < s.length; ++i) {
                liste.add(s[i]);
            }
        }
        String[] ret = new String[liste.size()];
        for (int i = 0; i < liste.size(); ++i) {
            ret[i] = liste.get(i);
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
        int MAX_SENDER = 25, MAX1 = 22, MAX2 = 15, MAX3 = 20, MAX4 = 30;
        Log.systemMeldung("Fertig " + sender + ": " + DatumZeit.getJetzt_HH_MM_SS());
        RunSender run = listeSenderLaufen.senderFertig(sender);
        if (run != null) {
            String zeile = "";
            zeile += textLaenge(MAX_SENDER, "Sender:   " + run.sender);
            zeile += textLaenge(MAX1, " Laufzeit: " + run.getLaufzeitMinuten() + " Min.");
            zeile += textLaenge(MAX2, " Seiten: " + GetUrl.getSeitenZaehler(run.sender));
            zeile += textLaenge(MAX3, " Ladefehler: " + GetUrl.getSeitenZaehlerFehler(run.sender));
            zeile += textLaenge(MAX3, " Fehlversuche: " + GetUrl.getSeitenZaehlerFehlerVersuche(run.sender));
            zeile += textLaenge(MAX4, " Wartezeit Fehler[s]: " + GetUrl.getSeitenZaehlerWartezeitFehlerVersuche(run.sender));
            String nameFilmliste[] = getMReaderNameSenderMreader(run.sender).getNameSenderFilmliste();
            if (nameFilmliste.length == 1) {
                zeile += textLaenge(MAX3, " Filme: " + listeFilmeNeu.countSender(nameFilmliste[0]));
                fertigMeldung.add(zeile);
            } else {
                fertigMeldung.add(zeile);
                for (int i = 0; i < nameFilmliste.length; i++) {
                    zeile = "              -->  Filme [" + nameFilmliste[i] + "]: " + listeFilmeNeu.countSender(nameFilmliste[i]);
                    fertigMeldung.add(zeile);
                }
            }
        }
        // wird einmal aufgerufen, wenn alle Sender fertig sind
        if (listeSenderLaufen.listeFertig()) {
            listeFilmeNeu.sort();
            if (!allesLaden) {
                // alte Filme eintragen
                listeFilmeNeu.updateListe(listeFilmeAlt, true /* über den Index vergleichen */);
            }
            listeFilmeAlt = null; // brauchmer nicht mehr
            metaDatenSchreiben();
            stopZeit = new Date(System.currentTimeMillis());

            SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
            int minuten;
            try {
                minuten = Math.round((stopZeit.getTime() - startZeit.getTime()) / (1000 * 60));
            } catch (Exception ex) {
                minuten = -1;
            }
            Log.systemMeldung("");
            Log.systemMeldung("============================================================");
            Log.systemMeldung("============================================================");
            Log.systemMeldung("");
            Log.systemMeldung(fertigMeldung.toArray(new String[0]));
            Log.systemMeldung("");
            Log.systemMeldung("============================================================");
            Log.systemMeldung("");
            Log.systemMeldung("Seiten geladen:   " + GetUrl.getSeitenZaehler());
            Log.systemMeldung("Summe geladen:    " + GetUrl.getSumByte() / 1024 / 1024 + " MByte");
            Log.systemMeldung("  --> Start:      " + sdf.format(startZeit));
            Log.systemMeldung("  --> Ende:       " + sdf.format(stopZeit));
            Log.systemMeldung("  --> Dauer[Min]: " + (minuten == 0 ? "<1" : minuten));
            Log.systemMeldung("");
            Log.systemMeldung("============================================================");
            Log.systemMeldung("============================================================");
            notifyFertig(new FilmListenerElement(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        } else {
            //nur ein Sender fertig
            notifyProgress(new FilmListenerElement(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        }
    }

    //===================================
    // private
    //===================================
    private void initStart(ListeFilme alteListe) {
        startZeit = new Date(System.currentTimeMillis());
        fertigMeldung.clear();
        listeFilmeAlt = alteListe;
        listeFilmeNeu = new ListeFilme();
        listeFilmeNeu.setInfo(alteListe.infos);
        GetUrl.resetZaehler();
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
