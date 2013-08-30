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
package mediathek.controller.filmeLaden.suchen;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.event.EventListenerList;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.controller.filmeLaden.suchen.sender.Mediathek3Sat;
import mediathek.controller.filmeLaden.suchen.sender.MediathekArd;
import mediathek.controller.filmeLaden.suchen.sender.MediathekArdPodcast;
import mediathek.controller.filmeLaden.suchen.sender.MediathekArte_de;
import mediathek.controller.filmeLaden.suchen.sender.MediathekArte_fr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekBr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekHr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekKika;
import mediathek.controller.filmeLaden.suchen.sender.MediathekMdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekNdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekOrf;
import mediathek.controller.filmeLaden.suchen.sender.MediathekRbb;
import mediathek.controller.filmeLaden.suchen.sender.MediathekReader;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSrf;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSrfPod;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSwr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekWdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekZdf;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.ListeFilme;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Log;
import mediathek.tool.MVUrlDateiGroesse;

public class FilmeSuchenSender {

    public LinkedList<MediathekReader> mediathekListe = new LinkedList<MediathekReader>();
    public boolean senderAllesLaden = false; // die Sender werden komplett geladen / nur die neuesten Filme geladen
    public boolean updateFilmliste = false; // die bestehende Filmliste wird upgedatet / es wird eine neue Filmlise gestartet
    public ListeFilme listeFilmeNeu; // neu angelegte Liste und da kommen die neu gesuchten Filme rein
    public ListeFilme listeFilmeAlt = null; // ist nur eine Referenz auf die bestehende Liste und die bleibt unverändert!!!
    private EventListenerList listeners = new EventListenerList();
    private ListeRunSender listeSenderLaufen = new ListeRunSender();
    private Date startZeit = null;
    private Date stopZeit = null;
    private ArrayList<String> runde1 = new ArrayList<String>();
    private ArrayList<String> runde2 = new ArrayList<String>();
    private ArrayList<String> runde3 = new ArrayList<String>();
    private String[] titel1 = {"Sender       ", "[min]", "Seiten", "Filme", "Fehler", "FVersuche", "FZeit[s]"};
    private String[] titel3 = {"Sender       ", "Geladen[MB]", "Nix", "Deflaet", "Gzip", "AnzGroesse"};
    private final String TRENNER = " | ";
    private final String TTRENNER = " || ";

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
        mediathekListe.add(new MediathekArdPodcast(this, 1));
        mediathekListe.add(new MediathekZdf(this, 0));
        mediathekListe.add(new MediathekArte_de(this, 1));
        mediathekListe.add(new MediathekArte_fr(this, 1));
        mediathekListe.add(new Mediathek3Sat(this, 0));
        mediathekListe.add(new MediathekSwr(this, 1));
        mediathekListe.add(new MediathekNdr(this, 1));
        mediathekListe.add(new MediathekKika(this, 0));
        // Spalte 2
        mediathekListe.add(new MediathekMdr(this, 0));
        mediathekListe.add(new MediathekWdr(this, 0));
        mediathekListe.add(new MediathekHr(this, 0));
        mediathekListe.add(new MediathekRbb(this, 1));
        mediathekListe.add(new MediathekBr(this, 0));
        mediathekListe.add(new MediathekSrf(this, 1));
        mediathekListe.add(new MediathekSrfPod(this, 0));
        mediathekListe.add(new MediathekOrf(this, 0));
    }

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    public synchronized void filmeBeimSenderLaden(boolean aallesLaden, boolean uupdateFilmliste, ListeFilme alteListe) {
        senderAllesLaden = aallesLaden;
        updateFilmliste = uupdateFilmliste;
        initStart(alteListe);
        listeFilmeNeu.liveStreamEintragen();
        // die mReader nach Prio starten
        mrStarten(0);
        if (!Daten.filmeLaden.getStop()) {
            mrWarten();
            mrStarten(1);
        }
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
    }

    private synchronized void mrWarten() {
        try {
            for (int i = 0; i < 6; ++i) {
                if (Daten.filmeLaden.getStop()) {
                    break;
                }
                this.wait(30 * 1000); // 0,5 Min. warten, Sender nach der Gesamtlaufzeit starten
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(978754213, Log.FEHLER_ART_PROG, "FilmeSuchenSender.mrWarten", ex);
        }
    }

    public void updateSender(String[] nameSenderFilmliste, boolean aallesLaden, ListeFilme alteListe) {
        // nur für den Mauskontext "Sender aktualisieren"
        boolean starten = false;
        senderAllesLaden = aallesLaden; // alles oder nur ein Update laden
        updateFilmliste = true; // die alte Filmliste wird aktualisiert, hier immer
        initStart(alteListe);
        listeFilmeNeu.liveStreamEintragen();
        Iterator<MediathekReader> it = mediathekListe.iterator();
        while (it.hasNext()) {
            MediathekReader reader = it.next();
            for (String s : nameSenderFilmliste) {
                if (reader.checkNameSenderFilmliste(s)) {
                    starten = true;
                    new Thread(reader).start();
                }
            }
        }
        if (!starten) {
            // dann fertig
            meldenFertig("");

        }
    }

    public void senderInAlteListeLoeschen(String sender) {
        listeFilmeAlt.delSender(sender);
    }

    public String[] getNamenSender() {
        // liefert eine Array mit allen Sendernamen
        LinkedList<String> liste = new LinkedList<String>();
        Iterator<MediathekReader> it = mediathekListe.iterator();
        while (it.hasNext()) {
            liste.add(it.next().getNameSender());
        }
        GuiFunktionen.listeSort(liste);
        return liste.toArray(new String[]{});
    }

    public synchronized void melden(String sender, int max, int progress, String text) {
        RunSender runSender = listeSenderLaufen.getSender(sender);
        if (runSender != null) {
            runSender.max = max;
            runSender.progress = progress;
        } else {
            // Sender startet
            Log.systemMeldung("Starten[" + ((senderAllesLaden) ? "alles" : "update") + "] " + sender + ": " + DatumZeit.getJetzt_HH_MM_SS());
            listeSenderLaufen.add(new RunSender(sender, max, progress));
            //wird beim Start des Senders aufgerufen, 1x
            if (listeSenderLaufen.size() <= 1 /* erster Aufruf */) {
                notifyStart(new ListenerFilmeLadenEvent(sender, text, listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
            }
        }
        notifyProgress(new ListenerFilmeLadenEvent(sender, text, listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        progressBar();
    }

    public void meldenFertig(String sender) {
        //wird ausgeführt wenn Sender beendet ist
        String zeile = "";
        Log.systemMeldung("-------------------------------------------------------------------------------------");
        Log.systemMeldung("Fertig " + sender + ": " + DatumZeit.getJetzt_HH_MM_SS() + ", Filme: " + listeFilmeNeu.countSender(sender));
        Log.systemMeldung("-------------------------------------------------------------------------------------");
        RunSender run = listeSenderLaufen.senderFertig(sender);
        if (run != null) {
            String groesse = (GetUrl.getSeitenZaehler(GetUrl.LISTE_SUMME_BYTE, run.sender) == 0) ? "<1" : Long.toString(GetUrl.getSeitenZaehler(GetUrl.LISTE_SUMME_BYTE, run.sender));
            String[] ladeart = GetUrl.getZaehlerLadeArt(run.sender);
            // =================================
            // Zeile1
            zeile = textLaenge(titel1[0].length(), run.sender) + TTRENNER;
            zeile += textLaenge(titel1[1].length(), run.getLaufzeitMinuten()) + TRENNER;
            zeile += textLaenge(titel1[2].length(), String.valueOf(GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER, run.sender))) + TRENNER;
            zeile += textLaenge(titel1[3].length(), String.valueOf(listeFilmeNeu.countSender(run.sender))) + TRENNER;
            zeile += textLaenge(titel1[4].length(), String.valueOf(GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER_FEHlER, run.sender))) + TRENNER;
            zeile += textLaenge(titel1[5].length(), String.valueOf(GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER_FEHLERVERSUCHE, run.sender))) + TRENNER;
            zeile += textLaenge(titel1[6].length(), String.valueOf(GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER_WARTEZEIT_FEHLVERSUCHE, run.sender))) + TRENNER;
            runde1.add(zeile);
            // =================================
            // Zeile3
            zeile = textLaenge(titel3[0].length(), run.sender) + TTRENNER;
            zeile += textLaenge(titel3[1].length(), groesse) + TRENNER;
            zeile += textLaenge(titel3[2].length(), ladeart[0]) + TRENNER;
            zeile += textLaenge(titel3[3].length(), ladeart[1]) + TRENNER;
            zeile += textLaenge(titel3[4].length(), ladeart[2]) + TRENNER;
            zeile += textLaenge(titel3[5].length(), String.valueOf(MVUrlDateiGroesse.getZaehler(run.sender))) + TRENNER;
            runde3.add(zeile);
        }
        if (!listeSenderLaufen.listeFertig()) {
            //nur ein Sender fertig
            notifyProgress(new ListenerFilmeLadenEvent(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        } else {
            // wird einmal aufgerufen, wenn alle Sender fertig sind
            Log.progressEnde();
            endeMeldung();
            notifyFertig(new ListenerFilmeLadenEvent(sender, "", listeSenderLaufen.getMax(), listeSenderLaufen.getProgress()));
        }
    }

    //===================================
    // private
    //===================================
    private void endeMeldung() {
        // wird einmal aufgerufen, wenn alle Sender fertig sind
        String zeile = "";
        Log.progressEnde();
        // Sender ===============================================
        // ======================================================
        Log.systemMeldung("");
        Log.systemMeldung("");
        Log.systemMeldung("=================================================================================");
        Log.systemMeldung("==  Sender  =====================================================================");
        Log.systemMeldung("");
        // Zeile 1 =============================================
        zeile = titel1[0] + TTRENNER + titel1[1] + TRENNER + titel1[2] + TRENNER + titel1[3] + TRENNER + titel1[4] + TRENNER + titel1[5] + TRENNER + titel1[6];
        Log.systemMeldung(zeile);
        Log.systemMeldung("---------------------------------------------------------------------------------");
        for (String s : runde1) {
            Log.systemMeldung(s);
        }
        Log.systemMeldung("");
        Log.systemMeldung("");
        // Zeile 3 =============================================
        zeile = titel3[0] + TTRENNER + titel3[1] + TRENNER + titel3[2] + TRENNER + titel3[3] + TRENNER + titel3[4] + TRENNER + titel3[5];
        Log.systemMeldung(zeile);
        Log.systemMeldung("---------------------------------------------------------------------------------");
        for (String s : runde3) {
            Log.systemMeldung(s);
        }
        // Gesamt ===============================================
        // ======================================================
        int anzFilme = listeFilmeNeu.size();
        if (!senderAllesLaden || updateFilmliste) {
            // alte Filme eintragen wenn angefordert oder nur ein update gesucht wurde
            listeFilmeNeu.updateListe(listeFilmeAlt, true /* über den Index vergleichen */);
        }
        listeFilmeNeu.sort();
        listeFilmeAlt = null; // brauchmer nicht mehr
        // FilmlisteMetaDaten
        listeFilmeNeu.metaDatenSchreiben();
        stopZeit = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        int sekunden;
        try {
            sekunden = Math.round((stopZeit.getTime() - startZeit.getTime()) / (1000));
        } catch (Exception ex) {
            sekunden = -1;
        }
        Log.systemMeldung("");
        Log.systemMeldung("=================================================================================");
        Log.systemMeldung("=================================================================================");
        Log.systemMeldung("");
        Log.systemMeldung("        Filme geladen: " + anzFilme);
        Log.systemMeldung("       Seiten geladen: " + GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER));
        String groesse = (GetUrl.getSeitenZaehler(GetUrl.LISTE_SUMME_BYTE) == 0) ? "<1" : Long.toString(GetUrl.getSeitenZaehler(GetUrl.LISTE_SUMME_BYTE));
        Log.systemMeldung("   Summe geladen[MiB]: " + groesse);
        Log.systemMeldung("        Traffic [MiB]: " + GetUrl.getSummeMegaByte());
        if (sekunden <= 0) {
            sekunden = 1;
        }
        // Durchschnittswerte ausgeben
        long kb = (GetUrl.getSeitenZaehler(GetUrl.LISTE_SUMME_BYTE) * 1024) / sekunden;
        Log.systemMeldung("     ->   Rate[KiB/s]: " + (kb == 0 ? "<1" : kb));
        Log.systemMeldung("     ->    Dauer[Min]: " + (sekunden / 60 == 0 ? "<1" : sekunden / 60));
        Log.systemMeldung("            ->  Start: " + sdf.format(startZeit));
        Log.systemMeldung("            ->   Ende: " + sdf.format(stopZeit));
        Log.systemMeldung("");
        Log.systemMeldung("=================================================================================");
        Log.systemMeldung("=================================================================================");
    }

    private void initStart(ListeFilme alteListe) {
        startZeit = new Date(System.currentTimeMillis());
        runde1.clear();
        runde2.clear();
        runde3.clear();
        listeFilmeAlt = alteListe;
        listeFilmeNeu = new ListeFilme();
        GetUrl.resetZaehler();
        MVUrlDateiGroesse.resetZaehler(getNamenSender());
        Log.systemMeldung("");
        Log.systemMeldung("=======================================");
        Log.systemMeldung("Start Filme laden:");
        if (senderAllesLaden) {
            Log.systemMeldung("Filme laden: alle laden");
        } else {
            Log.systemMeldung("Filme laden: nur update laden");
        }
        if (updateFilmliste) {
            Log.systemMeldung("Filmliste: aktualisieren");
        } else {
            Log.systemMeldung("Filmliste: neue erstellen");
        }
        Log.systemMeldung("=======================================");
        Log.systemMeldung("");
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
            text += " ]  " + GetUrl.getSeitenZaehler(GetUrl.LISTE_SEITEN_ZAEHLER) + " Seiten  /  " + proz + "% von " + max + " Themen  /  Filme: " + listeFilmeNeu.size();
            Log.progress(text);
        }
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

    private void notifyStart(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.start(event);
        }
    }

    private void notifyProgress(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.progress(event);
        }

    }

    private void notifyFertig(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.fertig(event);
        }

    }
}
