/*
 *   MediathekView
 *   Copyright (C) 2008 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import java.io.File;
import java.util.Iterator;
import javax.swing.JOptionPane;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.filme.filmeSuchen.sender.Mediathek3Sat;
import mediathek.controller.filme.filmeSuchen.sender.MediathekNdr;
import mediathek.controller.filme.filmeSuchen.sender.MediathekSwr;
import mediathek.controller.filme.filmeSuchen.sender.MediathekZdf;
import mediathek.controller.io.AsxLesen;
import mediathek.controller.io.starter.Starts;
import mediathek.gui.dialog.DialogZielDatei;
import mediathek.tool.DatumZeit;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;

public class DatenDownload implements Comparable<DatenDownload> {
    //Tags Filme

    public static final String DOWNLOAD = "Downlad";
    public static final int DOWNLOAD_MAX_ELEM = 19;
    //
    public static final String DOWNLOAD_NR = "Nr";
    public static final int DOWNLOAD_NR_NR = 0;
    public static final String DOWNLOAD_ABO = "Abo"; // wenn das Feld gefüllt ist, ist der Download ein Abo
    public static final int DOWNLOAD_ABO_NR = 1;
    public static final String DOWNLOAD_SENDER = "Sender";
    public static final int DOWNLOAD_SENDER_NR = 2;
    public static final String DOWNLOAD_THEMA = "Thema";
    public static final int DOWNLOAD_THEMA_NR = 3;
    public static final String DOWNLOAD_TITEL = "Titel";
    public static final int DOWNLOAD_TITEL_NR = 4;
    public static final String DOWNLOAD_DATUM = "Datum";
    public static final int DOWNLOAD_DATUM_NR = 5;
    public static final String DOWNLOAD_ZEIT = "Zeit";
    public static final int DOWNLOAD_ZEIT_NR = 6;
    public static final String DOWNLOAD_URL = "URL";
    public static final int DOWNLOAD_URL_NR = 7;
    public static final String DOWNLOAD_URL_AUTH = "URL-Auth";
    public static final int DOWNLOAD_URL_AUTH_NR = 8;
    public static final String DOWNLOAD_URL_RTMP = "URL-rtmp";
    public static final int DOWNLOAD_URL_RTMP_NR = 9;
    public static final String DOWNLOAD_PROGRAMMSET = "Programmset";
    public static final int DOWNLOAD_PROGRAMMSET_NR = 10;
    public static final String DOWNLOAD_PROGRAMM = "Programm";
    public static final int DOWNLOAD_PROGRAMM_NR = 11;
    public static final String DOWNLOAD_PROGRAMM_AUFRUF = "Programmaufruf";
    public static final int DOWNLOAD_PROGRAMM_AUFRUF_NR = 12;
    public static final String DOWNLOAD_PROGRAMM_RESTART = "Restart";
    public static final int DOWNLOAD_PROGRAMM_RESTART_NR = 13;
    public static final String DOWNLOAD_ZIEL_DATEINAME = "Dateiname";
    public static final int DOWNLOAD_ZIEL_DATEINAME_NR = 14;
    public static final String DOWNLOAD_ZIEL_PFAD = "Pfad";
    public static final int DOWNLOAD_ZIEL_PFAD_NR = 15;
    public static final String DOWNLOAD_ZIEL_PFAD_DATEINAME = "Pfad-Dateiname";
    public static final int DOWNLOAD_ZIEL_PFAD_DATEINAME_NR = 16;
    public static final String DOWNLOAD_ART = "Art"; //Art des Downloads: direkter Dateidownload oder über ein Programm
    public static final int DOWNLOAD_ART_NR = 17;
    public static final String DOWNLOAD_QUELLE = "Quelle"; //Quelle: gestartet über einen Button, Download, Abo
    public static final int DOWNLOAD_QUELLE_NR = 18;
    public static final String[] DOWNLOAD_COLUMN_NAMES = {DOWNLOAD_NR, DOWNLOAD_ABO, DOWNLOAD_SENDER, DOWNLOAD_THEMA, DOWNLOAD_TITEL,
        DOWNLOAD_DATUM, DOWNLOAD_ZEIT, DOWNLOAD_URL, DOWNLOAD_URL_AUTH, DOWNLOAD_URL_RTMP,
        DOWNLOAD_PROGRAMMSET, DOWNLOAD_PROGRAMM, DOWNLOAD_PROGRAMM_AUFRUF, DOWNLOAD_PROGRAMM_RESTART,
        DOWNLOAD_ZIEL_DATEINAME, DOWNLOAD_ZIEL_PFAD, DOWNLOAD_ZIEL_PFAD_DATEINAME, DOWNLOAD_ART, DOWNLOAD_QUELLE};
    public String[] arr;

    public DatenDownload() {
        makeArr();
    }

    public DatenDownload(DatenPset pSet, DatenFilm film, int quelle, DatenAbo abo) {
        makeArr();
        arr[DOWNLOAD_NR_NR] = film.arr[DatenFilm.FILM_NR_NR];
        arr[DOWNLOAD_SENDER_NR] = film.arr[DatenFilm.FILM_SENDER_NR];
        arr[DOWNLOAD_THEMA_NR] = film.arr[DatenFilm.FILM_THEMA_NR];
        arr[DOWNLOAD_TITEL_NR] = film.arr[DatenFilm.FILM_TITEL_NR];
        arr[DOWNLOAD_URL_NR] = film.arr[DatenFilm.FILM_URL_NR];
        arr[DOWNLOAD_URL_AUTH_NR] = film.arr[DatenFilm.FILM_URL_AUTH_NR];
        arr[DOWNLOAD_DATUM_NR] = film.arr[DatenFilm.FILM_DATUM_NR];
        arr[DOWNLOAD_ZEIT_NR] = film.arr[DatenFilm.FILM_ZEIT_NR];
        arr[DOWNLOAD_URL_RTMP_NR] = film.arr[DatenFilm.FILM_URL_RTMP_NR];
        arr[DOWNLOAD_QUELLE_NR] = String.valueOf(quelle);
        aufrufBauen(pSet, abo);
    }

    public DatenDownload getCopy() {
        DatenDownload ret = new DatenDownload();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        return ret;
    }

    public void aufMichKopieren(DatenDownload datenDownload) {
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String(datenDownload.arr[i]);
        }
    }

    public boolean istAbo() {
        return !arr[DatenDownload.DOWNLOAD_ABO_NR].equals("");
    }

    public int getArt() {
        try {
            return Integer.parseInt(arr[DOWNLOAD_ART_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(946325800, this.getClass().getName(), ex);
            return Starts.ART_PROGRAMM;
        }
    }

    public int getQuelle() {
        try {
            return Integer.parseInt(arr[DOWNLOAD_QUELLE_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(649632580, this.getClass().getName(), ex);
            return Starts.QUELLE_BUTTON;
        }
    }

    public boolean isRestart() {
        if (arr[DOWNLOAD_PROGRAMM_RESTART_NR].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_RESTART_NR]);
    }

    private void aufrufBauen(DatenPset pSet, DatenAbo abo) {
        //zieldatei und pfad bauen und eintragen
        //und gibt die Zieldatei mit Pfad zurück
        try {
            DatenProg programm = pSet.getProgUrl(arr[DOWNLOAD_URL_NR]);
            // ##############################################
            // für die alten Versionen:
            // ##############################################
            if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].contains("%p") || pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].contains("%n")) {
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].replace("%n", "");
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].replace("%p", "");
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR] = Boolean.TRUE.toString();
                Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PSET, this.getClass().getSimpleName());
            }
            if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].contains("%p") || pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].contains("%n")) {
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].replace("%n", "");
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].replace("%p", "");
                pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR] = Boolean.TRUE.toString();
                Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PSET, this.getClass().getSimpleName());
            }
            Iterator<DatenProg> it = pSet.getListeProg().iterator();
            while (it.hasNext()) {
                DatenProg prog = it.next();
                if (prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].contains("%p") || prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].contains("%n")) {
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%n", "");
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%p", "");
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = Boolean.TRUE.toString();
                    pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR] = Boolean.TRUE.toString();
                    Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PSET, this.getClass().getSimpleName());
                }
            }
            // ##############################################
            // ##############################################
            arr[DOWNLOAD_PROGRAMMSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
            int art = pSet.checkDownloadDirekt(arr[DOWNLOAD_URL_NR]);
            arr[DOWNLOAD_ART_NR] = String.valueOf(art);
            if (art == Starts.ART_DOWNLOAD) {
                arr[DatenDownload.DOWNLOAD_PROGRAMM_NR] = "direkter Download";
            } else {
                arr[DatenDownload.DOWNLOAD_PROGRAMM_NR] = programm.arr[DatenProg.PROGRAMM_NAME_NR];
            }
            arr[DOWNLOAD_PROGRAMM_RESTART_NR] = String.valueOf(programm.isRestart());
            dateinamePfadBauen(pSet, abo);
            programmaufrufBauen(programm);
        } catch (Exception ex) {
            Log.fehlerMeldung(825600145, this.getClass().getName(), ex);
        }
    }

    private void dateinamePfadBauen(DatenPset pSet, DatenAbo abo) {
        if (!pSet.progsContainPath()) {
            // dann können wir uns das sparen
            return;
        }
        arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR] = pSet.getZielDateiname(arr[DOWNLOAD_URL_NR]);
        arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR] = pSet.getZielPfad();
        String name = arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR];
        String pfad = arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR];
        // ##############################
        // Pfad sinnvoll belegen
        // ##############################
        if (pfad.equals("")) {
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR] = Boolean.TRUE.toString();
            // wenn leer, vorbelegen
            pfad = GuiFunktionen.getHomePath();
            pfad = GuiFunktionen.addsPfad(pfad, Konstanten.VERZEICNHISS_DOWNLOADS);
            if (abo == null) {
                // nur wenn kein Abo
                pfad = GuiFunktionen.addsPfad(pfad, arr[DatenDownload.DOWNLOAD_THEMA_NR]);
            }
        }
        if (abo != null) {
            // Bei Abos: den Namen des Abos an den Zielpfad anhängen, leer kann er jetzt ja schon nicht mehr sein
            pfad = GuiFunktionen.addsPfad(pfad, abo.arr[DatenAbo.ABO_ZIELPFAD_NR]);
            arr[DatenDownload.DOWNLOAD_ABO_NR] = abo.arr[DatenAbo.ABO_NAME_NR];
        }
        // ##############################
        // Name sinnvoll belegen
        // ##############################
        if (name.equals("")) {
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR] = Boolean.TRUE.toString();
            name = DatumZeit.getHeute_yyyyMMdd() + "_" + arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
        }
        name = name.replace("%D", arr[DOWNLOAD_DATUM_NR].equals("") ? DatumZeit.getHeute_yyyyMMdd() : datumDatumZeitReinigen(datumDrehen(arr[DOWNLOAD_DATUM_NR])));
        name = name.replace("%d", arr[DOWNLOAD_ZEIT_NR].equals("") ? DatumZeit.getJetzt_HHMMSS() : datumDatumZeitReinigen(arr[DOWNLOAD_ZEIT_NR]));
        name = name.replace("%t", arr[DOWNLOAD_THEMA_NR]);
        name = name.replace("%T", arr[DOWNLOAD_TITEL_NR]);
        name = name.replace("%s", arr[DOWNLOAD_SENDER_NR]);
        name = name.replace("%H", DatumZeit.getHeute_yyyyMMdd());
        name = name.replace("%h", DatumZeit.getJetzt_HHMMSS());
        name = name.replace("%N", GuiFunktionen.getDateiName(arr[DOWNLOAD_URL_NR]));
        // prüfen ob das Suffix 2x vorkommt
        if (name.length() > 8) {
            String suf1 = name.substring(name.length() - 8, name.length() - 4);
            String suf2 = name.substring(name.length() - 4);
            if (suf1.startsWith(".") && suf2.startsWith(".")) {
                if (suf1.equalsIgnoreCase(suf2)) {
                    name = name.substring(0, name.length() - 4);
                }
            }
        }
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        // ##############################
        // im Auto-Modus keinen Dialog anzeigen
        // Pfad abfragen
        // ##############################################
        if (!Daten.auto && Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_FRAGEN_NR])) {
            // %n und %p werden jetzt gleich verwendet, es wird immer ein Dialog angezeigt in dem beides (Name und Pfad) geändert werden kann
            name = GuiFunktionen.replaceLeerDateiname(name, true/* pfadtrennerEntfernen */);
            DialogZielDatei dialog = new DialogZielDatei(null, true, pfad, name);
            dialog.setVisible(true);
            if (!dialog.ok) {
                pfad = GuiFunktionen.getHomePath();
                pfad = GuiFunktionen.addsPfad(pfad, arr[DatenDownload.DOWNLOAD_THEMA_NR]);
                name = DatumZeit.getHeute_yyyyMMdd() + "_" + arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
            } else {
                name = dialog.zielDateiname;
                pfad = dialog.zielPfad;
            }
        }
        // ##############################################
        // und jetzt noch den Pfad aufbauen
        // ##############################################
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        // Kürzen
        if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN_NR])) {
            // nur dann ist was zu tun
            int laenge = GuiKonstanten.LAENGE_DATEINAME;
            if (!pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR].equals("")) {
                laenge = Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR]);
            }
            if (name.length() > laenge) {
                name = name.substring(0, laenge - 4) + name.substring(name.length() - 4);
            }
        }
        arr[DOWNLOAD_ZIEL_DATEINAME_NR] = GuiFunktionen.replaceLeerDateiname(name, true /* pfadtrennerEntfernen */);
        arr[DOWNLOAD_ZIEL_PFAD_NR] = pfad;
        arr[DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] = GuiFunktionen.addsPfad(pfad, arr[DOWNLOAD_ZIEL_DATEINAME_NR]);
    }

    private void programmaufrufBauen(DatenProg programm) {
        String befehlsString = programm.getProgrammAufruf();
        befehlsString = befehlsString.replace("**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        befehlsString = befehlsString.replace("%f", arr[DOWNLOAD_URL_NR]);
        befehlsString = befehlsString.replace("%F", getUrlFlvstreamer());
        befehlsString = befehlsString.replace("%k", getUrlLow());
        befehlsString = befehlsString.replace("%x", AsxLesen.lesen(arr[DOWNLOAD_URL_NR]));
        //Auth eintragen
        if (arr[DOWNLOAD_URL_AUTH_NR].equals("")) {
            befehlsString = befehlsString.replace("%a", "");
            befehlsString = befehlsString.replace("%A", "");
        } else {
            befehlsString = befehlsString.replace("%a", arr[DOWNLOAD_URL_AUTH_NR]);
            befehlsString = befehlsString.replace("%A", "-W " + arr[DOWNLOAD_URL_AUTH_NR]);
        }
        if (getArt() == Starts.ART_DOWNLOAD) {
            arr[DOWNLOAD_PROGRAMM_AUFRUF_NR] = "";
        } else {
            arr[DOWNLOAD_PROGRAMM_AUFRUF_NR] = befehlsString;
        }
    }

    private String getUrlFlvstreamer() {
        String ret = "";
        if (!arr[DOWNLOAD_URL_RTMP_NR].equals("")) {
            ret = arr[DOWNLOAD_URL_RTMP_NR];
        } else {
            if (arr[DOWNLOAD_URL_NR].startsWith(GuiKonstanten.RTMP_PRTOKOLL)) {
                ret = GuiKonstanten.RTMP_FLVSTREAMER + arr[DOWNLOAD_URL_NR];
            } else {
                ret = arr[DOWNLOAD_URL_NR];
            }
        }
        return ret;
    }

    private String getUrlLow() {
        String ret = "";
        if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekSwr.SENDER)) {
            //swr
            ret = arr[DOWNLOAD_URL_NR].replace(".m.mp4", ".l.mp4");
        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(Mediathek3Sat.SENDER)) {
            //3Sat
            ret = arr[DOWNLOAD_URL_NR].replace("/veryhigh/", "/300/");
        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekZdf.SENDER)) {
            //ZDF
            ret = arr[DOWNLOAD_URL_NR].replace("/veryhigh/", "/300/");
        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekNdr.SENDER)) {
            //NDR
            ret = arr[DOWNLOAD_URL_NR].replace(".hq.", ".lo.");
        }
        return ret;
    }

    @Override
    public int compareTo(DatenDownload arg0) {
        int ret = 0;
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        if ((ret = sorter.compare(arr[DatenDownload.DOWNLOAD_SENDER_NR], arg0.arr[DatenDownload.DOWNLOAD_SENDER_NR])) == 0) {
            ret = sorter.compare(arr[DatenDownload.DOWNLOAD_THEMA_NR], arg0.arr[DatenDownload.DOWNLOAD_THEMA_NR]);
        }
        return ret;
    }

    private void makeArr() {
        arr = new String[DOWNLOAD_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }

    private String datumDrehen(String datum) {
        String ret = "";
        if (!datum.equals("")) {
            try {
                if (datum.length() == 10) {
                    String tmp = datum.substring(6); // Jahr
                    tmp += "." + datum.substring(3, 5); // Monat
                    tmp += "." + datum.substring(0, 2); // Tag
                    ret = tmp;
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(775421006, "DatenFilm.datumDrehen", ex, datum);
            }

        }
        return ret;
    }

    private String datumDatumZeitReinigen(String datum) {
        String ret = "";
        ret = datum;
        ret = ret.replace(":", "");
        ret = ret.replace(".", "");
        return ret;
    }
}
