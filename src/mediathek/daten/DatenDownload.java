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
import mediathek.controller.filmeLaden.suchen.sender.Mediathek3Sat;
import mediathek.controller.filmeLaden.suchen.sender.MediathekNdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSwr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekZdf;
import mediathek.controller.io.AsxLesen;
import mediathek.controller.io.starter.Start;
import mediathek.tool.DatumZeit;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class DatenDownload implements Comparable<DatenDownload> {
    //Tags Filme

    public static final int PROGRESS_NICHT_GESTARTET = -1;
    public static final int PROGRESS_WARTEN = 0;
    public static final int PROGRESS_GESTARTET = 1;
    public static final int PROGRESS_FERTIG = 1000;
    //
    public static final String DOWNLOAD_NR = "Nr";
    public static final int DOWNLOAD_NR_NR = 0;
    public static final String DOWNLOAD_FILM_NR = "Filmnr";
    public static final int DOWNLOAD_FILM_NR_NR = 1;
    public static final String DOWNLOAD_ABO = "Abo"; // wenn das Feld gefüllt ist, ist der Download ein Abo
    public static final int DOWNLOAD_ABO_NR = 2;
    public static final String DOWNLOAD_SENDER = "Sender";
    public static final int DOWNLOAD_SENDER_NR = 3;
    public static final String DOWNLOAD_THEMA = "Thema";
    public static final int DOWNLOAD_THEMA_NR = 4;
    public static final String DOWNLOAD_TITEL = "Titel";
    public static final int DOWNLOAD_TITEL_NR = 5;
    public static final String DOWNLOAD_PROGRESS = "Fortschritt";
    public static final int DOWNLOAD_PROGRESS_NR = 6;
    public static final String DOWNLOAD_DATUM = "Datum";
    public static final int DOWNLOAD_DATUM_NR = 7;
    public static final String DOWNLOAD_ZEIT = "Zeit";
    public static final int DOWNLOAD_ZEIT_NR = 8;
    public static final String DOWNLOAD_URL = "URL";
    public static final int DOWNLOAD_URL_NR = 9;
    public static final String DOWNLOAD_URL_AUTH = "URL-Auth";
    public static final int DOWNLOAD_URL_AUTH_NR = 10;
    public static final String DOWNLOAD_URL_RTMP = "URL-rtmp";
    public static final int DOWNLOAD_URL_RTMP_NR = 11;
    public static final String DOWNLOAD_PROGRAMMSET = "Programmset";
    public static final int DOWNLOAD_PROGRAMMSET_NR = 12;
    public static final String DOWNLOAD_PROGRAMM = "Programm";
    public static final int DOWNLOAD_PROGRAMM_NR = 13;
    public static final String DOWNLOAD_PROGRAMM_AUFRUF = "Programmaufruf";
    public static final int DOWNLOAD_PROGRAMM_AUFRUF_NR = 14;
    public static final String DOWNLOAD_PROGRAMM_RESTART = "Restart";
    public static final int DOWNLOAD_PROGRAMM_RESTART_NR = 15;
    public static final String DOWNLOAD_ZIEL_DATEINAME = "Dateiname";
    public static final int DOWNLOAD_ZIEL_DATEINAME_NR = 16;
    public static final String DOWNLOAD_ZIEL_PFAD = "Pfad";
    public static final int DOWNLOAD_ZIEL_PFAD_NR = 17;
    public static final String DOWNLOAD_ZIEL_PFAD_DATEINAME = "Pfad-Dateiname";
    public static final int DOWNLOAD_ZIEL_PFAD_DATEINAME_NR = 18;
    public static final String DOWNLOAD_ART = "Art"; //Art des Downloads: direkter Dateidownload oder über ein Programm
    public static final int DOWNLOAD_ART_NR = 19;
    public static final String DOWNLOAD_QUELLE = "Quelle"; //Quelle: gestartet über einen Button, Download, Abo
    public static final int DOWNLOAD_QUELLE_NR = 20;
    //
    public static final String DOWNLOAD = "Downlad";
    public static final int DOWNLOAD_MAX_ELEM = 21;
    public static final String[] DOWNLOAD_COLUMN_NAMES = {DOWNLOAD_NR, DOWNLOAD_FILM_NR, DOWNLOAD_ABO, DOWNLOAD_SENDER, DOWNLOAD_THEMA, DOWNLOAD_TITEL, DOWNLOAD_PROGRESS,
        DOWNLOAD_DATUM, DOWNLOAD_ZEIT, DOWNLOAD_URL, DOWNLOAD_URL_AUTH, DOWNLOAD_URL_RTMP,
        DOWNLOAD_PROGRAMMSET, DOWNLOAD_PROGRAMM, DOWNLOAD_PROGRAMM_AUFRUF, DOWNLOAD_PROGRAMM_RESTART,
        DOWNLOAD_ZIEL_DATEINAME, DOWNLOAD_ZIEL_PFAD, DOWNLOAD_ZIEL_PFAD_DATEINAME, DOWNLOAD_ART, DOWNLOAD_QUELLE};
    public String[] arr;

    public DatenDownload() {
        makeArr();
    }

    public DatenDownload(DatenPset pSet, DatenFilm film, int quelle, DatenAbo abo, String name, String pfad) {
        makeArr();
        arr[DOWNLOAD_FILM_NR_NR] = film.arr[DatenFilm.FILM_NR_NR];
        arr[DOWNLOAD_SENDER_NR] = film.arr[DatenFilm.FILM_SENDER_NR];
        arr[DOWNLOAD_THEMA_NR] = film.arr[DatenFilm.FILM_THEMA_NR];
        arr[DOWNLOAD_TITEL_NR] = film.arr[DatenFilm.FILM_TITEL_NR];
        arr[DOWNLOAD_URL_NR] = film.arr[DatenFilm.FILM_URL_NR];
        arr[DOWNLOAD_URL_AUTH_NR] = film.arr[DatenFilm.FILM_URL_AUTH_NR];
        arr[DOWNLOAD_DATUM_NR] = film.arr[DatenFilm.FILM_DATUM_NR];
        arr[DOWNLOAD_ZEIT_NR] = film.arr[DatenFilm.FILM_ZEIT_NR];
        arr[DOWNLOAD_URL_RTMP_NR] = film.arr[DatenFilm.FILM_URL_RTMP_NR];
        arr[DOWNLOAD_QUELLE_NR] = String.valueOf(quelle);
        aufrufBauen(pSet, film, abo, name, pfad);
    }

    public void startMelden(int status) {
        arr[DatenDownload.DOWNLOAD_PROGRESS_NR] = String.valueOf(status);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, DatenDownload.class.getName());
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
            return Start.ART_PROGRAMM;
        }
    }

    public int getQuelle() {
        try {
            return Integer.parseInt(arr[DOWNLOAD_QUELLE_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(649632580, this.getClass().getName(), ex);
            return Start.QUELLE_BUTTON;
        }
    }

    public boolean isRestart() {
        if (arr[DOWNLOAD_PROGRAMM_RESTART_NR].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_RESTART_NR]);
    }

    private void aufrufBauen(DatenPset pSet, DatenFilm film, DatenAbo abo, String nname, String ppfad) {
        //zieldatei und pfad bauen und eintragen
        try {
            DatenProg programm = pSet.getProgUrl(arr[DOWNLOAD_URL_NR]);
            // ##############################################
            // für die alten Versionen:
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].replace("%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR].replace("%p", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].replace("%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].replace("%p", "");
            Iterator<DatenProg> it = pSet.getListeProg().iterator();
            while (it.hasNext()) {
                DatenProg prog = it.next();
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%n", "");
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%p", "");
            }
            // ##############################################
            // pSet und ... eintragen
            arr[DOWNLOAD_PROGRAMMSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
            int art = pSet.checkDownloadDirekt(arr[DOWNLOAD_URL_NR]);
            arr[DOWNLOAD_ART_NR] = String.valueOf(art);
            if (art == Start.ART_DOWNLOAD) {
                arr[DatenDownload.DOWNLOAD_PROGRAMM_NR] = Start.ART_DOWNLOAD_TXT;
            } else {
                arr[DatenDownload.DOWNLOAD_PROGRAMM_NR] = programm.arr[DatenProg.PROGRAMM_NAME_NR];
            }
            arr[DOWNLOAD_PROGRAMM_RESTART_NR] = String.valueOf(programm.isRestart());
            dateinamePfadBauen(pSet, film, abo, nname, ppfad);
            programmaufrufBauen(programm);
        } catch (Exception ex) {
            Log.fehlerMeldung(825600145, this.getClass().getName(), ex);
        }
    }

    private void dateinamePfadBauen(DatenPset pSet, DatenFilm film, DatenAbo abo, String nname, String ppfad) {
        String name;
        String pfad;
        if (!pSet.progsContainPath()) {
            // dann können wir uns das sparen
            arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR] = "";
            arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR] = "";
            return;
        }
        // ##############################################
        // Name
        // ##############################################
        if (!nname.equals("")) {
            // wenn vorgegeben, dann den nehmen
            name = nname;
        } else {
            arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR] = pSet.getZielDateiname(arr[DOWNLOAD_URL_NR]);
            name = arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR];
            // ##############################
            // Name sinnvoll belegen
            if (name.equals("")) {
                name = DatumZeit.getHeute_yyyyMMdd() + "_" + arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
            }
            name = GuiFunktionen.replaceString(name, film);
            name = GuiFunktionen.replaceLeerDateiname(name, true/* pfadtrennerEntfernen */, true /* leerEntfernen */);
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
        }
        // ##############################################
        // Pfad
        // ##############################################
        if (!ppfad.equals("")) {
            // wenn vorgegeben, dann den nehmen
            pfad = ppfad;
        } else {
            arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR] = pSet.getZielPfad();
            pfad = arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR];
            // ##############################
            // Pfad sinnvoll belegen
            if (pfad.equals("")) {
                // wenn leer, vorbelegen
                pfad = GuiFunktionen.getStandardDownloadPath();
            }
            if (abo != null) {
                // Bei Abos: den Namen des Abos eintragen
                arr[DatenDownload.DOWNLOAD_ABO_NR] = abo.arr[DatenAbo.ABO_NAME_NR];
                if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR])) {
                    // und Abopfad an den Pfad anhängen
                    pfad = GuiFunktionen.addsPfad(pfad, GuiFunktionen.replaceLeerDateiname(abo.arr[DatenAbo.ABO_ZIELPFAD_NR], true/* pfadtrennerEntfernen */, true /* leerEntfernen */));
                }
            } else if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR])) {
                // bei Downloads den Namen des Themas an den Zielpfad anhängen
                pfad = GuiFunktionen.addsPfad(pfad, GuiFunktionen.replaceLeerDateiname(arr[DatenDownload.DOWNLOAD_THEMA_NR], true/* pfadtrennerEntfernen */, true /* leerEntfernen */));
            }
            pfad = GuiFunktionen.replaceString(pfad, film);
            pfad = GuiFunktionen.replaceLeerDateiname(pfad, false/* pfadtrennerEntfernen */, false /* leerEntfernen */);
        }
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        arr[DOWNLOAD_ZIEL_DATEINAME_NR] = name;
        arr[DOWNLOAD_ZIEL_PFAD_NR] = pfad;
        arr[DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] = GuiFunktionen.addsPfad(pfad, name);
    }

    private void programmaufrufBauen(DatenProg programm) {
        String befehlsString = programm.getProgrammAufruf();
        befehlsString = befehlsString.replace("**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        befehlsString = befehlsString.replace("%f", arr[DOWNLOAD_URL_NR]);
        befehlsString = befehlsString.replace("%F", getUrlFlvstreamer());
        befehlsString = befehlsString.replace("%k", getUrlLow());
        if (befehlsString.contains("%x")) {
            // sonst holt er doch tatsächlich immer erst die asx-datei!
            befehlsString = befehlsString.replace("%x", AsxLesen.lesen(arr[DOWNLOAD_URL_NR]));
        }
        //Auth eintragen
        if (arr[DOWNLOAD_URL_AUTH_NR].equals("")) {
            befehlsString = befehlsString.replace("%a", "");
            befehlsString = befehlsString.replace("%A", "");
        } else {
            befehlsString = befehlsString.replace("%a", arr[DOWNLOAD_URL_AUTH_NR]);
            befehlsString = befehlsString.replace("%A", "-W " + arr[DOWNLOAD_URL_AUTH_NR]);
        }
        if (getArt() == Start.ART_DOWNLOAD) {
            arr[DOWNLOAD_PROGRAMM_AUFRUF_NR] = "";
        } else {
            arr[DOWNLOAD_PROGRAMM_AUFRUF_NR] = befehlsString;
        }
    }

    private String getUrlFlvstreamer() {
        String ret;
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
        String ret = arr[DOWNLOAD_URL_NR];
        if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekSwr.SENDER)) {
            //swr
            ret = arr[DOWNLOAD_URL_NR].replace(".m.mp4", ".l.mp4");
        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(Mediathek3Sat.SENDER)) {
            //3Sat
            //ret = arr[DOWNLOAD_URL_NR].replace("/veryhigh/", "/300/");
            ret = arr[DOWNLOAD_URL_NR].replace("vh.mp4", "h.mp4");
        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekZdf.SENDER)) {
            //ZDF
            //ret = arr[DOWNLOAD_URL_NR].replace("/veryhigh/", "/300/");
            ret = arr[DOWNLOAD_URL_NR].replace("vh.mp4", "h.mp4");
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
        arr[DOWNLOAD_PROGRESS_NR] = String.valueOf(PROGRESS_NICHT_GESTARTET);
    }
}
