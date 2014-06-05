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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.Log;
import mediathek.controller.MVUsedUrl;
import mediathek.controller.starter.Start;
import mediathek.tool.AsxLesen;
import mediathek.tool.Datum;
import mediathek.tool.DatumZeit;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVFilmSize;
import msearch.daten.DatenFilm;

public class DatenDownload implements Comparable<DatenDownload> {
    //

    private static GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static SimpleDateFormat sdf_datum_zeit = new SimpleDateFormat("dd.MM.yyyyHH:mm:ss");
    private static SimpleDateFormat sdf_datum = new SimpleDateFormat("dd.MM.yyyy");
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
    public static final String DOWNLOAD_BUTTON_START = "";
    public static final int DOWNLOAD_BUTTON_START_NR = 6;
    public static final String DOWNLOAD_BUTTON_DEL = "";
    public static final int DOWNLOAD_BUTTON_DEL_NR = 7;
    public static final String DOWNLOAD_PROGRESS = "Fortschritt";
    public static final int DOWNLOAD_PROGRESS_NR = 8;
    public static final String DOWNLOAD_RESTZEIT = "Restzeit";
    public static final int DOWNLOAD_RESTZEIT_NR = 9;
    public static final String DOWNLOAD_BANDBREITE = "Geschwindigkeit";
    public static final int DOWNLOAD_BANDBREITE_NR = 10;
    public static final String DOWNLOAD_GROESSE = "Größe [MB]";
    public static final int DOWNLOAD_GROESSE_NR = 11;
    public static final String DOWNLOAD_DATUM = "Datum";
    public static final int DOWNLOAD_DATUM_NR = 12;
    public static final String DOWNLOAD_ZEIT = "Zeit";
    public static final int DOWNLOAD_ZEIT_NR = 13;
    public static final String DOWNLOAD_DAUER = "Dauer";
    public static final int DOWNLOAD_DAUER_NR = 14;
    public static final String DOWNLOAD_UNTERBROCHEN = "Pause";
    public static final int DOWNLOAD_UNTERBROCHEN_NR = 15;
    public static final String DOWNLOAD_GEO = "Geo";
    public static final int DOWNLOAD_GEO_NR = 16;
    public static final String DOWNLOAD_FILM_URL = "Film-URL";
    public static final int DOWNLOAD_FILM_URL_NR = 17;
    public static final String DOWNLOAD_HISTORY_URL = "History-URL";
    public static final int DOWNLOAD_HISTORY_URL_NR = 18;
    public static final String DOWNLOAD_URL = "URL";
    public static final int DOWNLOAD_URL_NR = 19;
    public static final String DOWNLOAD_URL_RTMP = "URL-rtmp";
    public static final int DOWNLOAD_URL_RTMP_NR = 20;
    public static final String DOWNLOAD_URL_AUTH = "URL-Auth";
    public static final int DOWNLOAD_URL_AUTH_NR = 21;
    public static final String DOWNLOAD_PROGRAMMSET = "Programmset";
    public static final int DOWNLOAD_PROGRAMMSET_NR = 22;
    public static final String DOWNLOAD_PROGRAMM = "Programm";
    public static final int DOWNLOAD_PROGRAMM_NR = 23;
    public static final String DOWNLOAD_PROGRAMM_AUFRUF = "Programmaufruf";
    public static final int DOWNLOAD_PROGRAMM_AUFRUF_NR = 24;
    public static final String DOWNLOAD_PROGRAMM_RESTART = "Restart";
    public static final int DOWNLOAD_PROGRAMM_RESTART_NR = 25;
    public static final String DOWNLOAD_ZIEL_DATEINAME = "Dateiname";
    public static final int DOWNLOAD_ZIEL_DATEINAME_NR = 26;
    public static final String DOWNLOAD_ZIEL_PFAD = "Pfad";
    public static final int DOWNLOAD_ZIEL_PFAD_NR = 27;
    public static final String DOWNLOAD_ZIEL_PFAD_DATEINAME = "Pfad-Dateiname";
    public static final int DOWNLOAD_ZIEL_PFAD_DATEINAME_NR = 28;
    public static final String DOWNLOAD_ART = "Art"; //Art des Downloads: direkter Dateidownload oder über ein Programm
    public static final int DOWNLOAD_ART_NR = 29;
    public static final String DOWNLOAD_QUELLE = "Quelle"; //Quelle: gestartet über einen Button, Download, Abo
    public static final int DOWNLOAD_QUELLE_NR = 30;
    public static final String DOWNLOAD_ZURUECKGESTELLT = "Zurueckgestellt";
    public static final int DOWNLOAD_ZURUECKGESTELLT_NR = 31;
    public static final String DOWNLOAD_INFODATEI = "Infodatei";
    public static final int DOWNLOAD_INFODATEI_NR = 32;
    public static final String DOWNLOAD_REF = "Ref";
    public static final int DOWNLOAD_REF_NR = 33;
    //
    public static final String DOWNLOAD = "Downlad";
    public static final int MAX_ELEM = 34;
    public static final String[] COLUMN_NAMES = {DOWNLOAD_NR, DOWNLOAD_FILM_NR, DOWNLOAD_ABO, DOWNLOAD_SENDER, DOWNLOAD_THEMA, DOWNLOAD_TITEL,
        DOWNLOAD_BUTTON_START, DOWNLOAD_BUTTON_DEL,
        DOWNLOAD_PROGRESS, DOWNLOAD_RESTZEIT, DOWNLOAD_BANDBREITE, DOWNLOAD_GROESSE,
        DOWNLOAD_DATUM, DOWNLOAD_ZEIT, DOWNLOAD_DAUER, DOWNLOAD_UNTERBROCHEN, DOWNLOAD_GEO,
        DOWNLOAD_FILM_URL, DOWNLOAD_HISTORY_URL, DOWNLOAD_URL, DOWNLOAD_URL_RTMP, DOWNLOAD_URL_AUTH,
        DOWNLOAD_PROGRAMMSET, DOWNLOAD_PROGRAMM, DOWNLOAD_PROGRAMM_AUFRUF, DOWNLOAD_PROGRAMM_RESTART,
        DOWNLOAD_ZIEL_DATEINAME, DOWNLOAD_ZIEL_PFAD, DOWNLOAD_ZIEL_PFAD_DATEINAME, DOWNLOAD_ART, DOWNLOAD_QUELLE,
        DOWNLOAD_ZURUECKGESTELLT, DOWNLOAD_INFODATEI, DOWNLOAD_REF};
    public static final String[] COLUMN_NAMES_ = {DOWNLOAD_NR, DOWNLOAD_FILM_NR, DOWNLOAD_ABO, DOWNLOAD_SENDER, DOWNLOAD_THEMA, DOWNLOAD_TITEL,
        "Button-Start"/*DOWNLOAD_BUTTON_START*/, "Button-Del"/*DOWNLOAD_BUTTON_DEL*/,
        DOWNLOAD_PROGRESS, DOWNLOAD_RESTZEIT, DOWNLOAD_BANDBREITE, "Groesse"/*DOWNLOAD_GROESSE*/,
        DOWNLOAD_DATUM, DOWNLOAD_ZEIT, DOWNLOAD_DAUER, DOWNLOAD_UNTERBROCHEN, DOWNLOAD_GEO,
        DOWNLOAD_FILM_URL, DOWNLOAD_HISTORY_URL, DOWNLOAD_URL, DOWNLOAD_URL_RTMP, DOWNLOAD_URL_AUTH,
        DOWNLOAD_PROGRAMMSET, DOWNLOAD_PROGRAMM, DOWNLOAD_PROGRAMM_AUFRUF, DOWNLOAD_PROGRAMM_RESTART,
        DOWNLOAD_ZIEL_DATEINAME, DOWNLOAD_ZIEL_PFAD, DOWNLOAD_ZIEL_PFAD_DATEINAME, DOWNLOAD_ART, DOWNLOAD_QUELLE,
        DOWNLOAD_ZURUECKGESTELLT, DOWNLOAD_INFODATEI, DOWNLOAD_REF};
    public Datum datumFilm = new Datum(0);
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public String[] arr;
    public DatenFilm film = null;
    public MVFilmSize mVFilmSize = new MVFilmSize();
    public Start start = null;
    public int nr = 0;

    public DatenDownload() {
        makeArr();
    }

    public DatenDownload(DatenPset pSet, DatenFilm ffilm, int quelle, DatenAbo abo, String name, String pfad, String aufloesung) {
        makeArr();
        arr[DOWNLOAD_FILM_NR_NR] = ffilm.arr[DatenFilm.FILM_NR_NR];
        arr[DOWNLOAD_SENDER_NR] = ffilm.arr[DatenFilm.FILM_SENDER_NR];
        arr[DOWNLOAD_THEMA_NR] = ffilm.arr[DatenFilm.FILM_THEMA_NR];
        arr[DOWNLOAD_TITEL_NR] = ffilm.arr[DatenFilm.FILM_TITEL_NR];
        arr[DOWNLOAD_FILM_URL_NR] = ffilm.arr[DatenFilm.FILM_URL_NR];
        arr[DOWNLOAD_URL_AUTH_NR] = ffilm.arr[DatenFilm.FILM_URL_AUTH_NR];
        arr[DOWNLOAD_DATUM_NR] = ffilm.arr[DatenFilm.FILM_DATUM_NR];
        arr[DOWNLOAD_ZEIT_NR] = ffilm.arr[DatenFilm.FILM_ZEIT_NR];
        arr[DOWNLOAD_URL_RTMP_NR] = ffilm.arr[DatenFilm.FILM_URL_RTMP_NR];
        arr[DOWNLOAD_DAUER_NR] = ffilm.arr[DatenFilm.FILM_DAUER_NR];
        arr[DOWNLOAD_QUELLE_NR] = String.valueOf(quelle);
        arr[DOWNLOAD_HISTORY_URL_NR] = ffilm.getUrlHistory();
        if (aufloesung.isEmpty()) {
            arr[DOWNLOAD_URL_NR] = ffilm.getUrlFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR]);
            arr[DOWNLOAD_URL_RTMP_NR] = ffilm.getUrlRtmpFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR]);
        } else {
            arr[DOWNLOAD_URL_NR] = ffilm.getUrlFuerAufloesung(aufloesung);
            arr[DOWNLOAD_URL_RTMP_NR] = ffilm.getUrlRtmpFuerAufloesung(aufloesung);
        }
        arr[DatenDownload.DOWNLOAD_INFODATEI_NR] = pSet.arr[DatenPset.PROGRAMMSET_INFODATEI_NR];
        arr[DatenDownload.DOWNLOAD_GEO_NR] = ffilm.arr[DatenFilm.FILM_GEO_NR];
        // und jetzt noch die Dateigröße für die entsp. URL
        film = ffilm;
        if (film.arr[DatenFilm.FILM_URL_NR].equals(arr[DOWNLOAD_URL_NR])) {
            mVFilmSize.setSize(film.arr[DatenFilm.FILM_GROESSE_NR]);
        } else {
            mVFilmSize.setSize("");
        }
        aufrufBauen(pSet, ffilm, abo, name, pfad);
        init();
    }

    public void setGroesse(String groesse) {
        if (!groesse.isEmpty()) {
            mVFilmSize.setSize(groesse);
        } else {
            mVFilmSize.setSize(film.getDateigroesse(arr[DOWNLOAD_URL_NR]));
        }
    }

    public static boolean anzeigen(int i) {
        if (spaltenAnzeigen == null) {
            return true;
        } else {
            return spaltenAnzeigen[i];
        }
    }

    public final void init() {
        // Datum
        datumFilm = getDatumForObject();
    }

    public boolean istZurueckgestellt() {
        return arr[DOWNLOAD_ZURUECKGESTELLT_NR].equals(Boolean.TRUE.toString());
    }

    public void zurueckstellen() {
        if (start != null) {
            if (start.status > Start.STATUS_INIT) {
                // zu spät
                return;
            }
        }
        arr[DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.TRUE.toString();
        resetDownload();
    }

    public boolean interrupted() {
        if (istFertig()) {
            // fertige sind nicht mehr unterbrochen
            return false;
        }
        return arr[DOWNLOAD_UNTERBROCHEN_NR].equals(Boolean.TRUE.toString());
    }

    public void interrupt() {
        arr[DOWNLOAD_UNTERBROCHEN_NR] = Boolean.TRUE.toString();
    }

    public void interruptRestart() {
        arr[DOWNLOAD_UNTERBROCHEN_NR] = Boolean.FALSE.toString();
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_RESET_INTERRUPT, DatenDownload.class.getName());
    }

    public boolean istFertig() {
        if (start != null) {
            if (start.status == Start.STATUS_FERTIG) {
                return true;
            }
        }
        return false;
    }

    public void resetDownload() {
        mVFilmSize.reset();
        start = null;
    }

    public void startenDownload(Daten ddaten) {
        // Start erstellen und zur Liste hinzufügen
        this.start = new Start();
        // gestartete Filme (originalURL des Films) auch in die History eintragen
        if (film != null) {
            Daten.listeFilmeHistory.add(film);
        }
        ddaten.history.zeileSchreiben(arr[DatenDownload.DOWNLOAD_THEMA_NR], arr[DatenDownload.DOWNLOAD_TITEL_NR], arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
    }

    public static void startenDownloads(Daten ddaten, ArrayList<DatenDownload> ad) {
        // Start erstellen und zur Liste hinzufügen
        String zeit = DatumZeit.getHeute_dd_MM_yyyy();
        LinkedList<MVUsedUrl> urlList = new LinkedList<>();
        for (DatenDownload d : ad) {
            d.start = new Start();
            urlList.add(new MVUsedUrl(zeit,
                    d.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                    d.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                    d.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]));
        }
        if (!urlList.isEmpty()) {
            ddaten.history.zeilenSchreiben(urlList);
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, DatenDownload.class.getSimpleName());
    }

    public DatenDownload getCopy() {
        DatenDownload ret = new DatenDownload();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.datumFilm = this.datumFilm;
        return ret;
    }

    public void aufMichKopieren(DatenDownload datenDownload) {
        System.arraycopy(datenDownload.arr, 0, arr, 0, arr.length);
    }

    public boolean istAbo() {
        return !arr[DatenDownload.DOWNLOAD_ABO_NR].equals("");
    }

    public int getArt() {
        try {
            return Integer.parseInt(arr[DOWNLOAD_ART_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(946325800, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
            return Start.ART_PROGRAMM;
        }
    }

    public int getQuelle() {
        try {
            return Integer.parseInt(arr[DOWNLOAD_QUELLE_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(649632580, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
            return Start.QUELLE_BUTTON;
        }
    }

    public boolean isRestart() {
        if (arr[DOWNLOAD_PROGRAMM_RESTART_NR].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_RESTART_NR]);
    }

    public String getTextRestzeit() {
        if (start != null) {
            if (start.status < Start.STATUS_FERTIG && start.status >= Start.STATUS_RUN && start.restSekunden > 0) {
                if (start.restSekunden < 60) {
                    return "< 1 Min.";
                } else {
                    return Long.toString(start.restSekunden / 60) + " Min.";
                }
            }
        }
        return "";
    }

    public String getTextBandbreite() {
        if (start != null) {
            if (start.status < Start.STATUS_FERTIG && start.status >= Start.STATUS_RUN) {
                if (start.bandbreite > 1000 * 1000) {
                    String s = String.valueOf(start.bandbreite / 1000);
                    if (s.length() >= 4) {
                        s = s.substring(0, s.length() - 3) + "," + s.substring(s.length() - 3);
                    }
                    return s + " MB/s";
                    //return String.valueOf(start.bandbreite / (1000 * 1000)) + "," + (start.bandbreite / 1000 % 1000) + " MB/s";
                } else if (start.bandbreite > 1000) {
                    return (start.bandbreite / 1000) + " kB/s";
                } else if (start.bandbreite > 1) {
                    return start.bandbreite + " B/s";
                } else {
                    return "";
                }
            }
        }
        return "";
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
            for (DatenProg prog : pSet.getListeProg()) {
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%n", "");
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].replace("%p", "");
            }
            // ##############################################
            // pSet und ... eintragen
            arr[DOWNLOAD_PROGRAMMSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
            // Direkter Download nur wenn url passt und wenn im Programm ein Zielpfad ist sonst Abspielen
            int art = (pSet.checkDownloadDirekt(arr[DOWNLOAD_URL_NR]) && pSet.progsContainPath()) ? Start.ART_DOWNLOAD : Start.ART_PROGRAMM;
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
            Log.fehlerMeldung(825600145, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
    }

    private void dateinamePfadBauen(DatenPset pSet, DatenFilm film, DatenAbo abo, String nname, String ppfad) {
        // nname und ppfad sind nur belegt, wenn der Download über den DialogAddDownload gestartet wurde (aus TabFilme)
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
                name = DatumZeit.Heute_yyyyMMdd + "_" + arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
            }
            name = replaceString(name, film); // %D ... ersetzen
            name = GuiFunktionen.replaceLeerDateiname(name, true/* istDatei */);
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
                    pfad = GuiFunktionen.addsPfad(pfad, GuiFunktionen.replaceLeerDateiname(abo.arr[DatenAbo.ABO_ZIELPFAD_NR], true/* istDatei */));
                }
            } else if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR])) {
                // bei Downloads den Namen des Themas an den Zielpfad anhängen
                pfad = GuiFunktionen.addsPfad(pfad, GuiFunktionen.replaceLeerDateiname(arr[DatenDownload.DOWNLOAD_THEMA_NR], true/* istDatei */));
            }
            pfad = replaceString(pfad, film); // %D ... ersetzen
            // der vorgegebenen Pfad des Sets wird so genommen wie er ist
            //pfad = GuiFunktionen.replaceLeerDateiname(pfad, false/* istDatei */, false /* leerEntfernen */); 
        }
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        //###########################################################
        // zur Sicherheit bei Unsinn im Set
        if (pfad.equals("")) {
            pfad = GuiFunktionen.getStandardDownloadPath();
        }
        if (name.equals("")) {
            name = DatumZeit.Heute_yyyyMMdd + "_" + arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
        }

        arr[DOWNLOAD_ZIEL_DATEINAME_NR] = name;
        arr[DOWNLOAD_ZIEL_PFAD_NR] = pfad;
        arr[DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] = GuiFunktionen.addsPfad(pfad, name);
    }

    private void programmaufrufBauen(DatenProg programm) {
        String befehlsString = programm.getProgrammAufruf();
        befehlsString = befehlsString.replace("**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        befehlsString = befehlsString.replace("%f", arr[DOWNLOAD_URL_NR]);
        befehlsString = befehlsString.replace("%F", arr[DOWNLOAD_URL_RTMP_NR]);
        befehlsString = befehlsString.replace("%k", arr[DOWNLOAD_URL_NR]); // ehemals kleine Auflösung
        befehlsString = befehlsString.replace("%K", arr[DOWNLOAD_URL_RTMP_NR]); // ehemals kleine Auflösung
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

    private String replaceString(String s, DatenFilm film) {
        s = s.replace("%D", film.arr[DatenFilm.FILM_DATUM_NR].equals("") ? DatumZeit.Heute_yyyyMMdd : datumDatumZeitReinigen(datumDrehen(film.arr[DatenFilm.FILM_DATUM_NR])));
        s = s.replace("%d", film.arr[DatenFilm.FILM_ZEIT_NR].equals("") ? DatumZeit.Jetzt_HHMMSS : datumDatumZeitReinigen(film.arr[DatenFilm.FILM_ZEIT_NR]));
        s = s.replace("%t", film.arr[DatenFilm.FILM_THEMA_NR]);
        s = s.replace("%T", film.arr[DatenFilm.FILM_TITEL_NR]);
        s = s.replace("%s", film.arr[DatenFilm.FILM_SENDER_NR]);

        s = s.replace("%H", DatumZeit.Heute_yyyyMMdd);
        s = s.replace("%h", DatumZeit.Jetzt_HHMMSS);

        s = s.replace("%N", GuiFunktionen.getDateiName(this.arr[DatenDownload.DOWNLOAD_URL_NR]));
        s = s.replace("%S", GuiFunktionen.getDateiSuffix(this.arr[DatenDownload.DOWNLOAD_URL_NR]));
        return s;
    }

    private static String datumDrehen(String datum) {
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
                Log.fehlerMeldung(775421006, Log.FEHLER_ART_PROG, "DatenFilm.datumDrehen", ex, datum);
            }

        }
        return ret;
    }

    private static String datumDatumZeitReinigen(String datum) {
        String ret;
        ret = datum;
        ret = ret.replace(":", "");
        ret = ret.replace(".", "");
        return ret;
    }

    @Override
    public int compareTo(DatenDownload arg0) {
        int ret;
        if ((ret = sorter.compare(arr[DatenDownload.DOWNLOAD_SENDER_NR], arg0.arr[DatenDownload.DOWNLOAD_SENDER_NR])) == 0) {
            return sorter.compare(arr[DatenDownload.DOWNLOAD_THEMA_NR], arg0.arr[DatenDownload.DOWNLOAD_THEMA_NR]);
        }
        return ret;
    }

    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        arr[DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.FALSE.toString();
        arr[DOWNLOAD_UNTERBROCHEN_NR] = Boolean.FALSE.toString();
    }

    public Datum getDatumForObject() {
        Datum tmp = new Datum(0);
        if (!arr[DatenDownload.DOWNLOAD_DATUM_NR].equals("")) {
            try {
                if (!arr[DatenDownload.DOWNLOAD_ZEIT_NR].equals("")) {
                    tmp.setTime(sdf_datum_zeit.parse(arr[DatenDownload.DOWNLOAD_DATUM_NR] + arr[DatenDownload.DOWNLOAD_ZEIT_NR]).getTime());
                } else {
                    tmp.setTime(sdf_datum.parse(arr[DatenDownload.DOWNLOAD_DATUM_NR]).getTime());
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(649897321, Log.FEHLER_ART_PROG, "DatumZeit.getDatumForObject", ex,
                        new String[]{"Datum: " + arr[DatenDownload.DOWNLOAD_DATUM_NR], "Zeit: " + arr[DatenDownload.DOWNLOAD_ZEIT_NR]});
            }
        }
        return tmp;
    }
}
