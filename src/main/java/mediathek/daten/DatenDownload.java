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

import mSearch.daten.DatenFilm;
import mSearch.tool.*;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.MVUsedUrl;
import mediathek.controller.starter.Start;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVFilmSize;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;

public final class DatenDownload extends MVData<DatenDownload> {

    // Quelle - start über einen Button - Download - Abo
    public static final byte QUELLE_ALLE = -1;
    public static final byte QUELLE_BUTTON = 1;
    public static final byte QUELLE_DOWNLOAD = 2;
    public static final byte QUELLE_ABO = 3;
    public static final String QUELLE_ALLE_TXT = "Alle";
    public static final String QUELLE_BUTTON_TXT = "Button";
    public static final String QUELLE_DOWNLOAD_TXT = "Download";
    public static final String QUELLE_ABO_TXT = "Abo";

    // Art: direkter Download: http... oder über ein externes Programm: rtmp...
    public static final byte ART_DOWNLOAD = 1; // direkter Download
    public static final byte ART_PROGRAMM = 2; // Download über ein Programm
    public static final String ART_DOWNLOAD_TXT = "direkter Download";
    public static final String ART_PROGRAMM_TXT = "Programm";

    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final SimpleDateFormat sdf_datum_zeit = new SimpleDateFormat("dd.MM.yyyyHH:mm:ss");
    private static final SimpleDateFormat sdf_datum = new SimpleDateFormat("dd.MM.yyyy");

    public static final int DOWNLOAD_NR = 0;
    public static final int DOWNLOAD_FILM_NR = 1;// nur ein Platzhalter für: "film.nr"
    public static final int DOWNLOAD_ABO = 2; // wenn das Feld gefüllt ist, ist der Download ein Abo
    public static final int DOWNLOAD_SENDER = 3;
    public static final int DOWNLOAD_THEMA = 4;
    public static final int DOWNLOAD_TITEL = 5;
    public static final int DOWNLOAD_BUTTON_START = 6;
    public static final int DOWNLOAD_BUTTON_DEL = 7;
    public static final int DOWNLOAD_PROGRESS = 8;
    public static final int DOWNLOAD_RESTZEIT = 9;
    public static final int DOWNLOAD_BANDBREITE = 10;
    public static final int DOWNLOAD_GROESSE = 11;
    public static final int DOWNLOAD_DATUM = 12;
    public static final int DOWNLOAD_ZEIT = 13;
    public static final int DOWNLOAD_DAUER = 14;

    public static final int DOWNLOAD_HD = 15;
    public static final int DOWNLOAD_UT = 16;

    public static final int DOWNLOAD_UNTERBROCHEN = 17;
    public static final int DOWNLOAD_GEO = 18;
    public static final int DOWNLOAD_FILM_URL = 19;
    public static final int DOWNLOAD_HISTORY_URL = 20;
    public static final int DOWNLOAD_URL = 21;
    public static final int DOWNLOAD_URL_RTMP = 22;
    public static final int DOWNLOAD_URL_SUBTITLE = 23;
    public static final int DOWNLOAD_PROGRAMMSET = 24;
    public static final int DOWNLOAD_PROGRAMM = 25;
    public static final int DOWNLOAD_PROGRAMM_AUFRUF = 26;
    public static final int DOWNLOAD_PROGRAMM_AUFRUF_ARRAY = 27;
    public static final int DOWNLOAD_PROGRAMM_RESTART = 28;
    public static final int DOWNLOAD_ZIEL_DATEINAME = 29;
    public static final int DOWNLOAD_ZIEL_PFAD = 30;
    public static final int DOWNLOAD_ZIEL_PFAD_DATEINAME = 31;
    public static final int DOWNLOAD_ART = 32;//Art des Downloads: direkter Dateidownload oder über ein Programm
    public static final int DOWNLOAD_QUELLE = 33; //Quelle: gestartet über einen Button, Download, Abo
    public static final int DOWNLOAD_ZURUECKGESTELLT = 34;
    public static final int DOWNLOAD_INFODATEI = 35;
    public static final int DOWNLOAD_SPOTLIGHT = 36;
    public static final int DOWNLOAD_SUBTITLE = 37;// Untertitel anlegen ja/nein
    public static final int DOWNLOAD_PROGRAMM_DOWNLOADMANAGER = 38;
    public static final int DOWNLOAD_REF = 39;
    //
    public static final String TAG = "Downlad";
    public static final int MAX_ELEM = 40;
    public static final String[] COLUMN_NAMES = {"Nr", "Filmnr", "Abo", "Sender", "Thema", "Titel", "", "",
        "Fortschritt", "Restzeit", "Geschwindigkeit", "Größe [MB]",
        "Datum", "Zeit", "Dauer", "HD", "UT",
        "Pause", "Geo", "Url Film", "Url History", "Url", "Url RTMP", "Url Untertitel",
        "Programmset", "Programm", "Programmaufruf", "Programmaufruf Array", "Restart",
        "Dateiname", "Pfad", "Pfad-Dateiname", "Art", "Quelle",
        "Zurückgestellt", "Infodatei", "Spotlight", "Untertitel", "Remote Download", "Ref"};
    public static final String[] XML_NAMES = {"Nr", "Filmnr", "Abo", "Sender", "Thema", "Titel", "Button-Start", "Button-Del",
        "Fortschritt", "Restzeit", "Geschwindigkeit", "Groesse"/*DOWNLOAD_GROESSE*/,
        "Datum", "Zeit", "Dauer", "HD", "UT",
        "Pause", "Geo", "Film-URL", "History-URL", "URL", "URL-rtmp", "URL-Untertitel",
        "Programmset", "Programm", "Programmaufruf_", "Programmaufruf", "Restart",
        "Dateiname", "Pfad", "Pfad-Dateiname", "Art", "Quelle",
        "Zurueckgestellt", "Infodatei", "Spotlight", "Untertitel", "Remote-Download", "Ref"};
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public String[] arr;

    public Datum datumFilm = new Datum(0);
    public DatenFilm film = null;
    public MVFilmSize mVFilmSize = new MVFilmSize();
    public Start start = null;
    public DatenPset pSet = null;
    public DatenAbo abo = null;
    public int nr = 0;
    public byte quelle = QUELLE_ALLE;
    public byte art = ART_DOWNLOAD;

    public DatenDownload() {
        makeArr();
    }

    public DatenDownload(DatenPset pSet, DatenFilm film, byte quelle, DatenAbo abo, String name, String pfad, String aufloesung) {
        makeArr();
        this.film = film;
        this.pSet = pSet;
        this.abo = abo;
        arr[DOWNLOAD_FILM_NR] = film.arr[DatenFilm.FILM_NR];
        arr[DOWNLOAD_SENDER] = film.arr[DatenFilm.FILM_SENDER];
        arr[DOWNLOAD_THEMA] = film.arr[DatenFilm.FILM_THEMA];
        arr[DOWNLOAD_TITEL] = film.arr[DatenFilm.FILM_TITEL];
        arr[DOWNLOAD_FILM_URL] = film.arr[DatenFilm.FILM_URL];
        arr[DOWNLOAD_URL_SUBTITLE] = film.getUrlSubtitle();
        arr[DOWNLOAD_DATUM] = film.arr[DatenFilm.FILM_DATUM];
        arr[DOWNLOAD_ZEIT] = film.arr[DatenFilm.FILM_ZEIT];
        arr[DOWNLOAD_URL_RTMP] = film.arr[DatenFilm.FILM_URL_RTMP];
        arr[DOWNLOAD_DAUER] = film.arr[DatenFilm.FILM_DAUER];
        arr[DOWNLOAD_HD] = film.isHD() ? "1" : "0";
        arr[DOWNLOAD_UT] = film.hasUT() ? "1" : "0";
        arr[DOWNLOAD_QUELLE] = String.valueOf(quelle);
        arr[DOWNLOAD_HISTORY_URL] = film.getUrlHistory();
        if (aufloesung.isEmpty()) {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]);
            arr[DOWNLOAD_URL_RTMP] = film.getUrlRtmpFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]);
        } else {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(aufloesung);
            arr[DOWNLOAD_URL_RTMP] = film.getUrlRtmpFuerAufloesung(aufloesung);
        }
        arr[DatenDownload.DOWNLOAD_INFODATEI] = pSet.arr[DatenPset.PROGRAMMSET_INFODATEI];
        arr[DatenDownload.DOWNLOAD_SUBTITLE] = pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE];
        arr[DatenDownload.DOWNLOAD_SPOTLIGHT] = pSet.arr[DatenPset.PROGRAMMSET_SPOTLIGHT];
        arr[DatenDownload.DOWNLOAD_GEO] = film.arr[DatenFilm.FILM_GEO];
        // und jetzt noch die Dateigröße für die entsp. URL
        setGroesseFromFilm();
        //setGroesse(""); //dann dauert das Starten uu sehr lange

        aufrufBauen(pSet, film, abo, name, pfad);
        init();
    }

    public void setGroesseFromFilm() {
        if (film != null) {
            if (film.arr[DatenFilm.FILM_URL].equals(arr[DOWNLOAD_URL])) {
                mVFilmSize.setSize(film.arr[DatenFilm.FILM_GROESSE]);
            } else {
                mVFilmSize.setSize("");
            }
        }
    }

    public void setGroesse(String groesse) {
        if (film != null) {
            if (!groesse.isEmpty()) {
                mVFilmSize.setSize(groesse);
            } else {
                mVFilmSize.setSize(film.getDateigroesse(arr[DOWNLOAD_URL]));
            }
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
        datumFilm = getDatumForObject();
        try {
            art = Byte.parseByte(arr[DOWNLOAD_ART]);
            quelle = Byte.parseByte(arr[DOWNLOAD_QUELLE]);
        } catch (Exception ex) {
            Log.errorLog(649632580, ex, "Art: " + arr[DOWNLOAD_ART] + " Quelle: " + arr[DOWNLOAD_QUELLE]);
            art = ART_PROGRAMM;
            quelle = QUELLE_BUTTON;
        }
        if (film == null) {
            //damit die Sortierunt bei gespeicherten Downloads bei denen der 
            //Film nicht mehr ermittelt werden kann stimmt
            arr[DOWNLOAD_HD] = "0";
            arr[DOWNLOAD_UT] = "0";
        }
    }

    public boolean istZurueckgestellt() {
        return arr[DOWNLOAD_ZURUECKGESTELLT].equals(Boolean.TRUE.toString());
    }

    public void zurueckstellen() {
        if (start != null) {
            if (start.status > Start.STATUS_INIT) {
                // zu spät
                return;
            }
        }
        arr[DOWNLOAD_ZURUECKGESTELLT] = Boolean.TRUE.toString();
        resetDownload();
    }

    public boolean isInterrupted() {
        return !isFinished() && arr[DOWNLOAD_UNTERBROCHEN].equals(Boolean.TRUE.toString());
    }

    public void interrupt() {
        arr[DOWNLOAD_UNTERBROCHEN] = Boolean.TRUE.toString();
    }

    public void interruptRestart() {
        arr[DOWNLOAD_UNTERBROCHEN] = Boolean.FALSE.toString();
        Listener.notify(Listener.EREIGNIS_RESET_INTERRUPT, DatenDownload.class.getName());
    }

    public boolean notStarted() {
        return (start == null);
    }

    public boolean isWaiting() {
        return (start != null) && (start.status == Start.STATUS_INIT);
    }

    public boolean isFinished() {
        return (start != null) && (start.status == Start.STATUS_FERTIG);
    }

    public boolean runNotFinished() {
        if (start != null && start.status < Start.STATUS_FERTIG) {
            return true;
        }
        return false;
    }

    public boolean running() {
        if (start != null && start.status == Start.STATUS_RUN) {
            return true;
        }
        return false;
    }

    public void resetDownload() {
        mVFilmSize.reset();
        start = null;
    }

    public void startDownload(Daten aDaten) {
        // Start erstellen und zur Liste hinzufügen
        this.start = new Start();
        // gestartete Filme (originalURL des Films) auch in die History eintragen
        if (film != null) {
            aDaten.getListeFilmeHistory().add(film);
        }
        aDaten.history.zeileSchreiben(arr[DatenDownload.DOWNLOAD_THEMA], arr[DatenDownload.DOWNLOAD_TITEL], arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
        Listener.notify(Listener.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
    }

    public static void startenDownloads(Daten ddaten, ArrayList<DatenDownload> downloads) {
        // Start erstellen und zur Liste hinzufügen
        String zeit = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
        LinkedList<MVUsedUrl> urlList = new LinkedList<>();
        for (DatenDownload d : downloads) {
            d.start = new Start();
            urlList.add(new MVUsedUrl(zeit,
                    d.arr[DatenDownload.DOWNLOAD_THEMA],
                    d.arr[DatenDownload.DOWNLOAD_TITEL],
                    d.arr[DatenDownload.DOWNLOAD_HISTORY_URL]));
        }
        if (!urlList.isEmpty()) {
            ddaten.history.zeilenSchreiben(urlList);
        }
        Listener.notify(Listener.EREIGNIS_START_EVENT, DatenDownload.class.getSimpleName());
    }

    public DatenDownload getCopy() {
        DatenDownload ret = new DatenDownload();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.datumFilm = this.datumFilm;
        ret.film = this.film;
        ret.mVFilmSize = this.mVFilmSize;
        ret.start = this.start;
        ret.pSet = this.pSet;
        ret.abo = this.abo;
        ret.nr = this.nr;
        ret.quelle = this.quelle;
        ret.art = this.art;
        return ret;
    }

    public void aufMichKopieren(DatenDownload datenDownload) {
        System.arraycopy(datenDownload.arr, 0, arr, 0, arr.length);
        datumFilm = datenDownload.datumFilm;
        film = datenDownload.film;
        mVFilmSize = datenDownload.mVFilmSize;// die Auflösung des Films kann sich ändern
        start = datenDownload.start;
        pSet = datenDownload.pSet;
        abo = datenDownload.abo;
        nr = datenDownload.nr;
        quelle = datenDownload.quelle;
        art = datenDownload.art;
    }

    public boolean istAbo() {
        return !arr[DatenDownload.DOWNLOAD_ABO].isEmpty();
    }

    public boolean isRestart() {
        if (arr[DOWNLOAD_PROGRAMM_RESTART].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_RESTART]);
    }

    public boolean isDownloadManager() {
        if (arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER]);
    }

    public boolean isInfoFile() {
        if (arr[DOWNLOAD_INFODATEI].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_INFODATEI]);
    }

    public boolean isSubtitle() {
        if (arr[DOWNLOAD_SUBTITLE].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_SUBTITLE]);
    }

    public boolean isSpotlight() {
        if (arr[DOWNLOAD_SPOTLIGHT].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_SPOTLIGHT]);
    }

    public String getTextRestzeit() {
        if (start != null) {
            if (start.status < Start.STATUS_FERTIG && start.status >= Start.STATUS_RUN && start.restSekunden > 0) {

                if (start.restSekunden > 300) {
                    return Long.toString(Math.round(start.restSekunden / 60.0)) + " Min.";
                } else if (start.restSekunden > 230) {
                    return "5 Min.";
                } else if (start.restSekunden > 170) {
                    return "4 Min.";
                } else if (start.restSekunden > 110) {
                    return "3 Min.";
                } else if (start.restSekunden > 60) {
                    return "2 Min.";
                } else if (start.restSekunden > 30) {
                    return "1 Min.";
                } else if (start.restSekunden > 20) {
                    return "30 s";
                } else if (start.restSekunden > 10) {
                    return "20 s";
                } else {
                    return "10 s";
                }
            }
        }
        return "";
    }

    public String getTextBandbreite() {
        // start.bandbreite -->> bytes per second
        if (start != null) {
            if (/*start.status < Start.STATUS_FERTIG &&*/start.status >= Start.STATUS_RUN) {
                return getTextBandbreite(start.bandbreite);
            }
        }
        return "";
    }

    public static String getTextBandbreite(long b) {
        if (b > 1000 * 1000) {
            String s = String.valueOf(b / 1000);
            if (s.length() >= 4) {
                s = s.substring(0, s.length() - 3) + "," + s.substring(s.length() - 3);
            }
            return s + " MB/s";
        } else if (b > 1000) {
            return (b / 1000) + " kB/s";
        } else if (b > 1) {
            return b + " B/s";
        } else {
            return "";
        }
    }

    public boolean checkAufrufBauen() {
        return (pSet != null && film != null);
    }

    public void aufrufBauen() {
        aufrufBauen(pSet, film, abo, arr[DOWNLOAD_ZIEL_DATEINAME], arr[DOWNLOAD_ZIEL_PFAD]);
    }

    private void aufrufBauen(DatenPset pSet, DatenFilm film, DatenAbo abo, String nname, String ppfad) {
        //zieldatei und pfad bauen und eintragen
        try {
            DatenProg programm = pSet.getProgUrl(arr[DOWNLOAD_URL]);
            // ##############################################
            // für die alten Versionen:
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME].replace("%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME].replace("%p", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].replace("%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].replace("%p", "");
            for (DatenProg prog : pSet.getListeProg()) {
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME].replace("%n", "");
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME].replace("%p", "");
            }
            // ##############################################
            // pSet und ... eintragen
            arr[DOWNLOAD_PROGRAMMSET] = pSet.arr[DatenPset.PROGRAMMSET_NAME];

            // Direkter Download nur wenn url passt und wenn im Programm ein Zielpfad ist sonst Abspielen
            art = (pSet.checkDownloadDirekt(arr[DOWNLOAD_URL])
                    && pSet.progsContainPath()/*legt fest, dass NICHT Abspielen, Abspielen immer über Programm!*/) ? ART_DOWNLOAD : ART_PROGRAMM;
            arr[DOWNLOAD_ART] = String.valueOf(art);
            if (art == ART_DOWNLOAD) {
                arr[DatenDownload.DOWNLOAD_PROGRAMM] = ART_DOWNLOAD_TXT;
            } else {
                arr[DatenDownload.DOWNLOAD_PROGRAMM] = programm.arr[DatenProg.PROGRAMM_NAME];
            }

            arr[DOWNLOAD_PROGRAMM_RESTART] = String.valueOf(programm.isRestart());
            arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER] = String.valueOf(programm.isDownloadManager());
            dateinamePfadBauen(pSet, film, abo, nname, ppfad);
            programmaufrufBauen(programm);
        } catch (Exception ex) {
            Log.errorLog(825600145, ex);
        }
    }

    private void programmaufrufBauen(DatenProg programm) {
        if (art == ART_DOWNLOAD) {
            arr[DOWNLOAD_PROGRAMM_AUFRUF] = "";
            arr[DOWNLOAD_PROGRAMM_AUFRUF_ARRAY] = "";
        } else {
            String befehlsString = programm.getProgrammAufruf();
            befehlsString = replaceExec(befehlsString);
            arr[DOWNLOAD_PROGRAMM_AUFRUF] = befehlsString;

            String progArray = programm.getProgrammAufrufArray();
            progArray = replaceExec(progArray);
            arr[DOWNLOAD_PROGRAMM_AUFRUF_ARRAY] = progArray;
        }
    }

    private String replaceExec(String befehlsString) {
        befehlsString = befehlsString.replace("**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        befehlsString = befehlsString.replace("%f", arr[DOWNLOAD_URL]);
        befehlsString = befehlsString.replace("%F", arr[DOWNLOAD_URL_RTMP]);

        befehlsString = befehlsString.replace("%a", arr[DOWNLOAD_ZIEL_PFAD]);
        befehlsString = befehlsString.replace("%b", arr[DOWNLOAD_ZIEL_DATEINAME]);

        return befehlsString;
    }

    private void dateinamePfadBauen(DatenPset pSet, DatenFilm film, DatenAbo abo, String nname, String ppfad) {
        // nname und ppfad sind nur belegt, wenn der Download über den DialogAddDownload gestartet wurde (aus TabFilme)
        String name;
        String path;
        if (!pSet.progsContainPath()) {
            // dann können wir uns das sparen
            arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME] = "";
            arr[DatenDownload.DOWNLOAD_ZIEL_PFAD] = "";
            return;
        }

        // ##############################################
        // Name
        // ##############################################
        if (!nname.equals("")) {
            // wenn vorgegeben, dann den nehmen
            name = nname;
        } else {
            name = pSet.getZielDateiname(arr[DOWNLOAD_URL]);
            arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME] = name;
            // ##############################
            // Name sinnvoll belegen
            if (name.equals("")) {
                name = getHeute_yyyyMMdd() + "_" + arr[DatenDownload.DOWNLOAD_THEMA] + "-" + arr[DatenDownload.DOWNLOAD_TITEL] + ".mp4";
            }

            //Tags ersetzen
            name = replaceString(name, film); // %D ... ersetzen

            String suff = "";
            if (name.contains(".")) {
                //Suffix (und den . ) nicht ändern
                suff = name.substring(name.lastIndexOf("."));
                if (suff.length() <= 4 && suff.length() > 1) {
                    //dann ist es sonst was??
                    name = name.substring(0, name.lastIndexOf("."));
                } else {
                    suff = "";
                }
            }

            name = FilenameUtils.replaceLeerDateiname(name, false /*pfad*/,
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
            name = name + suff;

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
            if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN])) {
                int laenge = Konstanten.LAENGE_DATEINAME;
                if (!pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE].equals("")) {
                    laenge = Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE]);
                }
                name = GuiFunktionen.cutName(name, laenge);
            }
        }

        // ##############################################
        // Pfad
        // ##############################################
        if (!ppfad.isEmpty()) {
            // wenn vorgegeben, dann den nehmen
            path = ppfad;
        } else {
            // Pfad sinnvoll belegen
            if (pSet.getZielPfad().isEmpty()) {
                path = GuiFunktionen.getStandardDownloadPath();
            } else {
                path = pSet.getZielPfad();
            }

            if (abo != null) {
                //Abos: den Namen des Abos eintragen
                arr[DatenDownload.DOWNLOAD_ABO] = abo.arr[DatenAbo.ABO_NAME];
                if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN])) {
                    //und Abopfad an den Pfad anhängen
                    path = GuiFunktionen.addsPfad(path, FilenameUtils.removeIllegalCharacters(abo.arr[DatenAbo.ABO_ZIELPFAD], true));
                }
            } else //Downloads
             if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN])) {
                    //und den Namen des Themas an den Zielpfad anhängen
                    path = GuiFunktionen.addsPfad(path, FilenameUtils.replaceLeerDateiname(arr[DatenDownload.DOWNLOAD_THEMA], true /*pfad*/,
                            Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                            Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII))));
                }

            path = replaceString(path, film); // %D ... ersetzen
        }

        if (path.endsWith(File.separator)) {
            path = path.substring(0, path.length() - 1);
        }

        //###########################################################
        // zur Sicherheit bei Unsinn im Set
        if (path.isEmpty()) {
            path = GuiFunktionen.getStandardDownloadPath();
        }
        if (name.isEmpty()) {
            name = getHeute_yyyyMMdd() + "_" + arr[DatenDownload.DOWNLOAD_THEMA] + "-" + arr[DatenDownload.DOWNLOAD_TITEL] + ".mp4";
        }

        // in Win dürfen die Pfade nicht länger als 255 Zeichen haben (für die Infodatei kommen noch ".txt" dazu)
        String[] pathName = {path, name};
        GuiFunktionen.checkLengthPath(pathName);

        arr[DOWNLOAD_ZIEL_DATEINAME] = pathName[1];
        arr[DOWNLOAD_ZIEL_PFAD] = pathName[0];
        arr[DOWNLOAD_ZIEL_PFAD_DATEINAME] = GuiFunktionen.addsPfad(pathName[0], pathName[1]);
    }

    private String replaceString(String replStr, DatenFilm film) {
        //hier wird nur ersetzt! 
        //Felder mit variabler Länge, evtl. vorher kürzen

        int laenge = -1;

        if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN])) {
            // nur dann ist was zu tun
            laenge = Konstanten.LAENGE_FELD;
            if (!pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD].equals("")) {
                laenge = Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD]);
            }
        }

        replStr = replStr.replace("%t", getField(film.arr[DatenFilm.FILM_THEMA], laenge));
        replStr = replStr.replace("%T", getField(film.arr[DatenFilm.FILM_TITEL], laenge));
        replStr = replStr.replace("%s", getField(film.arr[DatenFilm.FILM_SENDER], laenge));
        replStr = replStr.replace("%N", getField(GuiFunktionen.getDateiName(this.arr[DatenDownload.DOWNLOAD_URL]), laenge));

        //Felder mit fester Länge werden immer ganz geschrieben
        replStr = replStr.replace("%D", film.arr[DatenFilm.FILM_DATUM].equals("") ? getHeute_yyyyMMdd() : datumDatumZeitReinigen(datumDrehen(film.arr[DatenFilm.FILM_DATUM])));
        replStr = replStr.replace("%d", film.arr[DatenFilm.FILM_ZEIT].equals("") ? getJetzt_HHMMSS() : datumDatumZeitReinigen(film.arr[DatenFilm.FILM_ZEIT]));
        replStr = replStr.replace("%H", getHeute_yyyyMMdd());
        replStr = replStr.replace("%h", getJetzt_HHMMSS());

        replStr = replStr.replace("%1", getDMY("%1", film.arr[DatenFilm.FILM_DATUM].equals("") ? getHeute_yyyy_MM_dd() : film.arr[DatenFilm.FILM_DATUM]));
        replStr = replStr.replace("%2", getDMY("%2", film.arr[DatenFilm.FILM_DATUM].equals("") ? getHeute_yyyy_MM_dd() : film.arr[DatenFilm.FILM_DATUM]));
        replStr = replStr.replace("%3", getDMY("%3", film.arr[DatenFilm.FILM_DATUM].equals("") ? getHeute_yyyy_MM_dd() : film.arr[DatenFilm.FILM_DATUM]));

        replStr = replStr.replace("%4", getHMS("%4", film.arr[DatenFilm.FILM_ZEIT].equals("") ? getJetzt_HH_MM_SS() : film.arr[DatenFilm.FILM_ZEIT]));
        replStr = replStr.replace("%5", getHMS("%5", film.arr[DatenFilm.FILM_ZEIT].equals("") ? getJetzt_HH_MM_SS() : film.arr[DatenFilm.FILM_ZEIT]));
        replStr = replStr.replace("%6", getHMS("%6", film.arr[DatenFilm.FILM_ZEIT].equals("") ? getJetzt_HH_MM_SS() : film.arr[DatenFilm.FILM_ZEIT]));

        replStr = replStr.replace("%i", String.valueOf(film.nr));

        String res = "";
        if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL))) {
            res = "H";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD))) {
            res = "HD";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN))) {
            res = "L";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlRtmpFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL))) {
            res = "H";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlRtmpFuerAufloesung(DatenFilm.AUFLOESUNG_HD))) {
            res = "HD";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlRtmpFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN))) {
            res = "L";
        }
        replStr = replStr.replace("%q", res); //%q Qualität des Films ("HD", "H", "L")

        replStr = replStr.replace("%S", GuiFunktionen.getSuffixFromUrl(this.arr[DatenDownload.DOWNLOAD_URL]));
        replStr = replStr.replace("%Z", GuiFunktionen.getHash(this.arr[DatenDownload.DOWNLOAD_URL]));
        replStr = replStr.replace("%z", GuiFunktionen.getHash(this.arr[DatenDownload.DOWNLOAD_URL])
                + "." + GuiFunktionen.getSuffixFromUrl(this.arr[DatenDownload.DOWNLOAD_URL]));

        return replStr;
    }

    private String getField(String name, int length) {
        name = FilenameUtils.replaceLeerDateiname(name, false /*pfad*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));

        if (length < 0) {
            return name;
        }

        if (name.length() > length) {
            name = name.substring(0, length);
        }
        return name;
    }

    private String getJetzt_HHMMSS() {
        return new SimpleDateFormat("HHmmss").format(new Date());
    }

    private String getJetzt_HH_MM_SS() {
        return new SimpleDateFormat("HH:mm:ss").format(new Date());
    }

    private String getHeute_yyyyMMdd() {
        return new SimpleDateFormat("yyyyMMdd").format(new Date());
    }

    private String getHeute_yyyy_MM_dd() {
        return new SimpleDateFormat("dd.MM.yyyy").format(new Date());
    }

    private static String getDMY(String s, String datum) {
        // liefert das Datum: Jahr - Monat - Tag aus dd.MM.yyyy
        // %1 - Tag
        // %2 - Monat
        // %3 - Jahr
        String ret = "";
        if (!datum.equals("")) {
            try {
                if (datum.length() == 10) {
                    switch (s) {
                        case "%1":
                            ret = datum.substring(0, 2); // Tag
                            break;
                        case "%2":
                            ret = datum.substring(3, 5); // Monat
                            break;
                        case "%3":
                            ret = datum.substring(6); // Jahr
                            break;

                    }
                }
            } catch (Exception ex) {
                Log.errorLog(775421006, ex, datum);
            }
        }
        return ret;
    }

    private static String getHMS(String s, String zeit) {
        // liefert die Zeit: Stunde, Minute, Sekunde aus "HH:mm:ss"
        // %4 - Stunde
        // %5 - Minute
        // %6 - Sekunde
        String ret = "";
        if (!zeit.equals("")) {
            try {
                if (zeit.length() == 8) {
                    switch (s) {
                        case "%4":
                            ret = zeit.substring(0, 2); // Stunde
                            break;
                        case "%5":
                            ret = zeit.substring(3, 5); // Minute
                            break;
                        case "%6":
                            ret = zeit.substring(6); // Sekunde
                            break;

                    }
                }
            } catch (Exception ex) {
                Log.errorLog(775421006, ex, zeit);
            }
        }
        return ret;
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
                Log.errorLog(775421006, ex, datum);
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

    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        arr[DOWNLOAD_ZURUECKGESTELLT] = Boolean.FALSE.toString();
        arr[DOWNLOAD_UNTERBROCHEN] = Boolean.FALSE.toString();
    }

    public Datum getDatumForObject() {
        Datum tmp = new Datum(0);
        if (!arr[DatenDownload.DOWNLOAD_DATUM].equals("")) {
            try {
                if (!arr[DatenDownload.DOWNLOAD_ZEIT].equals("")) {
                    tmp.setTime(sdf_datum_zeit.parse(arr[DatenDownload.DOWNLOAD_DATUM] + arr[DatenDownload.DOWNLOAD_ZEIT]).getTime());
                } else {
                    tmp.setTime(sdf_datum.parse(arr[DatenDownload.DOWNLOAD_DATUM]).getTime());
                }
            } catch (Exception ex) {
                Log.errorLog(649897321, ex,
                        new String[]{"Datum: " + arr[DatenDownload.DOWNLOAD_DATUM], "Zeit: " + arr[DatenDownload.DOWNLOAD_ZEIT]});
            }
        }
        return tmp;
    }

    public String getFileNameWithoutSuffix() {
        return GuiFunktionen.getFileNameWithoutSuffix(arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
    }

    public String getFileNameSuffix() {
        return GuiFunktionen.getFileNameSuffix(arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
    }

    @Override
    public int compareTo(DatenDownload arg0) {
        int ret;
        if ((ret = sorter.compare(arr[DatenDownload.DOWNLOAD_SENDER], arg0.arr[DatenDownload.DOWNLOAD_SENDER])) == 0) {
            return sorter.compare(arr[DatenDownload.DOWNLOAD_THEMA], arg0.arr[DatenDownload.DOWNLOAD_THEMA]);
        }
        return ret;
    }
}
