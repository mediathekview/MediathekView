package mediathek.daten;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.gui.messages.RestartDownloadEvent;
import mediathek.gui.messages.StartEvent;
import mediathek.tool.*;
import okhttp3.HttpUrl;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

public final class DatenDownload implements Comparable<DatenDownload> {

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
    public static final String TAG = "Downlad";
    public static final int MAX_ELEM = 40;
    private static final String[] XML_NAMES = {"Nr", "Filmnr", "Abo", "Sender", "Thema", "Titel", "Button-Start", "Button-Del",
            "Fortschritt", "Restzeit", "Geschwindigkeit", "Groesse"/*DOWNLOAD_GROESSE*/,
            "Datum", "Zeit", "Dauer", "HD", "UT",
            "Pause", "Geo", "Film-URL", "History-URL", "URL", "URL-rtmp", "URL-Untertitel",
            "Programmset", "Programm", "Programmaufruf_", "Programmaufruf", "Restart",
            "Dateiname", "Pfad", "Pfad-Dateiname", "Art", "Quelle",
            "Zurueckgestellt", "Infodatei", "Spotlight", "Untertitel", "Remote-Download", "Ref"};
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final FastDateFormat sdf_datum_zeit = FastDateFormat.getInstance("dd.MM.yyyyHH:mm:ss");
    private static final FastDateFormat sdf_datum = FastDateFormat.getInstance("dd.MM.yyyy");
    private static final Logger logger = LogManager.getLogger(DatenDownload.class);
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public String[] arr;
    public Datum datumFilm = new Datum(0);
    public DatenFilm film;
    public MVFilmSize mVFilmSize = new MVFilmSize();
    public Start start;
    public DatenPset pSet;
    public DatenAbo abo;
    public int nr;
    public byte quelle = QUELLE_ALLE;
    public byte art = ART_DOWNLOAD;

    public DatenDownload() {
        initialize();
    }

    public DatenDownload(DatenPset pSet, DatenFilm film, byte quelle, DatenAbo abo, String name, String pfad, String aufloesung) {
        initialize();
        this.film = film;
        this.pSet = pSet;
        this.abo = abo;
        arr[DOWNLOAD_FILM_NR] = Integer.toString(film.getFilmNr());
        arr[DOWNLOAD_SENDER] = film.getSender();
        arr[DOWNLOAD_THEMA] = film.getThema();
        arr[DOWNLOAD_TITEL] = film.getTitle();
        arr[DOWNLOAD_FILM_URL] = film.getUrl();
        arr[DOWNLOAD_URL_SUBTITLE] = film.getUrlSubtitle();
        arr[DOWNLOAD_DATUM] = film.getSendeDatum();
        arr[DOWNLOAD_ZEIT] = film.getSendeZeit();
        arr[DOWNLOAD_DAUER] = film.getDauer();
        arr[DOWNLOAD_HD] = film.isHighQuality() ? "1" : "0";
        arr[DOWNLOAD_UT] = film.hasSubtitle() ? "1" : "0";
        arr[DOWNLOAD_QUELLE] = String.valueOf(quelle);
        arr[DOWNLOAD_HISTORY_URL] = film.getUrl();
        if (aufloesung.isEmpty()) {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]));
        } else {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(aufloesung));
        }
        arr[DatenDownload.DOWNLOAD_INFODATEI] = pSet.arr[DatenPset.PROGRAMMSET_INFODATEI];
        arr[DatenDownload.DOWNLOAD_SUBTITLE] = pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE];
        arr[DatenDownload.DOWNLOAD_SPOTLIGHT] = pSet.arr[DatenPset.PROGRAMMSET_SPOTLIGHT];
        arr[DatenDownload.DOWNLOAD_GEO] = film.getGeo().orElse("");

        // und jetzt noch die Dateigröße für die entsp. URL
        setGroesse("");

        aufrufBauen(pSet, film, abo, name, pfad);
        init();
    }

    /**
     * Read the download data from config.
     * @param parser The parser for the config file
     * @return A valid DatenDownload object when everything went smooth.
     * @throws XMLStreamException
     */
    public static DatenDownload getFromConfig(XMLStreamReader parser) throws XMLStreamException {
        DatenDownload dl = new DatenDownload();

        final int maxElem = dl.arr.length;
        for (int i = 0; i < maxElem; ++i) {
            if (dl.arr[i] == null) {
                // damit Vorgaben nicht verschwinden!
                dl.arr[i] = "";
            }
        }

        while (parser.hasNext()) {
            final int event = parser.next();
            if (event == XMLStreamConstants.END_ELEMENT) {
                if (parser.getLocalName().equals(TAG)) {
                    break;
                }
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                for (int i = 0; i < maxElem; ++i) {
                    if (parser.getLocalName().equals(XML_NAMES[i])) {
                        dl.arr[i] = parser.getElementText();
                        break;
                    }
                }
            }
        }

        //everything went fine so initialize and return
        dl.init();
        return dl;
    }

    /**
     * Store the download data in config file.
     *
     * @param writer  the writer to the config file.
     */
    public void writeConfigEntry(XMLStreamWriter writer) {
        final int xmlMax = arr.length;
        try {
            writer.writeStartElement(TAG);
            writer.writeCharacters("\n");
            for (int i = 0; i < xmlMax; ++i) {
                if (!arr[i].isEmpty()) {
                    writer.writeCharacters("\t"); //Tab
                    writer.writeStartElement(XML_NAMES[i]);
                    writer.writeCharacters(arr[i]);
                    writer.writeEndElement();
                    writer.writeCharacters("\n");
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n");
        } catch (Exception ex) {
            logger.error("writeConfigEntry", ex);
        }
    }

    public static boolean anzeigen(int i) {
        if (spaltenAnzeigen == null) {
            return true;
        } else {
            return spaltenAnzeigen[i];
        }
    }

    public static void startenDownloads(ArrayList<DatenDownload> downloads) {
        // Start erstellen und zur Liste hinzufügen
        try (var historyController = new SeenHistoryController()){
            for (DatenDownload d : downloads) {
                d.start = new Start();
                historyController.writeManualEntry(d.arr[DatenDownload.DOWNLOAD_THEMA],
                        d.arr[DatenDownload.DOWNLOAD_TITEL], d.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
            }
        }
        Daten.getInstance().getMessageBus().publishAsync(new StartEvent());
    }

    public static String getTextBandbreite(long b) {
        if (b > 1000 * 1000) {
            String s = String.valueOf(b / 1000);
            if (s.length() >= 4) {
                s = s.substring(0, s.length() - 3) + ',' + s.substring(s.length() - 3);
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

    private static String getDMY(String s, String datum) {
        // liefert das Datum: Jahr - Monat - Tag aus dd.MM.yyyy
        // %1 - Tag
        // %2 - Monat
        // %3 - Jahr
        String ret = "";
        if (!datum.isEmpty()) {
            try {
                if (datum.length() == 10) {
                    switch (s) {
                        case DMYHMSTags.DAY -> ret = datum.substring(0, 2);
                        case DMYHMSTags.MONTH -> ret = datum.substring(3, 5);
                        case DMYHMSTags.YEAR -> ret = datum.substring(6);
                    }
                }
            } catch (Exception ex) {
                logger.error("Datum: {}", datum, ex);
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
        if (!zeit.isEmpty()) {
            try {
                if (zeit.length() == 8) {
                    switch (s) {
                        case DMYHMSTags.HOUR -> ret = zeit.substring(0, 2);
                        case DMYHMSTags.MINUTE -> ret = zeit.substring(3, 5);
                        case DMYHMSTags.SECOND -> ret = zeit.substring(6);
                    }
                }
            } catch (Exception ex) {
                logger.error("Zeit: {}", zeit, ex);
            }
        }
        return ret;
    }

    private static String datumDrehen(String datum) {
        String ret = "";
        if (!datum.isEmpty()) {
            try {
                if (datum.length() == 10) {
                    String tmp = datum.substring(6); // Jahr
                    tmp += '.' + datum.substring(3, 5); // Monat
                    tmp += '.' + datum.substring(0, 2); // Tag
                    ret = tmp;
                }
            } catch (Exception ex) {
                logger.error("Datum: {}", datum, ex);
            }
        }
        return ret;
    }

    private static String datumDatumZeitReinigen(String datum) {
        String ret = StringUtils.replace(datum, ":", "");
        ret = StringUtils.replace(ret, ".", "");
        return ret;
    }

    public void setGroesseFromFilm() {
        if (film != null) {
            if (film.getUrl().equals(arr[DOWNLOAD_URL])) {
                mVFilmSize.setSize(film.getSize());
            } else {
                mVFilmSize.setSize(0);
            }
        }
    }

    public void setGroesse(@NotNull String groesse) {
        if (film != null) {
            if (!groesse.isEmpty()) {
                mVFilmSize.setSize(groesse);
            } else {
                mVFilmSize.setSize(film.getDateigroesse(arr[DOWNLOAD_URL]));
            }
        }
    }

    public final void init() {
        datumFilm = getDatumForObject();
        try {
            art = Byte.parseByte(arr[DOWNLOAD_ART]);
            quelle = Byte.parseByte(arr[DOWNLOAD_QUELLE]);
        } catch (Exception ex) {
            logger.error("Art: {}, Quelle: {}", arr[DOWNLOAD_ART], arr[DOWNLOAD_QUELLE], ex);
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
        Daten.getInstance().getMessageBus().publishAsync(new RestartDownloadEvent());
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
        return start != null && start.status < Start.STATUS_FERTIG;
    }

    public boolean running() {
        return start != null && start.status == Start.STATUS_RUN;
    }

    public void resetDownload() {
        mVFilmSize.reset();
        start = null;
    }

    public void startDownload() {
        // Start erstellen und zur Liste hinzufügen
        this.start = new Start();

        try (var historyController = new SeenHistoryController()){
            historyController.writeManualEntry(arr[DatenDownload.DOWNLOAD_THEMA],
                    arr[DatenDownload.DOWNLOAD_TITEL], arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
        }

        Daten.getInstance().getMessageBus().publishAsync(new StartEvent());
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

    /**
     * Was this download generated by an abo?
     *
     * @return true if it was generated by an abo, otherwise false.
     */
    public boolean isFromAbo() {
        return !arr[DatenDownload.DOWNLOAD_ABO].isEmpty();
    }

    public boolean isRestart() {
        if (arr[DOWNLOAD_PROGRAMM_RESTART].isEmpty()) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_RESTART]);
    }

    public boolean isDownloadManager() {
        if (arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER].isEmpty()) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER]);
    }

    public boolean isInfoFile() {
        if (arr[DOWNLOAD_INFODATEI].isEmpty()) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_INFODATEI]);
    }

    public boolean isSubtitle() {
        if (arr[DOWNLOAD_SUBTITLE].isEmpty()) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_SUBTITLE]);
    }

    public boolean isSpotlight() {
        if (arr[DOWNLOAD_SPOTLIGHT].isEmpty()) {
            return false;
        }
        return Boolean.parseBoolean(arr[DOWNLOAD_SPOTLIGHT]);
    }

    public String getTextRestzeit() {
        if (start != null) {
            if (start.status < Start.STATUS_FERTIG && start.status >= Start.STATUS_RUN && start.restSekunden > 0) {

                if (start.restSekunden > 300) {
                    return Math.round(start.restSekunden / 60.0) + " Min.";
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
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME] = StringUtils.replace(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME], "%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME] = StringUtils.replace(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME], "%p", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = StringUtils.replace(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD], "%n", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = StringUtils.replace(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD], "%p", "");

            for (DatenProg prog : pSet.getListeProg()) {
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = StringUtils.replace(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME], "%n", "");
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = StringUtils.replace(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME], "%p", "");
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
                arr[DatenDownload.DOWNLOAD_PROGRAMM] = programm != null ? programm.arr[DatenProg.PROGRAMM_NAME] : "Unknown";
            }
            if (programm != null) {
                arr[DOWNLOAD_PROGRAMM_RESTART] = String.valueOf(programm.isRestart());
                arr[DOWNLOAD_PROGRAMM_DOWNLOADMANAGER] = String.valueOf(programm.isDownloadManager());
                dateinamePfadBauen(pSet, film, abo, nname, ppfad);
                programmaufrufBauen(programm);
            }
        } catch (Exception ex) {
            logger.error("aufrufBauen", ex);
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
        befehlsString = StringUtils.replace(befehlsString, "**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        befehlsString = StringUtils.replace(befehlsString, "%f", arr[DOWNLOAD_URL]);
        //FIXME %F needs to be removed as we no longer use flvstreamer
        befehlsString = StringUtils.replace(befehlsString, "%F", arr[DOWNLOAD_URL_RTMP]);
        befehlsString = StringUtils.replace(befehlsString, "%a", arr[DOWNLOAD_ZIEL_PFAD]);
        befehlsString = StringUtils.replace(befehlsString, "%b", arr[DOWNLOAD_ZIEL_DATEINAME]);

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

        // Name
        if (!nname.isEmpty()) {
            // wenn vorgegeben, dann den nehmen
            name = nname;
        } else {
            name = pSet.getZielDateiname(arr[DOWNLOAD_URL]);
            arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME] = name;
            // Name sinnvoll belegen
            if (name.isEmpty()) {
                name = getHeute_yyyyMMdd() + '_' + arr[DatenDownload.DOWNLOAD_THEMA] + '-' + arr[DatenDownload.DOWNLOAD_TITEL] + ".mp4";
            }

            //Tags ersetzen
            name = replaceString(name, film); // %D ... ersetzen

            String suff = "";
            if (name.contains(".")) {
                //Suffix (und den . ) nicht ändern
                suff = name.substring(name.lastIndexOf('.'));
                if (suff.length() <= 4 && suff.length() > 1) {
                    //dann ist es sonst was??
                    name = name.substring(0, name.lastIndexOf('.'));
                } else {
                    suff = "";
                }
            }

            name = FilenameUtils.replaceLeerDateiname(name, false /*pfad*/,
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
            name += suff;

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
                if (!pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE].isEmpty()) {
                    laenge = Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE]);
                }
                name = GuiFunktionen.cutName(name, laenge);
            }
        }

        // Pfad
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

        // zur Sicherheit bei Unsinn im Set
        if (path.isEmpty()) {
            path = GuiFunktionen.getStandardDownloadPath();
        }
        if (name.isEmpty()) {
            name = getHeute_yyyyMMdd() + '_' + arr[DatenDownload.DOWNLOAD_THEMA] + '-' + arr[DatenDownload.DOWNLOAD_TITEL] + ".mp4";
        }

        FileSpecifier fileSpecifier = new FileSpecifier(path, name);
        fileSpecifier.checkLength();

        arr[DOWNLOAD_ZIEL_DATEINAME] = fileSpecifier.getFileName();
        arr[DOWNLOAD_ZIEL_PFAD] = fileSpecifier.getPath();
        arr[DOWNLOAD_ZIEL_PFAD_DATEINAME] = GuiFunktionen.addsPfad(fileSpecifier.getPath(), fileSpecifier.getFileName());
    }

    private String replaceString(String replStr, DatenFilm film) {
        //hier wird nur ersetzt!
        //Felder mit variabler Länge, evtl. vorher kürzen

        int laenge = -1;

        if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN])) {
            // nur dann ist was zu tun
            laenge = Konstanten.LAENGE_FELD;
            if (!pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD].isEmpty()) {
                laenge = Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD]);
            }
        }

        replStr = StringUtils.replace(replStr, "%t", getField(film.getThema(), laenge));
        replStr = StringUtils.replace(replStr, "%T", getField(film.getTitle(), laenge));
        replStr = StringUtils.replace(replStr, "%s", getField(film.getSender(), laenge));

        final String downloadUrl = this.arr[DatenDownload.DOWNLOAD_URL];
        //special case only for austrian ORF and m3u8 files
        //github issue #388
        if (downloadUrl.endsWith(".m3u8") && downloadUrl.contains(".at")) {
            String field = getField(GuiFunktionen.getDateiName(downloadUrl), laenge);

            final HttpUrl url = HttpUrl.parse(downloadUrl);
            if (url != null) {
                final var segments = url.pathSegments();
                final var segment = segments.get(segments.size() - 2);
                field = getField(GuiFunktionen.getDateiName(segment), laenge);
                field = org.apache.commons.io.FilenameUtils.removeExtension(field);
            }

            replStr = StringUtils.replace(replStr, "%N", field);
        } else
            replStr = StringUtils.replace(replStr, "%N", getField(GuiFunktionen.getDateiName(downloadUrl), laenge));

        //Felder mit fester Länge werden immer ganz geschrieben
        replStr = StringUtils.replace(replStr, "%D", film.getSendeDatum().isEmpty() ? getHeute_yyyyMMdd() : datumDatumZeitReinigen(datumDrehen(film.getSendeDatum())));
        replStr = StringUtils.replace(replStr, "%d", film.getSendeZeit().isEmpty() ? getJetzt_HHMMSS() : datumDatumZeitReinigen(film.getSendeZeit()));
        replStr = StringUtils.replace(replStr, "%H", getHeute_yyyyMMdd());
        replStr = StringUtils.replace(replStr, "%h", getJetzt_HHMMSS());

        replStr = StringUtils.replace(replStr, "%1", getDMY("%1", film.getSendeDatum().isEmpty() ? getHeute_yyyy_MM_dd() : film.getSendeDatum()));
        replStr = StringUtils.replace(replStr, "%2", getDMY("%2", film.getSendeDatum().isEmpty() ? getHeute_yyyy_MM_dd() : film.getSendeDatum()));
        replStr = StringUtils.replace(replStr, "%3", getDMY("%3", film.getSendeDatum().isEmpty() ? getHeute_yyyy_MM_dd() : film.getSendeDatum()));
        replStr = StringUtils.replace(replStr, "%4", getHMS("%4", film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()));
        replStr = StringUtils.replace(replStr, "%5", getHMS("%5", film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()));
        replStr = StringUtils.replace(replStr, "%6", getHMS("%6", film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()));

        replStr = StringUtils.replace(replStr, "%i", String.valueOf(film.getFilmNr()));

        String res = "";
        if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL))) {
            res = "H";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY))) {
            res = "HD";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW))) {
            res = "L";
        }
        replStr = StringUtils.replace(replStr, "%q", res);

        replStr = StringUtils.replace(replStr, "%S", GuiFunktionen.getSuffixFromUrl(downloadUrl));
        replStr = StringUtils.replace(replStr, "%Z", getHash(downloadUrl));

        replStr = StringUtils.replace(replStr, "%z", getHash(downloadUrl)
                + '.' + GuiFunktionen.getSuffixFromUrl(downloadUrl));

        return replStr;
    }

    private String getHash(String pfad) {
        //Hash eines Dateinamens zB. 1433245578
        final int h = Math.abs(pfad.hashCode());
        StringBuilder hh = new StringBuilder(Integer.toString(h));
        while (hh.length() < 10) {
            hh.insert(0, '0');
        }
        return hh.toString();
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
        return FastDateFormat.getInstance("HHmmss").format(new Date());
    }

    private String getJetzt_HH_MM_SS() {
        return FastDateFormat.getInstance("HH:mm:ss").format(new Date());
    }

    private String getHeute_yyyyMMdd() {
        return FastDateFormat.getInstance("yyyyMMdd").format(new Date());
    }

    private String getHeute_yyyy_MM_dd() {
        return sdf_datum.format(new Date());
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");

        arr[DOWNLOAD_ZURUECKGESTELLT] = Boolean.FALSE.toString();
        arr[DOWNLOAD_UNTERBROCHEN] = Boolean.FALSE.toString();
    }

    public Datum getDatumForObject() {
        Datum tmp = new Datum(0);
        if (!arr[DatenDownload.DOWNLOAD_DATUM].isEmpty()) {
            try {
                if (!arr[DatenDownload.DOWNLOAD_ZEIT].isEmpty()) {
                    tmp.setTime(sdf_datum_zeit.parse(arr[DatenDownload.DOWNLOAD_DATUM] + arr[DatenDownload.DOWNLOAD_ZEIT]).getTime());
                } else {
                    tmp.setTime(sdf_datum.parse(arr[DatenDownload.DOWNLOAD_DATUM]).getTime());
                }
            } catch (Exception ex) {
                logger.error("Datum: {}, Zeit: {}", arr[DatenDownload.DOWNLOAD_DATUM], arr[DatenDownload.DOWNLOAD_ZEIT], ex);
            }
        }
        return tmp;
    }

    public String getFileNameWithoutSuffix() {
        return GuiFunktionen.getFileNameWithoutSuffix(arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
    }

    @Override
    public int compareTo(DatenDownload arg0) {
        int ret;
        if ((ret = sorter.compare(arr[DatenDownload.DOWNLOAD_SENDER], arg0.arr[DatenDownload.DOWNLOAD_SENDER])) == 0) {
            return sorter.compare(arr[DatenDownload.DOWNLOAD_THEMA], arg0.arr[DatenDownload.DOWNLOAD_THEMA]);
        }
        return ret;
    }

    /**
     * Tags for getDMY() and getHMS() functions.
     */
    private static class DMYHMSTags {
        public static final String DAY = "%1";
        public static final String MONTH = "%2";
        public static final String YEAR = "%3";

        public static final String HOUR = "%4";
        public static final String MINUTE = "%5";
        public static final String SECOND = "%6";
    }
}
