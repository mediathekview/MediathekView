package mediathek.daten;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.config.StandardLocations;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.abo.DatenAbo;
import mediathek.gui.messages.RestartDownloadEvent;
import mediathek.gui.messages.StartEvent;
import mediathek.tool.*;
import mediathek.tool.datum.Datum;
import okhttp3.HttpUrl;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

public class DatenDownload implements Comparable<DatenDownload> {

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
    public static final boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public static final String[] XML_NAMES = {"Nr", "Filmnr", "Abo", "Sender", "Thema", "Titel", "Button-Start", "Button-Del",
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
    private static final String TWO_LETTER_YEAR_PARAMETER = "%3_2";
    private static final String FOUR_LETTER_YEAR_PARAMETER = "%3";
    private static final DateTimeFormatter HHMMSS = DateTimeFormatter.ofPattern("HHmmss");
    private static final DateTimeFormatter HH_MM_SS = DateTimeFormatter.ofPattern("HH:mm:ss");
    private static final DateTimeFormatter YYYYMMDD = DateTimeFormatter.ofPattern("yyyyMMdd");
    private static final DateTimeFormatter DATUM_FORMAT = DateTimeFormatter.ofPattern("dd.MM.yyyy");
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
    private String websiteUrl = "";

    public DatenDownload() {
        initialize();
    }

    public DatenDownload(DatenPset pSet, DatenFilm film, byte quelle, DatenAbo abo, String name, String pfad, String aufloesung) {
        initialize();
        this.film = film;
        this.pSet = pSet;
        this.abo = abo;
        arr[DOWNLOAD_FILM_NR] = Long.toString(film.getFilmNr());
        arr[DOWNLOAD_SENDER] = film.getSender();
        arr[DOWNLOAD_THEMA] = film.getThema();
        arr[DOWNLOAD_TITEL] = film.getTitle();
        arr[DOWNLOAD_FILM_URL] = film.getUrlNormalQuality();
        arr[DOWNLOAD_URL_SUBTITLE] = film.getSubtitleUrl();
        arr[DOWNLOAD_DATUM] = film.getSendeDatum();
        arr[DOWNLOAD_ZEIT] = film.getSendeZeit();
        arr[DOWNLOAD_DAUER] = film.getFilmLengthAsString();
        arr[DOWNLOAD_HD] = film.isHighQuality() ? "1" : "0";
        arr[DOWNLOAD_UT] = film.hasSubtitle() ? "1" : "0";
        arr[DOWNLOAD_QUELLE] = String.valueOf(quelle);
        arr[DOWNLOAD_HISTORY_URL] = film.getUrlNormalQuality();
        if (aufloesung.isEmpty()) {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]));
        } else {
            arr[DOWNLOAD_URL] = film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(aufloesung));
        }
        //if URL contains query parameters
        if (arr[DOWNLOAD_URL].contains("?")) {
            //remove query parameters
            arr[DOWNLOAD_URL] = getUrlWithoutParameters(arr[DOWNLOAD_URL]);
        }

        arr[DatenDownload.DOWNLOAD_INFODATEI] = pSet.arr[DatenPset.PROGRAMMSET_INFODATEI];
        arr[DatenDownload.DOWNLOAD_SUBTITLE] = pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE];
        arr[DatenDownload.DOWNLOAD_SPOTLIGHT] = pSet.arr[DatenPset.PROGRAMMSET_SPOTLIGHT];
        if (film.countrySet.isEmpty())
            arr[DatenDownload.DOWNLOAD_GEO] = "";
        else {
            arr[DatenDownload.DOWNLOAD_GEO] = film.countrySet.stream().map(Country::toString).collect(Collectors.joining("-"));
        }

        websiteUrl = film.getWebsiteUrl();

        // und jetzt noch die Dateigröße für die entsp. URL
        setGroesse("");

        aufrufBauen(pSet, film, abo, name, pfad);
        init();
    }

    public DatenDownload(@NotNull DatenPset pSet, @NotNull DatenFilm film, byte quelle, DatenAbo abo, String name, String pfad, String aufloesung, boolean info, boolean subtitle) {
        this(pSet, film, quelle, abo, name, pfad, aufloesung);
        arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
        arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);
    }

    /**
     * Read the download data from config.
     *
     * @param parser The parser for the config file
     * @return A valid DatenDownload object when everything went smooth.
     * @throws XMLStreamException when something went wrong
     */
    public static DatenDownload readFromConfig(@NotNull XMLStreamReader parser) throws XMLStreamException {
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

    public static boolean anzeigen(int i) {
        if (spaltenAnzeigen == null) {
            return true;
        } else {
            return spaltenAnzeigen[i];
        }
    }

    /**
     * Starts all downloads from list but fire only one update event.
     *
     * @param downloads the list of downloads
     */
    public static void startenDownloads(ArrayList<DatenDownload> downloads) {
        // Start erstellen und zur Liste hinzufügen
        try (var historyController = new SeenHistoryController()) {
            for (DatenDownload d : downloads) {
                d.start = new Start();
                historyController.markSeen(d.film);
            }
        }
        MessageBus.getMessageBus().publishAsync(new StartEvent());
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

    /// Remove `.` and `:` from input string.
    /// @return the cleanup result.
    protected String stripDotsAndColons(String datum) {
        return datum.replace(":", "").replace(".", "");
    }

    /**
     * Remove all query parameters from url, e.g. ?explicit=true
     *
     * @param url the original url
     * @return filtered url string
     */
    private String getUrlWithoutParameters(String url) {
        try {
            var uri = new URI(url);
            return new URI(uri.getScheme(),
                    uri.getAuthority(),
                    uri.getPath(),
                    null, // Ignore the query part of the input url
                    uri.getFragment()).toString();
        } catch (URISyntaxException e) {
            logger.error("Failed to parse url, returning unmodified", e);
            return url;
        }
    }

    public void startDownload() {
        // Start erstellen und zur Liste hinzufügen
        this.start = new Start();

        try (var historyController = new SeenHistoryController()) {
            historyController.markSeen(film);
        }

        MessageBus.getMessageBus().publishAsync(new StartEvent());
    }

    /**
     * Return a specific string based on the specified tag.
     *
     * @param tag   Specifies what to return from the date string (xx.xx.xxxx)
     * @param datum a date string
     * @return Return the string part specified by tag
     */
    private String getDMY(DMYTag tag, String datum) {
        String ret = "";
        if (!datum.isEmpty()) {
            try {
                if (datum.length() == 10) {
                    switch (tag) {
                        case DAY -> ret = datum.substring(0, 2);
                        case MONTH -> ret = datum.substring(3, 5);
                        case YEAR -> ret = datum.substring(6);
                    }
                }
            } catch (Exception ex) {
                logger.error("Datum: {}", datum, ex);
            }
        }
        return ret;
    }

    /**
     * Return a specific string based on the specified tag.
     *
     * @param tag  Specifies what to return from the time string ("HH:mm:ss")
     * @param zeit a time string
     * @return Return the string part specified by tag
     */
    private String getHMS(HMSTag tag, String zeit) {
        String ret = "";
        if (!zeit.isEmpty()) {
            try {
                if (zeit.length() == 8) {
                    switch (tag) {
                        case HOUR -> ret = zeit.substring(0, 2);
                        case MINUTE -> ret = zeit.substring(3, 5);
                        case SECOND -> ret = zeit.substring(6);
                    }
                }
            } catch (Exception ex) {
                logger.error("Zeit: {}", zeit, ex);
            }
        }
        return ret;
    }

    private void writeEntry(@NotNull XMLStreamWriter writer, int index, @NotNull String data) throws XMLStreamException {
        writer.writeCharacters("\t"); //Tab
        writer.writeStartElement(XML_NAMES[index]);
        writer.writeCharacters(data);
        writer.writeEndElement();
        writer.writeCharacters("\n");
    }

    /**
     * Store the download data in config file.
     *
     * @param writer the writer to the config file.
     */
    public void writeConfigEntry(XMLStreamWriter writer) {
        final int xmlMax = arr.length;
        try {
            writer.writeStartElement(TAG);
            writer.writeCharacters("\n");
            for (int i = 0; i < xmlMax; ++i) {
                if (i == DatenDownload.DOWNLOAD_GROESSE) {
                    var size = mVFilmSize.getSize();
                    size /= FileSize.ONE_MiB;
                    writeEntry(writer, DatenDownload.DOWNLOAD_GROESSE, Long.toString(size));
                } else {
                    if (!arr[i].isEmpty()) {
                        writeEntry(writer, i, arr[i]);
                    }
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n");
        } catch (Exception ex) {
            logger.error("writeConfigEntry", ex);
        }
    }

    public void setGroesseFromFilm() {
        if (film != null) {
            if (film.getUrlNormalQuality().equals(arr[DOWNLOAD_URL])) {
                mVFilmSize.setSize(film.getFileSize().toString());
            } else {
                mVFilmSize.setSize(0);
            }
        }
    }

    public void setGroesse(@NotNull String groesse) {
        if (film != null) {
            if (!groesse.isEmpty()) {
                mVFilmSize.setSize(groesse);
            }
        }
    }

    /**
     * Request the file size live from Internet.
     * Might cause delays when network is slow.
     */
    public void queryLiveSize() {
        if (film != null) {
            mVFilmSize.setSize(film.getFileSizeForUrl(arr[DOWNLOAD_URL]));
        }
    }

    public void init() {
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

        /*
         * reader reads only into arr but does not properly fill the mVFilmSize...
         * do it manually
         */
        if (!arr[DatenDownload.DOWNLOAD_GROESSE].isEmpty()) {
            try {
                var size = Long.parseLong(arr[DatenDownload.DOWNLOAD_GROESSE]);
                mVFilmSize.setSize(size * FileSize.ONE_MiB);
            } catch (NumberFormatException ignored) {
            }
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
        MessageBus.getMessageBus().publishAsync(new RestartDownloadEvent());
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

    /**
     * Return number of minutes based on input seconds.
      * @param sekunden remaining time in seconds
     * @return string for minutes remaining
     */
    protected String formatTimeRemaining(long sekunden) {
        if (sekunden > 300) {
            return Math.round(sekunden / 60.0) + " Min.";
        }

        // geordnete Schwellenwerte (von hoch nach niedrig)
        int[] limits =    {230, 170, 110,  60,  30,  20,  10};
        String[] labels = {"5 Min.", "4 Min.", "3 Min.", "2 Min.", "1 Min.", "30 s", "20 s"};

        for (int i = 0; i < limits.length; i++) {
            if (sekunden > limits[i]) {
                return labels[i];
            }
        }

        return "10 s";
    }

    public String getTextRestzeit() {
        if (start != null) {
            if (start.status < Start.STATUS_FERTIG && start.status == Start.STATUS_RUN && start.restSekunden > 0) {
                return formatTimeRemaining(start.restSekunden);
            }
        }
        return "";
    }

    public String getTextBandbreite() {
        // start.bandbreite -->> bytes per second
        if (start != null) {
            if (start.status >= Start.STATUS_RUN) {
                return BandwidthFormatter.format(start.bandbreite);
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
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME].replace("%n", "")
                    .replace("%p", "");
            pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].replace("%n", "")
                    .replace("%p", "");

            for (DatenProg prog : pSet.getListeProg()) {
                prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME].replace("%n", "")
                        .replace("%p", "");
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
        return befehlsString.replace("**", arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                .replace("%f", arr[DOWNLOAD_URL])
                .replace("%F", arr[DOWNLOAD_URL_RTMP])
                .replace("%a", arr[DOWNLOAD_ZIEL_PFAD])
                .replace("%b", arr[DOWNLOAD_ZIEL_DATEINAME])
                .replace("%w", websiteUrl);
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
                path = StandardLocations.getStandardDownloadPath();
            } else {
                path = pSet.getZielPfad();
            }

            if (abo != null) {
                //Abos: den Namen des Abos eintragen
                arr[DatenDownload.DOWNLOAD_ABO] = abo.getName();
                if (Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN])) {
                    //und Abopfad an den Pfad anhängen
                    path = GuiFunktionen.addsPfad(path, FilenameUtils.removeIllegalCharacters(abo.getZielpfad(), true));
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
            path = StandardLocations.getStandardDownloadPath();
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

        replStr = replStr.replace("%t", getField(film.getThema(), laenge))
                .replace("%T", getField(film.getTitle(), laenge))
                .replace("%s", getField(film.getSender(), laenge));

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
                field = FileUtils.removeExtension(field);
            }

            replStr = replStr.replace("%N", field);
        } else
            replStr = replStr.replace("%N", getField(GuiFunktionen.getDateiName(downloadUrl), laenge));

        //Felder mit fester Länge werden immer ganz geschrieben
        replStr = replStr.replace("%D", film.getSendeDatum().isEmpty() ? getHeute_yyyyMMdd() : stripDotsAndColons(datumDrehen(film.getSendeDatum())))
                .replace("%d", film.getSendeZeit().isEmpty() ? getJetzt_HHMMSS() : stripDotsAndColons(film.getSendeZeit()))
                .replace("%H", getHeute_yyyyMMdd())
                .replace("%h", getJetzt_HHMMSS())
                .replace("%1", getDMY(DMYTag.DAY, film.getSendeDatum().isEmpty() ? getHeute_dd_MM_yyy() : film.getSendeDatum()))
                .replace("%2", getDMY(DMYTag.MONTH, film.getSendeDatum().isEmpty() ? getHeute_dd_MM_yyy() : film.getSendeDatum()));

        replStr = replaceYearParameter(replStr, film);

        replStr = replStr.replace("%4", getHMS(HMSTag.HOUR, film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()))
                .replace("%5", getHMS(HMSTag.MINUTE, film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()))
                .replace("%6", getHMS(HMSTag.SECOND, film.getSendeZeit().isEmpty() ? getJetzt_HH_MM_SS() : film.getSendeZeit()))
                .replace("%i", String.valueOf(System.currentTimeMillis()));

        replStr = replaceResolutionParameter(replStr, film);

        replStr = replStr.replace("%S", GuiFunktionen.getSuffixFromUrl(downloadUrl))
                .replace("%Z", getHash(downloadUrl))
                .replace("%z", getHash(downloadUrl) + '.' + GuiFunktionen.getSuffixFromUrl(downloadUrl));

        return replStr;
    }

    protected String replaceResolutionParameter(@NotNull String replStr, @NotNull DatenFilm film) {
        String res = "";
        if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL))) {
            res = "H";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY))) {
            res = "HD";
        } else if (arr[DOWNLOAD_URL].equals(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW))) {
            res = "L";
        }
        return replStr.replace("%q", res);
    }

    /**
     * Replace year parameter either with four digit year or two digit year.
     * @param replStr the original string
     * @param film the film object
     * @return the processed string
     */
    protected String replaceYearParameter(String replStr, DatenFilm film) {
        var datum = film.getSendeDatum().isEmpty() ? getHeute_dd_MM_yyy() : film.getSendeDatum();
        var year = getDMY(DMYTag.YEAR, datum);
        if (replStr.contains(TWO_LETTER_YEAR_PARAMETER)) {
            // two-digit year
            year = year.substring(2);
            return replStr.replace(TWO_LETTER_YEAR_PARAMETER, year);
        } else {
            return replStr.replace(FOUR_LETTER_YEAR_PARAMETER, year);
        }
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
        return LocalTime.now().format(HHMMSS);
    }

    private String getJetzt_HH_MM_SS() {
        return LocalTime.now().format(HH_MM_SS);
    }

    private String getHeute_yyyyMMdd() {
        return LocalDate.now().format(YYYYMMDD);
    }

    protected String getHeute_dd_MM_yyy() {
        return LocalDate.now().format(DATUM_FORMAT);
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
        return GuiFunktionen.getFileNameWithoutExtension(arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
    }

    @Override
    public int compareTo(DatenDownload arg0) {
        int ret;
        if ((ret = sorter.compare(arr[DatenDownload.DOWNLOAD_SENDER], arg0.arr[DatenDownload.DOWNLOAD_SENDER])) == 0) {
            return sorter.compare(arr[DatenDownload.DOWNLOAD_THEMA], arg0.arr[DatenDownload.DOWNLOAD_THEMA]);
        }
        return ret;
    }

    private enum DMYTag {
        DAY,
        MONTH,
        YEAR
    }

    private enum HMSTag {
        HOUR,
        MINUTE,
        SECOND
    }
}
