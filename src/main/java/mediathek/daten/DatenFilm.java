package mediathek.daten;

import mediathek.daten.abo.DatenAbo;
import mediathek.javafx.bookmark.BookmarkData;
import mediathek.tool.FileSize;
import mediathek.tool.FilmSize;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.datum.DatumFilm;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.EnumSet;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

/*
 * TODO:
 * - Remove the Database Stuff from this Class to own Classes and a real OR-Mapping
 * - Finalize a Real Entity
 * - Write test cases for each Method
 * - Write JavaDoc for each of the new Methods that were split from this moloch
 */

public class DatenFilm implements Comparable<DatenFilm> {
    public static final int FILM_NR = 0;      // wird vor dem Speichern gelöscht!
    public static final int FILM_SENDER = 1;
    public static final int FILM_THEMA = 2;
    public static final int FILM_TITEL = 3;
    public static final int FILM_ABSPIELEN = 4; // no getter/setter access
    public static final int FILM_AUFZEICHNEN = 5; // no getter/setter access
    public static final int FILM_MERKEN = 6; // no getter/setter access
    public static final int FILM_DATUM = 7;
    public static final int FILM_ZEIT = 8;
    public static final int FILM_DAUER = 9;
    public static final int FILM_GROESSE = 10;
    public static final int FILM_HD = 11; // no getter/setter access
    public static final int FILM_UT = 12; // no getter/setter access
    public static final int FILM_GEO = 13; // Geoblocking
    public static final int FILM_URL = 14;
    /**
     * Index for Date as long value in SECONDS!!
     */
    public static final int FILM_DATUM_LONG = 15;
    public static final int FILM_REF = 16; // no getter/setter access // Referenz auf this
    public static final int MAX_ELEM = 17;
    /**
     * Compressed URLs are missing the base normal quality URL and are indicated by the pipe-symbol.
     */
    public static final char COMPRESSION_MARKER = '|';
    /**
     * The database instance for all descriptions.
     */
    private final static AtomicInteger FILM_COUNTER = new AtomicInteger(0);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final Logger logger = LogManager.getLogger(DatenFilm.class);
    private final EnumSet<DatenFilmFlags> flags = EnumSet.noneOf(DatenFilmFlags.class);
    /**
     * Internal film number, used for storage in database
     */
    private final int databaseFilmNumber;
    private DatenAbo abo;
    private BookmarkData bookmark;
    /**
     * film date stored IN SECONDS!!!
     */
    private DatumFilm datumFilm = DatumFilm.UNDEFINED_FILM_DATE;
    /**
     * File size in MByte
     */
    private FilmSize filmSize;
    /**
     * film length in seconds.
     */
    private long filmLength;
    private String websiteLink;
    private String description;
    /**
     * Low quality URL.
     */
    private String url_low_quality = "";
    /**
     * High Quality (formerly known as HD) URL if available.
     */
    private Optional<String> url_high_quality = Optional.empty();
    private String datumLong = "";
    private String sender = "";
    private String thema = "";
    private String titel = "";
    /**
     * String of countries where this entry can be viewed, if available.
     * Empty means viewable without restrictions.
     */
    private Optional<String> availableInCountries = Optional.empty();
    /**
     * URL to the subtitle file, if available.
     */
    private Optional<String> subtitle_url = Optional.empty();
    private String datum = "";
    private String sendeZeit = "";
    private String dauer = "";
    private String groesse = "";
    /**
     * Normal quality URL.
     */
    private String url_normal_quality = "";
    /**
     * film duration in seconds.
     * getDauer() stores the same info as a String
     */
    private int duration;

    public DatenFilm() {
        filmSize = new FilmSize(0); // Dateigröße in MByte
        databaseFilmNumber = FILM_COUNTER.getAndIncrement();
    }

    public DatenFilm(@NotNull DatenFilm other) {
        this.abo = other.abo;
        this.bookmark = other.bookmark;
        this.datumFilm = other.datumFilm;
        this.filmSize = other.filmSize;
        this.filmLength = other.filmLength;
        this.databaseFilmNumber = other.databaseFilmNumber;
        this.websiteLink = other.websiteLink;
        this.description = other.description;
        this.url_low_quality = other.url_low_quality;
        this.url_high_quality = other.url_high_quality;
        this.datumLong = other.datumLong;
        this.sender = other.sender;
        this.thema = other.thema;
        this.titel = other.titel;
        this.availableInCountries = other.availableInCountries;
        this.subtitle_url = other.subtitle_url;
        this.datum = other.datum;
        this.sendeZeit = other.sendeZeit;
        this.dauer = other.dauer;
        this.groesse = other.groesse;
        this.url_normal_quality = other.url_normal_quality;
        this.duration = other.duration;
    }

    public int getDuration() {
        return duration;
    }

    public @Nullable DatenAbo getAbo() {
        return abo;
    }

    public void setAbo(DatenAbo abo) {
        this.abo = abo;
    }

    public DatumFilm getDatumFilm() {
        return datumFilm;
    }

    public String getUrlLowQuality() {
        return url_low_quality;
    }

    public void setUrlLowQuality(String url_low_quality) {
        this.url_low_quality = url_low_quality;
    }

    public String getUrlHighQuality() {
        return url_high_quality.orElse("");
    }

    public void setUrlHighQuality(String urlHd) {
        if (!urlHd.isEmpty())
            url_high_quality = Optional.of(urlHd);
        else
            url_high_quality = Optional.empty();
    }

    public String getDatumLong() {
        return datumLong;
    }

    public void setDatumLong(String datumLong) {
        this.datumLong = datumLong;
    }

    public boolean isTrailerTeaser() {
        return flags.contains(DatenFilmFlags.TRAILER_TEASER);
    }

    public void setTrailerTeaser(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.TRAILER_TEASER);
        } else {
            flags.remove(DatenFilmFlags.TRAILER_TEASER);
        }
    }

    public boolean isAudioVersion() {
        return flags.contains(DatenFilmFlags.AUDIO_VERSION);
    }

    public void setAudioVersion(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.AUDIO_VERSION);
        } else {
            flags.remove(DatenFilmFlags.AUDIO_VERSION);
        }
    }

    public boolean isPlayList() {
        return flags.contains(DatenFilmFlags.PLAYLIST);
    }

    public void setPlayList(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.PLAYLIST);
        } else {
            flags.remove(DatenFilmFlags.PLAYLIST);
        }
    }

    public boolean isSignLanguage() {
        return flags.contains(DatenFilmFlags.SIGN_LANGUAGE);
    }

    public void setSignLanguage(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.SIGN_LANGUAGE);
        } else {
            flags.remove(DatenFilmFlags.SIGN_LANGUAGE);
        }
    }

    /**
     * Get the film number.
     * This is used internally for the database id AND
     * for the old MV code that might access it for various stuff.
     *
     * @return the original internal film number
     */
    public int getFilmNr() {
        return databaseFilmNumber;
    }

    /**
     * Get the file size of this film.
     *
     * @return The size in MByte
     */
    public FilmSize getFilmSize() {
        return filmSize;
    }

    /**
     * Get the film description.
     *
     * @return the film description.
     */
    public String getDescription() {
        return StringUtils.defaultString(description);
    }

    /**
     * Store film description.
     *
     * @param desc String to be stored.
     */
    public void setDescription(final String desc) {
        if (desc != null && !desc.isEmpty()) {
            description = desc;
        }
    }

    public String getWebsiteLink() {
        return StringUtils.defaultString(websiteLink);
    }

    public void setWebsiteLink(String link) {
        if (link != null && !link.isEmpty()) {
            websiteLink = link;
        }
    }

    /**
     * Indicate whether this entry has been in the filmlist before.
     *
     * @return true if it is a new entry, false otherwise.
     */
    public boolean isNew() {
        return flags.contains(DatenFilmFlags.NEW_ENTRY);
    }

    public void setNew(final boolean newFilm) {
        if (newFilm) {
            flags.add(DatenFilmFlags.NEW_ENTRY);
        } else {
            flags.remove(DatenFilmFlags.NEW_ENTRY);
        }
    }

    public boolean isLivestream() {
        return flags.contains(DatenFilmFlags.LIVESTREAM);
    }

    public void setLivestream(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.LIVESTREAM);
        } else {
            flags.remove(DatenFilmFlags.LIVESTREAM);
        }
    }

    public void setBurnedInSubtitles(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.BURNED_IN_SUBTITLES);
        } else {
            flags.remove(DatenFilmFlags.BURNED_IN_SUBTITLES);
        }
    }

    /**
     * Indicate if the film has encoded aka. "burned in" subtitles"
     *
     * @return true if they are burned in, false othewise.
     */
    public boolean hasBurnedInSubtitles() {
        return flags.contains(DatenFilmFlags.BURNED_IN_SUBTITLES);
    }

    public boolean hasSubtitle() {
        return subtitle_url.isPresent();
    }

    //TODO This function might not be necessary as getUrlNormalOrRequested does almost the same
    public String getUrlFuerAufloesung(FilmResolution.Enum resolution) {
        return switch (resolution) {
            case LOW, HIGH_QUALITY -> getUrlNormalOrRequested(resolution);
            default -> getUrlNormalQuality();
        };
    }

    public String getDateigroesse(String url) {
        if (url.equalsIgnoreCase(getUrlNormalQuality())) {
            return getSize();
        } else {
            return FileSize.getFileLengthFromUrl(url);
        }
    }

    /**
     * Return a unique index for comparison during updating the filmlist from diff.
     *
     * @return a unique "hash" string
     */
    public String getUniqueHash() {
        return (getSender() + getThema()).toLowerCase() + getUrlNormalQuality() + getWebsiteLink();
    }

    /**
     * film entry contains non-empty HQ url.
     *
     * @return true if HQ url is not empty.
     */
    public boolean isHighQuality() {
        return url_high_quality.isPresent();
    }

    @Override
    public int compareTo(@NotNull DatenFilm other) {
        int ret;
        if ((ret = sorter.compare(getSender(), other.getSender())) == 0) {
            return sorter.compare(getThema(), other.getThema());
        }
        return ret;
    }

    /**
     * Get the filmlength in seconds.
     *
     * @return filmlength in seconds, or 0.
     */
    public long getFilmLength() {
        return filmLength;
    }

    /**
     * Convert HH:MM:SS string into seconds.
     * Or set to 0 in case of error.
     *
     * @return result in seconds or 0.
     */
    private long parseTimeToSeconds() {
        long seconds = 0;
        final String[] split = StringUtils.split(dauer, ':');
        // if empty, don't try to split and return early...
        if (split == null || split.length == 0) {
            return 0;
        }
        else {
            try {
                seconds += Long.parseLong(split[0]) * 3600; //hour
                seconds += Long.parseLong(split[1]) * 60; //minute
                seconds += Long.parseLong(split[2]); //second
            } catch (Exception e) {
                seconds = 0;
            }

            return seconds;
        }
    }

    private void setDatum() {
        if (!getSendeDatum().isEmpty()) {
            // nur dann gibts ein Datum
            try {
                final long l = Long.parseLong(getDatumLong());
                datumFilm = new DatumFilm(l * 1000); // sind SEKUNDEN!!
            } catch (Exception ex) {
                logger.error("Datum: {}, Zeit: {}, Datum_LONG: {}", getSendeDatum(), getSendeZeit(), getDatumLong(), ex);
                datumFilm = new DatumFilm(0);
                setSendeDatum("");
                setSendeZeit("");
            }
        }
    }

    public void init() {
        filmSize = new FilmSize(this);
        filmLength = parseTimeToSeconds();

        setDatum();
    }

    /**
     * Return unpacked url as string.
     * High quality URLs may be "compressed" in the filmlist and need to be unpacked before use.
     *
     * @param resolution One of FilmResolution.HIGH_QUALITY,FilmResolution.LOW,FilmResolution.NORMAL.
     * @return An unpacked version of the film url as string.
     */
    private String getUrlNormalOrRequested(@NotNull FilmResolution.Enum resolution) {
        String ret;
        // liefert die kleine normale URL oder die HD URL
        final String requestedUrl = getUrlByResolution(resolution);
        if (requestedUrl.isEmpty())
            ret = getUrlNormalQuality();
        else {
            try {
                if (isUrlCompressed(requestedUrl))
                    ret = decompressUrl(requestedUrl);
                else
                    ret = requestedUrl;
            } catch (Exception e) {
                ret = "";
                logger.error("getUrlNormalOrRequested(auflösung: {}, requestedUrl: {})", resolution, requestedUrl, e);
            }
        }

        return ret;
    }

    /**
     * URLs are considered compressed if they contain a '|'-symbol in the text.
     * They need to be decompressed before use.
     * @param requestedUrl the string to be checked.
     * @return true if url is compressed, false otherwise.
     */
    public static boolean isUrlCompressed(@NotNull String requestedUrl) {
        final int indexPipe = requestedUrl.indexOf(COMPRESSION_MARKER);
        return indexPipe != -1;
    }

    public String decompressUrl(@NotNull final String requestedUrl) throws NumberFormatException, IndexOutOfBoundsException {
        final int indexPipe = requestedUrl.indexOf(COMPRESSION_MARKER);
        final int i = Integer.parseInt(requestedUrl.substring(0, indexPipe));
        return getUrlNormalQuality().substring(0, i) + requestedUrl.substring(indexPipe + 1);
    }

    /**
     * Return url based on requested resolution
     *
     * @param resolution One of FilmResolution.AUFLOESUNG_HD,FilmResolution.AUFLOESUNG_KLEIN,FilmResolution.AUFLOESUNG_NORMAL.
     * @return url as String.
     */
    private String getUrlByResolution(@NotNull final FilmResolution.Enum resolution) {
        return switch (resolution) {
            case HIGH_QUALITY -> getUrlHighQuality();
            case LOW -> getUrlLowQuality();
            default -> getUrlNormalQuality();
        };
    }

    public String getSender() {
        return sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public String getThema() {
        return thema;
    }

    public void setThema(String thema) {
        this.thema = thema;
    }

    public String getTitle() {
        return titel;
    }

    public void setTitle(String title) {
        this.titel = title;
    }

    public String getSendeDatum() {
        return datum;
    }

    public void setSendeDatum(String sendeDatum) {
        this.datum = sendeDatum;
    }

    public String getSendeZeit() {
        return sendeZeit;
    }

    public void setSendeZeit(String sendeZeit) {
        this.sendeZeit = sendeZeit;
    }

    public String getDauer() {
        return dauer;
    }

    public void setDauer(String dauer) {
        this.dauer = dauer;

        //bail out early if there is nothing to split...
        if (dauer == null || dauer.isEmpty()) {
            duration = 0;
        }
        else {
            //FIXME gefällt mir nicht
            final String[] split = StringUtils.split(this.dauer, ':');

            try {
                duration += Integer.parseInt(split[0]) * 3600; //hour
                duration += Integer.parseInt(split[1]) * 60; //minute
                duration += Integer.parseInt(split[2]); //second
            } catch (Exception e) {
                duration = 0;
            }
        }
    }

    public String getSize() {
        return groesse;
    }

    public void setSize(String size) {
        this.groesse = size;
    }

    public String getUrlNormalQuality() {
        return url_normal_quality;
    }

    public void setUrlNormalQuality(String url_normal_quality) {
        this.url_normal_quality = url_normal_quality;
    }

    public String getUrlSubtitle() {
        return subtitle_url.orElse("");
    }

    public void setUrlSubtitle(String urlSubtitle) {
        if (!urlSubtitle.isEmpty())
            subtitle_url = Optional.of(urlSubtitle);
        else
            subtitle_url = Optional.empty();
    }

    public Optional<String> getGeo() {
        return availableInCountries;
    }

    public void setGeo(Optional<String> availableInCountries) {
        this.availableInCountries = availableInCountries;
    }

    /**
     * Get bookmark entry
     *
     * @return BookmarkData entry
     */
    public BookmarkData getBookmark() {
        return this.bookmark;
    }

    /**
     * Link with bookmark entry
     *
     * @param bookmark Bookmark entry
     */
    public void setBookmark(BookmarkData bookmark) {
        this.bookmark = bookmark;
    }

    /**
     * check if movie is bookmarked
     *
     * @return boolean true
     */
    public boolean isBookmarked() {
        return this.bookmark != null;
    }
}
