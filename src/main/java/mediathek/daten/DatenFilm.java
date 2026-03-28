/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.daten;

import mediathek.config.Config;
import mediathek.daten.abo.DatenAbo;
import mediathek.gui.bookmark.BookmarkData;
import mediathek.tool.FileSize;
import mediathek.tool.FilmSize;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.datum.DatumFilm;
import mediathek.tool.episodes.SeasonEpisode;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

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
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final Logger logger = LogManager.getLogger(DatenFilm.class);
    private final static AtomicInteger FILMNR_GENERATOR = new AtomicInteger(0);
    /**
     * List of countries which can view this film.
     */
    private EnumSet<Country> countrySet;
    private final EnumSet<DatenFilmFlags> flags = EnumSet.noneOf(DatenFilmFlags.class);
    /**
     * File size in MByte
     */
    private final FilmSize filmSize = new FilmSize();
    /**
     * Stores all URLs, some keys may not exist.
     */
    private final EnumMap<MapKeys, Object> dataMap = new EnumMap<>(MapKeys.class);
    /// The date until this film will be available. is set by the film info search worker.
    LocalDate availableUntil;
    /**
     * film date stored IN SECONDS!!!
     */
    private DatumFilm datumFilm = DatumFilm.UNDEFINED_FILM_DATE;
    private String description;
    private String sender = "";
    private String thema = "";
    private String titel = "";
    private String datum = "";
    private String sendeZeit = "";
    /**
     * film duration or film length in seconds.
     */
    private int filmLength;
    private String filmLengthAsString = "";
    private int season = 0;
    private int episode = 0;

    public DatenFilm() {
        dataMap.put(MapKeys.FILM_NR, FILMNR_GENERATOR.getAndIncrement());
    }

    public DatenFilm(@NotNull DatenFilm other) {
        this.datumFilm = other.datumFilm;
        this.filmSize.setSize(other.filmSize.toString());
        this.description = other.description;
        this.sender = other.sender;
        this.thema = other.thema;
        this.titel = other.titel;
        if (other.countrySet != null && !other.countrySet.isEmpty()) {
            this.countrySet = EnumSet.copyOf(other.countrySet);
        }
        this.dataMap.putAll(other.dataMap);
        this.datum = other.datum;
        this.sendeZeit = other.sendeZeit;
        this.filmLength = other.filmLength;
        this.filmLengthAsString = other.filmLengthAsString;
        this.season = other.season;
        this.episode = other.episode;
        this.availableUntil = other.availableUntil;
    }

    /**
     * URLs are considered compressed if they contain a '|'-symbol in the text.
     * They need to be decompressed before use.
     *
     * @param requestedUrl the string to be checked.
     * @return true if url is compressed, false otherwise.
     */
    public static boolean isCompressedUrl(@NotNull String requestedUrl) {
        final int indexPipe = requestedUrl.indexOf(COMPRESSION_MARKER);
        return indexPipe != -1;
    }

    public @Nullable LocalDate getAvailableUntil() {
        return availableUntil;
    }

    public void setAvailableUntil(@Nullable LocalDate availableUntil) {
        this.availableUntil = availableUntil;
    }

    /**
     * Get the filmlength or duration.
     *
     * @return filmlength/duration in seconds, or 0.
     */
    public int getFilmLength() {
        return filmLength;
    }

    /**
     * Set the film's length or duration.
     *
     * @param dauer Input string in format "HH:MM:SS".
     */
    public void setFilmLength(String dauer) {
        filmLength = 0;
        filmLengthAsString = "";
        //bail out early if there is nothing to split...
        if (dauer == null || dauer.isEmpty()) {
            return;
        }
        else {
            final String[] split = dauer.split(":");

            try {
                filmLength += Integer.parseInt(split[0]) * 3600; //hour
                filmLength += Integer.parseInt(split[1]) * 60; //minute
                filmLength += Integer.parseInt(split[2]); //second
            }
            catch (Exception e) {
                filmLength = 0;
            }
        }
    }

    public Optional<DatenAbo> getAboOptional() {
        var abo = getAbo();
        return abo == null ? Optional.empty() : Optional.of(abo);
    }

    public @Nullable DatenAbo getAbo() {
        return (DatenAbo) dataMap.getOrDefault(MapKeys.ABO_DATA, null);
    }

    public void setAbo(@Nullable DatenAbo abo) {
        if (abo == null)
            dataMap.remove(MapKeys.ABO_DATA);
        else
            dataMap.put(MapKeys.ABO_DATA, abo);
    }

    public DatumFilm getDatumFilm() {
        return datumFilm;
    }

    public String getLowQualityUrl() {
        return (String) dataMap.getOrDefault(MapKeys.LOW_QUALITY_URL, "");
    }

    public void setLowQualityUrl(@NotNull String url_low_quality) {
        if (url_low_quality.isEmpty())
            dataMap.remove(MapKeys.LOW_QUALITY_URL);
        else
            dataMap.put(MapKeys.LOW_QUALITY_URL, url_low_quality);
    }

    public String getHighQualityUrl() {
        return (String) dataMap.getOrDefault(MapKeys.HIGH_QUALITY_URL, "");
    }

    public void setHighQualityUrl(@NotNull String urlHd) {
        if (urlHd.isEmpty())
            dataMap.remove(MapKeys.HIGH_QUALITY_URL);
        else {
            if (isCompressedUrl(urlHd)) {
                urlHd = decompressUrl(urlHd);
            }
            dataMap.put(MapKeys.HIGH_QUALITY_URL, urlHd);
        }
    }

    public void setDatumLong(String datumLong) {
        long datum_long;
        try {
            if (!datumLong.isEmpty()) {
                datum_long = Long.parseLong(datumLong);
            }
            else {
                datum_long = 0;
            }
        }
        catch (Exception e) {
            if (Config.isDebugModeEnabled()) {
                logger.error("Failed to parse datum long string: {}", datumLong);
            }
            datum_long = 0;
        }
        dataMap.put(MapKeys.TEMP_DATUM_LONG, datum_long);
    }

    public boolean isTrailerTeaser() {
        return flags.contains(DatenFilmFlags.TRAILER_TEASER);
    }

    public void setTrailerTeaser(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.TRAILER_TEASER);
        }
        else {
            flags.remove(DatenFilmFlags.TRAILER_TEASER);
        }
    }

    /**
     * Returns whether this film entry was already seen before and is hence a duplicate entry.
     *
     * @return true if it was seen before, false otherwise.
     */
    public boolean isDuplicate() {
        return flags.contains(DatenFilmFlags.DUPLICATE);
    }

    /**
     * Set if this film entry has a URL that was seen before.
     *
     * @param duplicate are we a duplicate?
     */
    public void setDuplicate(boolean duplicate) {
        if (duplicate) {
            flags.add(DatenFilmFlags.DUPLICATE);
        }
        else {
            flags.remove(DatenFilmFlags.DUPLICATE);
        }
    }

    public boolean isAudioVersion() {
        return flags.contains(DatenFilmFlags.AUDIO_VERSION);
    }

    public void setAudioVersion(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.AUDIO_VERSION);
        }
        else {
            flags.remove(DatenFilmFlags.AUDIO_VERSION);
        }
    }

    public boolean isPlayList() {
        return flags.contains(DatenFilmFlags.PLAYLIST);
    }

    public void setPlayList(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.PLAYLIST);
        }
        else {
            flags.remove(DatenFilmFlags.PLAYLIST);
        }
    }

    public boolean isSignLanguage() {
        return flags.contains(DatenFilmFlags.SIGN_LANGUAGE);
    }

    public void setSignLanguage(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.SIGN_LANGUAGE);
        }
        else {
            flags.remove(DatenFilmFlags.SIGN_LANGUAGE);
        }
    }

    /**
     * Get the film number.
     * This is used internally for the database id AND
     * for the old MV code that might access it for various stuff.
     * <p>
     * This number is UNUSED and always returns 1 until it is completely removed.
     *
     * @return the original internal film number
     */
    public int getFilmNr() {
        return (int) dataMap.get(MapKeys.FILM_NR);
    }

    /**
     * Get the file size of this film.
     *
     * @return The size in MByte
     */
    public FilmSize getFileSize() {
        return filmSize;
    }

    /**
     * Get the film description.
     *
     * @return the film description.
     */
    public String getDescription() {
        return Objects.requireNonNullElse(description, "");
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

    public String getWebsiteUrl() {
        return (String) dataMap.getOrDefault(MapKeys.WEBSITE_URL, "");
    }

    public void setWebsiteUrl(String link) {
        if (link == null || link.isEmpty()) {
            dataMap.remove(MapKeys.WEBSITE_URL);
        }
        else {
            dataMap.put(MapKeys.WEBSITE_URL, link);
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
        }
        else {
            flags.remove(DatenFilmFlags.NEW_ENTRY);
        }
    }

    public boolean isLivestream() {
        return flags.contains(DatenFilmFlags.LIVESTREAM);
    }

    public void setLivestream(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.LIVESTREAM);
        }
        else {
            flags.remove(DatenFilmFlags.LIVESTREAM);
        }
    }

    public void setBurnedInSubtitles(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.BURNED_IN_SUBTITLES);
        }
        else {
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

    /**
     * Return if the film has a subtitle (URL).
     *
     * @return true if a downloadable subtitle is available.
     */
    public boolean hasSubtitle() {
        return dataMap.containsKey(MapKeys.SUBTITLE_URL);
    }

    /**
     * Return if the film has any sort of subtitle available.
     *
     * @return true if any subtitle is available.
     */
    public boolean hasAnySubtitles() {
        return hasSubtitle() || hasBurnedInSubtitles();
    }

    public void clearCountries() {
        countrySet = null;
    }

    public void addCountry(@NotNull Country country) {
        if (countrySet == null) {
            countrySet = EnumSet.noneOf(Country.class);
        }
        countrySet.add(country);
    }

    public boolean hasCountries() {
        return countrySet != null && !countrySet.isEmpty();
    }

    public boolean hasCountry(@NotNull Country country) {
        return countrySet != null && countrySet.contains(country);
    }

    public boolean isGeoBlockedForLocation(@NotNull Country location) {
        if (!hasCountries()) {
            return false;
        }
        if (hasCountry(Country.EU)) {
            return !(hasCountry(location) || Country.EU_COUNTRIES.contains(location));
        }
        return !hasCountry(location);
    }

    public String getCountriesAsString() {
        if (countrySet == null || countrySet.isEmpty()) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        var iterator = countrySet.iterator();
        sb.append(iterator.next());
        while (iterator.hasNext()) {
            sb.append('-').append(iterator.next());
        }
        return sb.toString();
    }

    //TODO This function might not be necessary as getUrlNormalOrRequested does almost the same
    public String getUrlFuerAufloesung(FilmResolution.Enum resolution) {
        return switch (resolution) {
            case LOW, HIGH_QUALITY -> getUrlNormalOrRequested(resolution);
            default -> getUrlNormalQuality();
        };
    }

    public String getFileSizeForUrl(@NotNull String url) {
        if (url.equalsIgnoreCase(getUrlNormalQuality())) {
            return getFileSize().toString();
        }
        else {
            //FIXME this is blocking EDT!
            return FileSize.getFileLengthFromUrl(url);
        }
    }

    /**
     * Return a unique index for comparison during updating the filmlist from diff.
     *
     * @return a unique hash
     */
    public String getSha256() {
        try {
            var digest = MessageDigest.getInstance("SHA-256");
            digest.update(getSender().getBytes(StandardCharsets.UTF_16LE));
            digest.update(getThema().getBytes(StandardCharsets.UTF_16LE));
            digest.update(getUrlNormalQuality().getBytes(StandardCharsets.UTF_16LE));
            digest.update(getWebsiteUrl().getBytes(StandardCharsets.UTF_16LE));
            return HexFormat.of().formatHex(digest.digest());
        }
        catch (NoSuchAlgorithmException e) {
            logger.error("Failed to get SHA-256 hash for film: {}", this, e);
            throw new IllegalStateException("SHA-256 algorithm is unavailable", e);
        }
    }

    /**
     * film entry contains non-empty HQ url.
     *
     * @return true if HQ url is not empty.
     */
    public boolean isHighQuality() {
        return dataMap.containsKey(MapKeys.HIGH_QUALITY_URL);
    }

    public boolean hasLowQuality() {
        return dataMap.containsKey(MapKeys.LOW_QUALITY_URL);
    }

    @Override
    public int compareTo(@NotNull DatenFilm other) {
        int ret;
        if ((ret = sorter.compare(getSender(), other.getSender())) == 0) {
            return sorter.compare(getThema(), other.getThema());
        }
        return ret;
    }

    private void setupDatumFilm() {
        if (!getSendeDatum().isEmpty()) {
            // nur dann gibts ein Datum
            long datum_long = (long) dataMap.getOrDefault(MapKeys.TEMP_DATUM_LONG, 0L);
            if (datum_long == 0) {
                setSendeDatum("");
                setSendeZeit("");
                datumFilm = new DatumFilm(0);
                dataMap.remove(MapKeys.TEMP_DATUM_LONG);
            }
            else {
                datumFilm = new DatumFilm(TimeUnit.MILLISECONDS.convert(datum_long, TimeUnit.SECONDS));
                dataMap.put(MapKeys.TEMP_DATUM_LONG, datum_long);
            }
        }
    }

    public void init() {
        setupDatumFilm();
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
                if (isCompressedUrl(requestedUrl))
                    ret = decompressUrl(requestedUrl);
                else
                    ret = requestedUrl;
            }
            catch (Exception e) {
                ret = "";
                logger.error("getUrlNormalOrRequested(auflösung: {}, requestedUrl: {})", resolution, requestedUrl, e);
            }
        }

        return ret;
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
            case HIGH_QUALITY -> getHighQualityUrl();
            case LOW -> getLowQualityUrl();
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

    /**
     * Get the film's length or duration formatted as a string in "HH:MM:SS" format.
     * Similar to {@link DatenFilm#getFilmLength()}.
     *
     * @return film length or duration as String.
     */
    public String getFilmLengthAsString() {
        if (filmLength == 0)
            return "";
        else if (filmLengthAsString.isEmpty()) {
            var duration = TimeUnit.MILLISECONDS.convert(filmLength, TimeUnit.SECONDS);
            filmLengthAsString = DurationFormatUtils.formatDuration(duration, "HH:mm:ss", true);
        }
        return filmLengthAsString;
    }

    public String getUrlNormalQuality() {
        return (String) dataMap.getOrDefault(MapKeys.NORMAL_QUALITY_URL, "");
    }

    public void setNormalQualityUrl(@NotNull String url_normal_quality) {
        if (url_normal_quality.isEmpty())
            dataMap.remove(MapKeys.NORMAL_QUALITY_URL);
        else
            dataMap.put(MapKeys.NORMAL_QUALITY_URL, url_normal_quality);
    }

    public String getSubtitleUrl() {
        return (String) dataMap.getOrDefault(MapKeys.SUBTITLE_URL, "");
    }

    public void setSubtitleUrl(@NotNull String urlSubtitle) {
        if (urlSubtitle.isEmpty())
            dataMap.remove(MapKeys.SUBTITLE_URL);
        else {
            dataMap.put(MapKeys.SUBTITLE_URL, urlSubtitle);
        }
    }

    /**
     * Get bookmark entry
     *
     * @return BookmarkData entry
     */
    public @Nullable BookmarkData getBookmark() {
        return (BookmarkData) dataMap.getOrDefault(MapKeys.BOOKMARK_DATA, null);
    }

    /**
     * Link with bookmark entry
     *
     * @param bookmark Bookmark entry
     */
    public void setBookmark(@Nullable BookmarkData bookmark) {
        if (bookmark == null)
            dataMap.remove(MapKeys.BOOKMARK_DATA);
        else
            dataMap.put(MapKeys.BOOKMARK_DATA, bookmark);
    }

    /**
     * check if movie is bookmarked
     *
     * @return boolean true
     */
    public boolean isBookmarked() {
        return dataMap.containsKey(MapKeys.BOOKMARK_DATA);
    }

    /// store the associated seaon episode data
    public void setSeasonEpisode(SeasonEpisode seasonEpisode) {
            season = seasonEpisode.season();
            episode = seasonEpisode.episode();
    }

    public int getSeason() {
        return season;
    }

    public int getEpisode() {
        return episode;
    }

    enum MapKeys {
        FILM_NR,
        SUBTITLE_URL,
        WEBSITE_URL,
        LOW_QUALITY_URL,
        NORMAL_QUALITY_URL,
        HIGH_QUALITY_URL,
        BOOKMARK_DATA,
        ABO_DATA,
        TEMP_DATUM_LONG
    }
}
