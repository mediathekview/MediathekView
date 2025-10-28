/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import mediathek.daten.DatenFilm;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Bookmark data definition used to store movies
 *
 * @author K. Wich
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BookmarkData {
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);
    @JsonProperty("seen")
    private boolean seen;
    private String url;
    @JsonIgnore
    private DatenFilm filmdata;
    private String note;
    private LocalDate availableUntil;
    /// will be added from [BookmarkDataList#checkAndBookmarkMovies(List)]
    private LocalDate bookmarkAdded;
    /// The SHA256 hashcode from the film object used to create the bookmark.
    /// This is the *correct* way to store film object info as it will be unique.
    ///
    /// Must be converted back to [com.google.common.hash.HashCode].
    /// Will erase [BookmarkData#url] as it is unneeded then.
    private String filmHashCode;
    private String originalSender;
    private String originalTitle;
    private String originalThema;

    public BookmarkData() {
        seen = false;
        //availableUntil = LocalDate.now();
    }

    public BookmarkData(DatenFilm film) {
        this();
        this.url = film.getUrlNormalQuality();
        this.filmdata = film;
        this.originalSender = film.getSender();
        this.originalTitle = film.getTitle();
        this.originalThema = film.getThema();
    }

    public String getOriginalSender() {
        return originalSender;
    }

    public void setOriginalSender(String originalSender) {
        this.originalSender = originalSender;
    }

    public String getOriginalThema() {
        return originalThema;
    }

    public void setOriginalThema(String originalThema) {
        this.originalThema = originalThema;
    }

    public String getOriginalTitle() {
        return originalTitle;
    }

    public void setOriginalTitle(String originalTitle) {
        this.originalTitle = originalTitle;
    }

    @JsonIgnore
    public String getSender() {
        if (filmdata != null) {
            return filmdata.getSender();
        }
        else
            return Objects.requireNonNullElse(originalSender, "NO SENDER");
    }

    @JsonIgnore
    public String getThema() {
        if (filmdata != null) {
            return filmdata.getThema();
        }
        else
            return Objects.requireNonNullElse(originalThema, "NO THEMA");
    }

    @JsonIgnore
    public String getTitle() {
        if (filmdata != null) {
            return filmdata.getTitle();
        }
        else
            return Objects.requireNonNullElse(originalTitle, "NO TITLE");
    }

    @JsonIgnore
    public int getDauer() {
        if (filmdata != null) {
            return filmdata.getFilmLength();
        }
        else
            return -1;
    }

    @JsonIgnore
    public Date getSendedatum() {
        if (filmdata != null) {
            return filmdata.getDatumFilm();
        }
        else
            return null;
    }

    @SuppressWarnings("unused")
    public LocalDate getBookmarkAdded() {
        return bookmarkAdded;
    }

    public void setBookmarkAdded(LocalDate bookmarkAdded) {
        LocalDate oldDate = this.bookmarkAdded;
        this.bookmarkAdded = bookmarkAdded;
        support.firePropertyChange("bookmarkAdded", oldDate, bookmarkAdded);
    }

    public String getFilmHashCode() {
        return filmHashCode;
    }

    public void setFilmHashCode(String filmHashCode) {
        String oldHash = this.filmHashCode;
        this.filmHashCode = filmHashCode;
        this.url = null; // remove Url if we use the hashcode.
        support.firePropertyChange("hashCode", oldHash, filmHashCode);
    }

    public LocalDate getAvailableUntil() {
        return availableUntil;
    }

    public void setAvailableUntil(LocalDate availableUntil) {
        LocalDate oldDate = this.availableUntil;
        this.availableUntil = availableUntil;
        support.firePropertyChange("availableUntil", oldDate, availableUntil);
    }

    public String getUrl() {
        return this.url;
    }

    public void setUrl(String url) {
        String oldUrl = this.url;
        this.url = url;
        support.firePropertyChange("url", oldUrl, url);
    }

    @JsonIgnore
    public String getNormalQualityUrl() {
        if (filmdata != null) {
            return filmdata.getUrlNormalQuality();
        }
        else
            return null;
    }

    public String getNote() {
        return this.note;
    }

    public void setNote(String note) {
        String oldNote = this.note;
        this.note = note;
        support.firePropertyChange("note", oldNote, note);
    }

    public void addPropertyChangeListener(PropertyChangeListener l) {
        support.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
        support.removePropertyChangeListener(l);
    }

    @JsonIgnore
    public Optional<String> getNoteOptional() {
        return Optional.ofNullable(note);
    }

    public boolean getSeen() {
        return this.seen;
    }

    public void setSeen(boolean seen) {
        boolean oldSeen = this.seen;
        this.seen = seen;
        support.firePropertyChange("seen", oldSeen, seen);
    }

    @JsonIgnore
    public boolean getNotSeen() {
        return !this.seen;
    }

    @JsonIgnore
    public boolean isNotInFilmList() {
        return this.filmdata == null;
    }

    @JsonIgnore
    public DatenFilm getDatenFilm() {
        return this.filmdata;
    }

    @JsonIgnore
    public void setDatenFilm(DatenFilm filmdata) {
        DatenFilm oldFilm = this.filmdata;
        this.filmdata = filmdata;
        support.firePropertyChange("datenFilm", oldFilm, filmdata);
    }

    @JsonIgnore
    public String getWebUrl() {
        return (this.filmdata != null) ? this.filmdata.getWebsiteUrl() : null;
    }

    @JsonIgnore
    public Optional<DatenFilm> getDatenFilmOptional() {
        return Optional.ofNullable(filmdata);
    }
}
