package mediathek.javafx.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import mediathek.daten.DatenFilm;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Bookmark data definition used to store movies
 *
 * @author K. Wich
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BookmarkData {
    @JsonProperty("seen")
    private final BooleanProperty seen;
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

    public BookmarkData() {
        seen = new SimpleBooleanProperty(false);
        //availableUntil = LocalDate.now();
    }
    public BookmarkData(DatenFilm film) {
        this();
        this.url = film.getUrlNormalQuality();
        this.filmdata = film;
    }

    @SuppressWarnings("unused")
    public LocalDate getBookmarkAdded() {
        return bookmarkAdded;
    }

    public void setBookmarkAdded(LocalDate bookmarkAdded) {
        this.bookmarkAdded = bookmarkAdded;
    }

    public String getFilmHashCode() {
        return filmHashCode;
    }

    public void setFilmHashCode(String filmHashCode) {
        this.filmHashCode = filmHashCode;
        this.url = null; // remove Url if we use the hashcode.
    }

    public LocalDate getAvailableUntil() {
        return availableUntil;
    }

    public void setAvailableUntil(LocalDate availableUntil) {
        this.availableUntil = availableUntil;
    }

    public String getUrl() {
        return this.url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getNote() {
        return this.note;
    }

    public void setNote(String note) {
        this.note = note;
    }

    @JsonIgnore
    public Optional<String> getNoteOptional() {
        return Optional.ofNullable(note);
    }

    public boolean getSeen() {
        return this.seen.get();
    }

    public void setSeen(boolean seen) {
        this.seen.set(seen);
    }

    @JsonIgnore
    public boolean getNotSeen() {
        return !this.seen.get();
    }

    @JsonIgnore
    public BooleanProperty getSeenProperty() {
        return seen;
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
        this.filmdata = filmdata;
    }

    @JsonIgnore
    public String getWebUrl() {
        return (this.filmdata != null) ? this.filmdata.getWebsiteUrl() : null;
    }

    @JsonIgnore
    public String getFormattedNote() {
        return note != null && !note.isEmpty() ? String.format("\n\nNotiz:\n%s", note) : "";
    }

    @JsonIgnore
    public String getExtendedDescription() {
        if (filmdata == null) {
            return "Es wurde kein Filmobjekt mehr in der Filmliste gefunden. --> Ungültiger Eintrag!";
        }
        else {
            return String.format("%s - %s\n\n%s%s", filmdata.getSender(), filmdata.getTitle(), filmdata.getDescription(), getFormattedNote());
        }
    }

    @JsonIgnore
    public Optional<DatenFilm> getDatenFilmOptional() {
        return Optional.ofNullable(filmdata);
    }
}
