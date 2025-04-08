package mediathek.daten.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import mediathek.daten.DatenFilm;
import mediathek.gui.dialog.bookmark.BookmarkDateDiff;

/**
 * Bookmark data definition used to store movies
 * Prepared for Jackson JSON storage
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DatenBookmark {

  private String url;
  private String sender;
  private String titel;
  private String senddate;
  private boolean seen;
  private DatenFilm filmdata;
  private String highQualityUrl;
  private String urlKlein;
  private String note;
  private String expiry;
  private boolean willExpire;

  public DatenBookmark() {
    this.seen = false;
  }

  public DatenBookmark(DatenFilm filmdata) {
    this();
    this.url = filmdata.getUrlNormalQuality();
    this.sender = filmdata.getSender();
    this.titel = filmdata.getTitle();
    this.senddate = filmdata.getSendeDatum();
    this.highQualityUrl = filmdata.getHighQualityUrl();
    this.urlKlein = filmdata.getLowQualityUrl();
    this.filmdata = filmdata;
    this.willExpire = false;
  }

  // Getter/Setter (für Jackson & UI-Zugriff)
  public String getUrl() { return url; }
  public void setUrl(String url) { this.url = url; }

  public String getSender() { return sender; }
  public void setSender(String sender) { this.sender = sender; }

  public String getThema() { return (filmdata != null ? filmdata.getThema() : ""); }
  public void setThema(String thema) {}

  public String getTitel() { return titel; }
  public void setTitel(String titel) { this.titel = titel; }

  public String getDauer() { return (filmdata != null ? filmdata.getFilmLengthAsString() : ""); }
  public void setDauer(String dauer) {}

  public String getDescription() { return (filmdata != null ? filmdata.getDescription() : ""); }
  public void setDescription(String description) {}

  public String getNote() { return note; }
  public void setNote(String note) { this.note = note; }

  public String getExpiry() { return expiry; }
  public void setExpiry(String expiry) {
    this.expiry = expiry;
    willExpire = expiry != null && BookmarkDateDiff.getInstance().diff2Today(expiry) < 4;
  }

  public boolean getSeen() { return seen; }
  public void setSeen(boolean seen) { this.seen = seen; }

  public String getSendDate() { return senddate; }
  public void setSendDate(String senddate) { this.senddate = senddate; }

  public String getHighQualityUrl() { return highQualityUrl; }
  public void setHighQualityUrl(String highQualityUrl) { this.highQualityUrl = highQualityUrl; }

  public String getUrlKlein() { return urlKlein; }
  public void setUrlKlein(String urlKlein) { this.urlKlein = urlKlein; }

  // Zusätzliche Methoden
  @JsonIgnore
  public boolean hasURL() {
    return url != null;
  }

  @JsonIgnore
  public boolean hasWebURL() {
    return filmdata != null && !filmdata.getWebsiteUrl().isEmpty();
  }

  @JsonIgnore
  public boolean isMovie(String url, String sender) {
    return this.url != null && this.sender != null
        && this.url.equals(url) && this.sender.equals(sender);
  }

  @JsonIgnore
  public boolean isNotInFilmList() {
    return this.filmdata == null;
  }

  @JsonIgnore
  public boolean isLiveStream() {
    return (filmdata != null) && filmdata.isLivestream();
  }

  @JsonIgnore
  public void setDatenFilm(DatenFilm filmdata) {
    this.filmdata = filmdata;
  }

  @JsonIgnore
  public DatenFilm getDatenFilm() {
    return filmdata;
  }

  @JsonIgnore
  public String getWebUrl() {
    return (filmdata != null) ? filmdata.getWebsiteUrl() : null;
  }

  @JsonIgnore
  public String getFormattedNote() {
    return (note != null && !note.isEmpty()) ? "\n\nAnmerkung:\n" + note : "";
  }

  @JsonIgnore
  public String getExtendedDescription() {
    String base = String.format("%s - %s", sender, titel);
    String details = getDescription() + getFormattedNote();
    return (expiry != null && !expiry.isEmpty())
        ? String.format("%s (Verfügbar bis %s)\n\n%s", base, expiry, details)
        : String.format("%s\n\n%s", base, details);
  }

  @JsonIgnore
  public DatenFilm getDataAsDatenFilm() {
    DatenFilm film = getDatenFilm();
    if (film == null) {
      film = new DatenFilm();
      film.setThema(getThema());
      film.setTitle(getTitel());
      film.setNormalQualityUrl(getUrl());
      film.setHighQualityUrl(getHighQualityUrl());
      film.setLowQualityUrl(getUrlKlein());
      film.setSender(getSender());
      film.setFilmLength(getDauer());
    }
    return film;
  }

  @JsonIgnore
  public boolean willExpire() {
    return willExpire;
  }
}
