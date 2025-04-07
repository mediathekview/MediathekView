package mediathek.daten.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import mediathek.daten.DatenFilm;
import mediathek.javafx.bookmark.BookmarkDateDiff;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DatenBookmark {

    private String url;
    private String sender;
  private String thema;
    private String titel;
  private String sendDate;
    private boolean seen;
    private String highQualityUrl;
    private String urlKlein;
    private String note;
    private String expiry;
    private boolean willExpire;
  private String dauer;

  @JsonIgnore private DatenFilm filmdata;

    public DatenBookmark() {
    this.seen = false;
    }

    public DatenBookmark(DatenFilm filmdata) {
        this();
        this.url = filmdata.getUrlNormalQuality();
    this.thema = filmdata.getThema();
        this.sender = filmdata.getSender();
        this.titel = filmdata.getTitle();
    this.sendDate = filmdata.getSendeDatum();
        this.highQualityUrl = filmdata.getHighQualityUrl();
        this.urlKlein = filmdata.getLowQualityUrl();
        this.filmdata = filmdata;
    this.dauer = filmdata.getFilmLengthAsString();
        this.willExpire = false;
    }

  // --- Getter/Setter für JSON & JTable-Nutzung ---

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
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

  public String getTitel() {
    return titel;
  }

  public void setTitel(String titel) {
    this.titel = titel;
  }

  public String getSendDate() {
    return sendDate;
  }

  public void setSendDate(String sendDate) {
    this.sendDate = sendDate;
  }

  public boolean isSeen() {
    return seen;
  }

  public void setSeen(boolean seen) {
    this.seen = seen;
  }

  public String getHighQualityUrl() {
    return highQualityUrl;
  }

  public void setHighQualityUrl(String highQualityUrl) {
    this.highQualityUrl = highQualityUrl;
  }

    public String getUrlKlein() { return urlKlein; }
    public void setUrlKlein(String urlKlein) { this.urlKlein = urlKlein; }

  public String getNote() {
    return note;
  }

  public void setNote(String note) {
    this.note = note;
  }

  public String getExpiry() {
    return expiry;
  }

  public void setExpiry(String expiry) {
    this.expiry = expiry;
    this.willExpire = expiry != null && BookmarkDateDiff.getInstance().diff2Today(expiry) < 4;
  }

  public String getDauer() {
    return dauer;
  }

  public void setDauer(String dauer) {
    this.dauer = dauer;
  }

  public boolean isWillExpire() {
    return willExpire;
  }

  public void setWillExpire(boolean willExpire) {
    this.willExpire = willExpire;
  }

  // --- Optional, falls DatenFilm benötigt wird ---

  @JsonIgnore
  public DatenFilm getFilmdata() {
    return filmdata;
  }

  @JsonIgnore
  public void setFilmdata(DatenFilm filmdata) {
    this.filmdata = filmdata;
  }

  // --- Hilfsmethoden (nur wenn wirklich gebraucht) ---

  @JsonIgnore
  public boolean hasWebURL() {
    return filmdata != null
        && filmdata.getWebsiteUrl() != null
        && !filmdata.getWebsiteUrl().isEmpty();
    }

  @JsonIgnore
  public boolean isLiveStream() {
    return filmdata != null && filmdata.isLivestream();
    }

    @JsonIgnore
    public String getExtendedDescription() {
    return filmdata.getDescription();
    }
}
