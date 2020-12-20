package mediathek.javafx.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import mediathek.daten.DatenFilm;

/**
 * Bookmark data definition used to store movies
 * @author K. Wich
 * 
 * Note: Prepared for Jackson JSON storage
 */
@JsonIgnoreProperties(ignoreUnknown=true)
public class BookmarkData {

  private String url;
  private String sender;
  private String titel;
  private String senddate;
  private final BooleanProperty seen;
  private DatenFilm filmdata;
  private String highQualityUrl;
  private String urlKlein;
  private String note;
  private String expiry;
  private boolean willExpire;
  
  public BookmarkData() {
    seen = new SimpleBooleanProperty(false);
  }
 
  public BookmarkData(DatenFilm filmdata) {
    this();
    this.url = filmdata.getUrl();
    this.sender = filmdata.getSender();
    this.titel = filmdata.getTitle();
    this.senddate = filmdata.getSendeDatum();
    this.highQualityUrl = filmdata.getUrlHighQuality();
    this.urlKlein = filmdata.getUrlKlein();
    this.filmdata = filmdata; 
    this.willExpire = false;
  }

  // getter/setter used for Jackson
  public String getUrl(){ return this.url; }
  public void   setUrl(String url){ this.url = url; }

  public String getSender(){ return this.sender; }
  public void   setSender(String url){ this.sender = url; }

  public String getThema(){ return (filmdata != null ? filmdata.getThema(): ""); }
  public void   setThema(String url){}

  public String getTitel(){ return this.titel; }
  public void   setTitel(String url){ this.titel = url;}

  public String getDauer(){ return ((filmdata != null) ? filmdata.getDauer(): ""); }
  public void   setDauer(String dauer){}

  public String getDescription(){ return ((filmdata != null) ? filmdata.getDescription(): ""); }
  public void   setDescription(String description){}
  
  public String getNote(){ return this.note; }
  public void   setNote(String note){ this.note = note; }
  
  public String getExpiry(){ return this.expiry; }
  public void   setExpiry(String expiry){ 
    this.expiry = expiry; 
    // Check if expiry is about to happen:
    willExpire = expiry != null && BookmarkDateDiff.getInstance().diff2Today(expiry) < 4;
  }
          
  public boolean getSeen(){ return this.seen.get(); }
  public void   setSeen(boolean seen){ this.seen.set(seen);}

  public String getSendDate(){ return this.senddate; }
  public void   setSendDate(String senddate){ this.senddate = senddate; }
  
  public String getHighQualityUrl(){ return this.highQualityUrl; }
  public void   setHighQualityUrl(String highQualityUrl){ this.highQualityUrl = highQualityUrl;}
  
  public String getUrlKlein() { return urlKlein; }
  public void setUrlKlein(String urlKlein) { this.urlKlein = urlKlein; }
  
  // property access:
  @JsonIgnore
  public BooleanProperty getSeenProperty() { return seen; }
  
  // other methods:
  @JsonIgnore
  public boolean hasURL() {
    return this.url != null;
  }
  
  @JsonIgnore
  public boolean hasWebURL() {
    return (this.filmdata != null && !this.filmdata.getWebsiteLink().isEmpty());
  }

  /**
   * Compare with URL and Sender to get unique movie
   * @param url String
   * @param sender String
   * @return true if equal
   */
  @JsonIgnore
  public boolean isMovie(String url, String sender) {
    return this.url.compareTo(url) == 0 && this.sender.compareTo(sender) == 0;
  }
    
  @JsonIgnore
  public boolean isNotInFilmList() {
    return this.filmdata == null;
  }
  
  @JsonIgnore
  public boolean isLiveStream() {
    return (this.filmdata != null) ? this.filmdata.isLivestream() : false;
  }
    
  @JsonIgnore
  public void setDatenFilm(DatenFilm filmdata) {
    this.filmdata = filmdata;
  }
  
  @JsonIgnore
  public DatenFilm getDatenFilm() {
    return this.filmdata;
  }
  
  @JsonIgnore
  public String getWebUrl() {
    return (this.filmdata != null) ? this.filmdata.getWebsiteLink() : null;
  }

  @JsonIgnore
  public String getFormattedNote() {
    return note != null && !note.isEmpty() ? String.format("\n\nAnmerkung:\n%s", note) : "";
  }
  
  @JsonIgnore
  public String getExtendedDescription() {
    if (expiry != null && !expiry.isEmpty()) {
      return String.format("%s - %s (Verfügbar bis %s)\n\n%s%s", sender, titel, expiry, getDescription(), getFormattedNote());
    }
    else {
      return String.format("%s - %s\n\n%s%s", sender, titel, getDescription(), getFormattedNote());
    }
  }
  
  /**
   * Get either the stored DatenFilm object or a new created from the internal data
   * @return DatenFilm Object
   */
  @JsonIgnore
  public DatenFilm getDataAsDatenFilm() {
    DatenFilm Film = getDatenFilm();
    if (Film == null) { // No reference in in object create new return object
      Film = new DatenFilm();
      Film.setThema(getThema());
      Film.setTitle(getTitel());
      Film.setUrl(getUrl());
      Film.setUrlHighQuality(getHighQualityUrl());
      Film.setUrlKlein(getUrlKlein());
      Film.setSender(getSender());
      Film.setDauer(getDauer());
    }
    return Film;
  }

  /**
   * Check if expiry date is about to expire
   * @return true/false
   *
   * Note: Always returns false if no expiry date is set
   */  
  @JsonIgnore
  public boolean willExpire() {
    return this.willExpire;
  }
}
