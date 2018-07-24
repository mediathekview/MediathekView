package mediathek.daten;

import java.net.URL;

/**
 * Contains all necessary entries for a livestream item.
 */
public class LiveStreamItem {
    private String sender;
    private String titel;
    private URL url;

    public String getSender() {
        return sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public String getTitel() {
        return titel;
    }

    public void setTitel(String titel) {
        this.titel = titel;
    }

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

    public String toString() {
        return getSender() + "-" + getTitel() + "---" + getUrl().toString();
    }
}
