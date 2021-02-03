package mediathek.filmeSuchen;

public class ListenerFilmeLadenEvent {

    public String senderUrl;
    public String text;
    public int max;
    public int progress;
    public boolean fehler;

    public ListenerFilmeLadenEvent(String ssender, String ttext, int mmax, int pprogress, boolean ffehler) {
        senderUrl = ssender;
        text = ttext;
        max = mmax;
        progress = pprogress;
        fehler = ffehler;
    }
}
