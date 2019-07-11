package mSearch.filmeSuchen;

public class ListenerFilmeLadenEvent {

    public String senderUrl;
    public String text;
    public int max;
    public int progress;
    public boolean fehler;
    public int count;

    public ListenerFilmeLadenEvent(String ssender, String ttext, int mmax, int pprogress, int ccount, boolean ffehler) {
        senderUrl = ssender;
        text = ttext;
        max = mmax;
        progress = pprogress;
        count = ccount;
        fehler = ffehler;
    }
}
