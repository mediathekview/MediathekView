package mSearch.filmeSuchen;

import java.util.EventListener;

public class ListenerFilmeLaden implements EventListener {
    public void start(ListenerFilmeLadenEvent e) {
    }

    public void progress(ListenerFilmeLadenEvent e) {
    }

    public void fertig(ListenerFilmeLadenEvent e) {
    }

    public void fertigOnlyOne(ListenerFilmeLadenEvent e) {
        // dient dem Melden des ersten Mal Laden der Filmliste beim ProgStart
    }
}
