package mediathek.gui;

import javax.swing.event.ChangeListener;
import mSearch.daten.DatenFilm;

/**
 * Display the current film information
 */
public interface MVFilmInfo extends ChangeListener {

    void showInfo();

    boolean isVisible();

    void updateCurrentFilm(DatenFilm film);

}
