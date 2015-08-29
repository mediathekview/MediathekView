package mediathek.gui.dialog;

import javax.swing.event.ChangeListener;
import msearch.daten.DatenFilm;

/**
 * Display the current film information
 */
public interface MVFilmInfo extends ChangeListener {

    public void showInfo();

    public boolean isVisible();

    public void updateCurrentFilm(DatenFilm film);

}
