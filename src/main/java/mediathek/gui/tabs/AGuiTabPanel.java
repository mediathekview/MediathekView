package mediathek.gui.tabs;

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.Optional;

public abstract class AGuiTabPanel extends JPanel {
    protected Daten daten;
    protected MediathekGui mediathekGui;

    public abstract void tabelleSpeichern();

    /**
     * Get the list of currently selected films.
     *
     * @return List of Films
     */
    protected abstract List<DatenFilm> getSelFilme();

    protected abstract Optional<DatenFilm> getCurrentlySelectedFilm();

    public abstract void installMenuEntries(JMenu menu);

    public class MarkFilmAsSeenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            var listFilms = getSelFilme();
            try (var controller = new SeenHistoryController()) {
                controller.markSeen(listFilms);
            }
        }
    }

    public class MarkFilmAsUnseenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            var listFilms = getSelFilme();
            try (var controller = new SeenHistoryController()) {
                controller.markUnseen(listFilms);
            }
        }
    }
}