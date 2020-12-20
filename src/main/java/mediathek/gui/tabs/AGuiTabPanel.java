package mediathek.gui.tabs;

import com.thizzer.jtouchbar.JTouchBar;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.table.MVTable;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.Optional;

public abstract class AGuiTabPanel extends JPanel {
    protected MVTable tabelle;
    protected Daten daten;
    protected MediathekGui mediathekGui;
    protected JTouchBar touchBar;

    public abstract void showTouchBar();
    public abstract void hideTouchBar();

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.tabelleNachDatenSchreiben();
        }
    }

    protected void setupTouchBar() {

    }

    /**
     * Get the list of currently selected films.
     *
     * @return List of Films
     */
    protected abstract List<DatenFilm> getSelFilme();

    protected abstract Optional<DatenFilm> getCurrentlySelectedFilm();

    public abstract void installMenuEntries(JMenu menu);
    protected abstract void installTabInfoStatusBarControl();

    protected void initializeTouchBar() {
        if (SystemUtils.IS_OS_MAC_OSX && TouchBarUtils.isTouchBarSupported()) {
            setupTouchBar();
        }
    }

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