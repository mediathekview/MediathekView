package mediathek.gui.tabs;

import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.table.MVTable;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.util.List;
import java.util.Optional;

public abstract class AGuiTabPanel extends JPanel {
    protected MVTable tabelle;
    protected Daten daten;
    protected MediathekGui mediathekGui;


    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.tabelleNachDatenSchreiben();
        }
    }

    protected void setupTouchBar() {

    }

    /**
     * Get the list of currently selected films.
     * @return List of Films
     */
    protected abstract List<DatenFilm> getSelFilme();

    protected abstract Optional<DatenFilm> getCurrentlySelectedFilm();

    protected void initializeTouchBar() {
        if (SystemUtils.IS_OS_MAC_OSX && TouchBarUtils.isTouchBarSupported()) {
            setupTouchBar();
        }
    }
}