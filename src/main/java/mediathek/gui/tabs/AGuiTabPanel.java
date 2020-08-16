package mediathek.gui.tabs;

import mediathek.config.Daten;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.table.MVTable;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;

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

    protected void initializeTouchBar() {
        if (SystemUtils.IS_OS_MAC_OSX && TouchBarUtils.isTouchBarSupported()) {
            setupTouchBar();
        }
    }
}