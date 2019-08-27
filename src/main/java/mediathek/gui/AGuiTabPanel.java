package mediathek.gui;

import mediathek.config.Daten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.table.MVTable;

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
}