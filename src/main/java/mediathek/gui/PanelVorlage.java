package mediathek.gui;

import mediathek.config.Daten;

import javax.swing.*;

public abstract class PanelVorlage extends JPanel {
    protected final Daten daten;
    protected boolean stopBeob;
    protected final JFrame parentComponent;

    public PanelVorlage(Daten daten, JFrame pparentComponent) {
        this.daten = daten;
        parentComponent = pparentComponent;
    }
}
