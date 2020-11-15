package mediathek.gui;

import mediathek.config.Daten;

import javax.swing.*;

@SuppressWarnings("serial")
public abstract class PanelVorlage extends JPanel {
    protected Daten daten;
    protected boolean stopBeob;
    protected JFrame parentComponent;

    public PanelVorlage(Daten daten, JFrame pparentComponent) {
        this.daten = daten;
        parentComponent = pparentComponent;
    }
}
