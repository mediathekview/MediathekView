package mediathek.gui;

import mediathek.config.Daten;

import javax.swing.*;

@SuppressWarnings("serial")
public abstract class PanelVorlage extends JPanel {
    public Daten daten;
    public boolean stopBeob = false;
    public JFrame parentComponent;

    public PanelVorlage(Daten d, JFrame pparentComponent) {
        daten = d;
        parentComponent = pparentComponent;
    }
}
