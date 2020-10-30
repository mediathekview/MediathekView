package mediathek.gui.tabs.tab_film;

import mediathek.daten.DatenPset;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public final class BeobOpenPlayer implements ActionListener {
    //ext. Programme starten

    private final GuiFilme guiFilme;
    DatenPset pset;

    public BeobOpenPlayer(GuiFilme guiFilme, DatenPset p) {
        this.guiFilme = guiFilme;
        pset = p;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiFilme.playerStarten(pset);
    }
}
