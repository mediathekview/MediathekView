package mediathek.gui.dialog;

import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.gui.GuiAbo;

import javax.swing.*;
import java.awt.*;

public class ManageAboDialog extends JDialog {
    private final GuiAbo aboPanel;

    public ManageAboDialog(Frame owner, Daten daten) {
        super(owner);
        setTitle("Abos verwalten");
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setResizable(true);

        aboPanel = new GuiAbo(daten, MediathekGui.ui());
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(aboPanel, BorderLayout.CENTER);

        pack();
    }

    public GuiAbo getAboPanel() {
        return aboPanel;
    }
}
