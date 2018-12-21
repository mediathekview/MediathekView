package mediathek.gui.dialog;

import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.GuiAbo;
import mediathek.tool.GuiFunktionen;

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
        //restore saved size
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, this, owner);
    }

    public GuiAbo getAboPanel() {
        return aboPanel;
    }
}
