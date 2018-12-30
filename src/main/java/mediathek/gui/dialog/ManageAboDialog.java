package mediathek.gui.dialog;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.GuiAbo;
import mediathek.javafx.AboInformationPanel;
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

        aboPanel = new GuiAbo(daten);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(aboPanel, BorderLayout.CENTER);

        AboInformationPanel infoLabel = new AboInformationPanel(daten);
        JFXPanel infoPanel = new JFXPanel();
        contentPane.add(infoPanel,BorderLayout.SOUTH);
        Platform.runLater(() -> {
            Scene scene = new Scene(infoLabel);
            infoPanel.setScene(scene);
        });


        pack();
        //restore saved size
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, this, owner);
    }

    public GuiAbo getAboPanel() {
        return aboPanel;
    }
}
