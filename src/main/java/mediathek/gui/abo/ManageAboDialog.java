package mediathek.gui.abo;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.*;
import java.net.URL;

public class ManageAboDialog extends JDialog {
    private final ManageAboPanel aboPanel;
    private final JFXPanel infoPanel = new JFXPanel();
    private Parent infoPane;
    private AboInformationController infoController;

    public ManageAboDialog(Frame owner, Daten daten) {
        super(owner);
        setTitle("Abos verwalten");
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setResizable(true);

        aboPanel = new ManageAboPanel(daten);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(aboPanel, BorderLayout.CENTER);
        contentPane.add(infoPanel, BorderLayout.SOUTH);

        setupControllers();

        pack();
        //restore saved size
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, this, owner);

        EscapeKeyHandler.installHandler(this, this::dispose);
    }

    private void setupControllers() {
        Platform.runLater(() -> {
            try {
                URL url = getClass().getResource("/mediathek/res/programm/fxml/abo_information_panel.fxml");

                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(url);

                infoPane = loader.load();
                /*
                 * controller must be kept in variable for strong ref, otherwise GC will erase controller and therefore
                 * update of abos in dialog will stop working...
                 */
                infoController = loader.getController();

                Scene scene = new Scene(infoPane);
                infoPanel.setScene(scene);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    public ManageAboPanel getAboPanel() {
        return aboPanel;
    }
}
