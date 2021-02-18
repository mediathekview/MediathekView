package mediathek.gui.tabs.tab_film;

import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.net.URL;

class SaveDownloadDialog extends JDialog {
    public SaveDownloadController controller;

    public SaveDownloadDialog(DatenFilm datenFilm, DatenPset pSet) {
        setTitle("FX TestDialog");
        setModal(true);
        JFXPanel fxPanel = new JFXPanel();

        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(fxPanel, BorderLayout.CENTER);
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            try {
                URL url = getClass().getResource("/mediathek/res/programm/fxml/save_download_dialog.fxml");
                FXMLLoader fxmlLoader = new FXMLLoader(url);
                controller = new SaveDownloadController(this, datenFilm, pSet);
                fxmlLoader.setController(controller);
                BorderPane p = fxmlLoader.load();
                fxPanel.setScene(new Scene(p));
            } catch (IOException e) {
                e.printStackTrace();
            }
            SwingUtilities.invokeLater(() -> {
                pack();
                GuiFunktionen.centerOnScreen(this, false);
            });
        });
    }
}
