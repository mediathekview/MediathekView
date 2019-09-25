package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.layout.FlowPane;
import mediathek.config.Konstanten;
import mediathek.daten.GeoblockingField;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.URISyntaxException;

public class OrfSetupInformationThread implements Runnable {
    private static final Logger logger = LogManager.getLogger(OrfSetupInformationThread.class);

    @Override
    public void run() {
        logger.trace("ORF setup tutorial display check started");
        var config = ApplicationConfiguration.getConfiguration();
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, true)) {
            //we haven´t shown the config help dialog before
            var location = config.getString(ApplicationConfiguration.GEO_LOCATION, "");
            if (location.equals(GeoblockingField.GEO_AT)) {
                // show help dialog
                Platform.runLater(() -> {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle(Konstanten.PROGRAMMNAME);
                    alert.setHeaderText("Wichtige Information für ORF");
                    FlowPane fp = new FlowPane();
                    Label lbl = new Label("Um den ORF erfolgreich zu nutzen sind zusätzlich Einstellungen erforderlich.\n" +
                            "Bitte lesen Sie sich hierzu folgenden Link durch: ");
                    Hyperlink link = new Hyperlink("Link zum Tutorial");
                    fp.getChildren().addAll(lbl, link);

                    link.setOnAction((evt) -> {
                        //disable display of dialog when user has clicked on the hyperlink
                        config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, false);
                        try {
                            UrlHyperlinkAction.openURL(MediathekGui.ui(), "https://forum.mediathekview.de/topic/2546/anleitung-einstellungen-für-orf-download");
                        } catch (URISyntaxException ignored) {
                        }
                        alert.close();
                    });

                    alert.getDialogPane().contentProperty().set(fp);

                    alert.show();
                });
            }
        }
        logger.trace("ORF setup tutorial display check finished");
    }
}
