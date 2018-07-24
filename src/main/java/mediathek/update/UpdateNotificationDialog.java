package mediathek.update;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import mediathek.config.Konstanten;
import mediathek.gui.dialog.StandardCloseDialog;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.*;

public class UpdateNotificationDialog extends StandardCloseDialog {
    private final ServerProgramInformation programInformation;
    private final UpdateNotificationPanel panel = new UpdateNotificationPanel();
    private WebView browser;
    private WebEngine webEngine;

    public UpdateNotificationDialog(Frame owner, String title, ServerProgramInformation progInfo) {
        super(owner, title, true);
        programInformation = progInfo;

        setupDialogInformation();
        setupFxWebView();

        pack();
        GuiFunktionen.centerOnScreen(this, false);
    }

    private void setupFxWebView() {
        Platform.runLater(() -> {
            browser = new WebView();
            Scene scene = new Scene(browser);
            webEngine = browser.getEngine();
            webEngine.load("http://mediathekview.crystalpalace.info/Update_MediathekView_Dummy.html");


            panel.getFxPanel().setScene(scene);
        });
    }

    private void setupDialogInformation() {
        String label = "MediathekView " + programInformation.getVersion() + " ist verfügbar - "
                + "Sie haben Version " + Konstanten.MVVERSION;
        panel.getReleaseInfoLabel().setText(label);

    }

    @Override
    public JComponent createContentPanel() {
        return panel;
    }
}
