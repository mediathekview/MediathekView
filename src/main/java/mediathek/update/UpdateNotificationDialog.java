package mediathek.update;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import mediathek.config.Konstanten;
import mediathek.gui.dialog.StandardCloseDialog;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Version;

import javax.swing.*;
import java.awt.*;
import java.util.Objects;

public class UpdateNotificationDialog extends StandardCloseDialog {
    private final Version version;
    private final UpdateNotificationPanel panel = new UpdateNotificationPanel();
    private WebView browser;
    private WebEngine webEngine;

    public UpdateNotificationDialog(Frame owner, String title, Version version) {
        super(owner, title, true);
        this.version = version;

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
            webEngine.load(Objects.requireNonNull(Konstanten.WEBSITE_BASE_URL.resolve("changelogs")).toString());


            panel.getFxPanel().setScene(scene);
        });
    }

    private void setupDialogInformation() {
        String label = "MediathekView " + version + " ist verfügbar - "
                + "Sie haben Version " + Konstanten.MVVERSION;
        panel.getReleaseInfoLabel().setText(label);

    }

    @Override
    public JComponent createContentPanel() {
        return panel;
    }
}
