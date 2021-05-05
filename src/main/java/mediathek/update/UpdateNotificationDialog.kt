package mediathek.update;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import mediathek.config.Konstanten;
import mediathek.gui.actions.DisposeDialogAction;
import mediathek.gui.dialog.ButtonFlowPanel;
import mediathek.gui.dialog.ButtonPanel;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Version;

import javax.swing.*;
import java.awt.*;
import java.util.Objects;

public class UpdateNotificationDialog extends JDialog {
    private final Version version;
    private final UpdateNotificationPanel panel = new UpdateNotificationPanel();
    private WebView browser;
    private WebEngine webEngine;

    public UpdateNotificationDialog(Frame owner, String title, Version version) {
        super(owner, title, true);
        this.version = version;
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        EscapeKeyHandler.installHandler(this, this::dispose);

        setupDialogInformation();
        setupFxWebView();

        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(panel, BorderLayout.CENTER);
        var buttonPanel = new ButtonPanel();
        buttonPanel.add(createButtonPanel(), BorderLayout.EAST);
        contentPane.add(buttonPanel, BorderLayout.SOUTH);
        pack();
        GuiFunktionen.centerOnScreen(this, false);
    }

    private ButtonFlowPanel createButtonPanel() {
        ButtonFlowPanel pnl = new ButtonFlowPanel();
        JButton btn = new JButton(new DisposeDialogAction(this, "Schließen", "Dialog schließen"));
        getRootPane().setDefaultButton(btn);
        pnl.add(btn);
        return pnl;
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
}
