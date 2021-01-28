package mediathek.gui.actions;

import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;
import mediathek.gui.messages.ProgramLocationChangedEvent;
import mediathek.tool.MessageBus;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class UrlHyperlinkAction extends AbstractAction {

    private final JFrame jFrameParent;

    public UrlHyperlinkAction(JFrame jjFrameParent, String url) {
        jFrameParent = jjFrameParent;
        super.putValue(Action.NAME, url);
        super.putValue(SHORT_DESCRIPTION, url);
    }

    private static void configureAndStartCustomWebBrowser(JFrame paFrame, String url) {
        try {
            String programm = "";
            if (MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN).isEmpty()) {
                String text = "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen.";
                DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(paFrame, true, "", "Browser suchen", text);
                dialog.setVisible(true);
                if (dialog.ok) {
                    programm = dialog.ziel;
                }
            } else {
                programm = MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN);
            }

            launchApplication(programm, url);

            MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, programm);
            MessageBus.getMessageBus().publishAsync(new ProgramLocationChangedEvent());
        } catch (Exception ex) {
            MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, ""); // dann wars wohl nix
            logger.error("Failed to launch URL {} with custom browser", url);
        }
    }

    private static void launchApplication(String app, String url) throws IOException {
        ProcessBuilder builder = new ProcessBuilder(app, url);
        var env = builder.environment();
        env.remove("GDK_SCALE");
        builder.start();
    }

    private static void launchMacDefaultBrowser(String url) throws IOException {
        final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
        String command = "open location \"" + url + '"';
        builder.command().add(command);
        builder.start();
    }

    private static final Logger logger = LogManager.getLogger(UrlHyperlinkAction.class);

    /**
     * Try to open a browser window.
     *
     * @param paFrame the parent window.
     * @param url     URL to be opened. Here in string format.
     * @throws URISyntaxException when URL is malformed.
     */
    public static void openURL(JFrame paFrame, String url) throws URISyntaxException {
        boolean launchFailed = false;

        if (SystemUtils.IS_OS_MAC_OSX) {
            try {
                logger.trace("trying to launch macOS default web browser");
                launchMacDefaultBrowser(url);
            } catch (IOException e) {
                logger.error("Failed to launch default macOS web browser, using custom...");
                launchFailed = true;
            }
        } else if (SystemUtils.IS_OS_LINUX) {
            try {
                logger.trace("trying to use xdg-open to start web browser");
                launchApplication("xdg-open", url);
            } catch (IOException e) {
                logger.error("Failed to launch web browser with xdg-open");
                launchFailed = true;
            }
        } else {
            if (Desktop.isDesktopSupported()) {
                logger.trace("trying to launch java desktop default web browser");
                try {
                    final Desktop d = Desktop.getDesktop();
                    if (d.isSupported(Desktop.Action.BROWSE)) {
                        d.browse(new URI(url));
                    }
                } catch (IOException e) {
                    logger.error("Failed to launch java desktop supported web browser, using custom...");
                    launchFailed = true;
                }
            } else {
                logger.trace("trying to launch custom web browser");
                //Desktop not supported, we need to work custom
                configureAndStartCustomWebBrowser(paFrame, url);
            }
        }

        if (launchFailed)
            configureAndStartCustomWebBrowser(paFrame,url);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            openURL(jFrameParent, e.getActionCommand());
        } catch (URISyntaxException ignored) {
        }
    }
}
