package mediathek.gui.actions;

import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;
import mediathek.gui.messages.ProgramLocationChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MessageBus;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;

public class UrlHyperlinkAction extends AbstractAction {

    public UrlHyperlinkAction(@NotNull String url) {
        super.putValue(Action.NAME, url);
        super.putValue(SHORT_DESCRIPTION, url);
    }

    private static void configureAndStartCustomWebBrowser(@NotNull String url) {
        try {
            String programm = "";
            if (MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN).isEmpty()) {
                String text = "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen.";
                DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(MediathekGui.ui(), true, "", "Browser suchen", text);
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

    private static void launchApplication(@NotNull String app, @NotNull String url) throws IOException {
        logger.trace("trying to use xdg-open to start web browser");
        ProcessBuilder builder = new ProcessBuilder(app, url);
        var env = builder.environment();
        env.remove("GDK_SCALE");
        builder.start();
    }

    private static void launchMacDefaultBrowser(@NotNull String url) throws IOException {
        logger.trace("trying to launch macOS default web browser");
        final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
        String command = "open location \"" + url + '"';
        builder.command().add(command);
        builder.start();
    }

    private static void launchWithJavaDesktopServices(@NotNull String url) throws Exception {
        logger.trace("trying to launch java desktop default web browser");
        final Desktop d = Desktop.getDesktop();
        if (d.isSupported(Desktop.Action.BROWSE)) {
            d.browse(new URI(url));
        }
        else
            throw new UnsupportedOperationException("Desktop is not supported");
    }

    private static final Logger logger = LogManager.getLogger(UrlHyperlinkAction.class);

    public static void openURI(@NotNull URI uri) {
        openURL(uri.toString());
    }
    /**
     * Try to open a browser window.
     *
     * @param url     URL to be opened. Here in string format.
     */
    public static void openURL(String url) {
        boolean launchFailed = false;

        if (SystemUtils.IS_OS_MAC_OSX) {
            try {
                launchMacDefaultBrowser(url);
            } catch (IOException e) {
                logger.error("Failed to launch default macOS web browser, using custom...", e);
                launchFailed = true;
            }
        } else if (SystemUtils.IS_OS_LINUX) {
            try {
                launchApplication("xdg-open", url);
            } catch (IOException e) {
                logger.error("Failed to launch web browser with xdg-open", e);
                launchFailed = true;
            }
        } else {
            if (Desktop.isDesktopSupported()) {
                try {
                    launchWithJavaDesktopServices(url);
                } catch (Exception e) {
                    logger.error("Failed to launch java desktop supported web browser, using custom...", e);
                    launchFailed = true;
                }
            } else {
                logger.trace("trying to launch custom web browser");
                //Desktop not supported, we need to work custom
                configureAndStartCustomWebBrowser(url);
            }
        }

        if (launchFailed)
            configureAndStartCustomWebBrowser(url);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
            openURL(e.getActionCommand());
    }
}
