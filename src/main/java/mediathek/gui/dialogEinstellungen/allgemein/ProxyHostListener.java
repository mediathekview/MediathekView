package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;

import javax.swing.*;

final class ProxyHostListener extends PropertyDocumentListener {
    public ProxyHostListener(JTextField control) {
        savePropertyTransition.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
            final String proxyHost = control.getText();
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.HTTP_PROXY_HOSTNAME, proxyHost);
        }));
    }
}
