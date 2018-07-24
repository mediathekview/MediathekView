package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;

import javax.swing.*;

final class ProxyUsernameListener extends PropertyDocumentListener {
    public ProxyUsernameListener(JTextField control) {
        savePropertyTransition.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
            final String proxyUser = control.getText();
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.HTTP_PROXY_USERNAME, proxyUser);
        }));
    }
}
