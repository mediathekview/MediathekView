package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;

import javax.swing.*;

final class ProxyPasswordListener extends PropertyDocumentListener {
    public ProxyPasswordListener(JPasswordField control) {
        savePropertyTransition.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
            final String password = String.valueOf(control.getPassword());
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.HTTP_PROXY_PASSWORD, password);
        }));
    }
}
