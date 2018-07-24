package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;

import javax.swing.*;

final class ProxyPortListener extends PropertyDocumentListener {
    public ProxyPortListener(JTextField control) {
        savePropertyTransition.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
            final String proxyPort = control.getText();
            try {
                final int port = Integer.parseInt(proxyPort);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.HTTP_PROXY_PORT, port);
            } catch (NumberFormatException ex) {
                ex.printStackTrace();
                control.setText("");
            }
        }));
    }
}
