package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;

import javax.swing.*;

final class UserAgentListener extends PropertyDocumentListener {
    public UserAgentListener(JTextField control) {
        savePropertyTransition.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
            final String userAgent = control.getText();
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_USER_AGENT, userAgent);
        }));
    }
}
