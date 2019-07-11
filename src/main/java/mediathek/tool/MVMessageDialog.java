package mediathek.tool;

import javax.swing.*;
import java.awt.*;

public class MVMessageDialog {

    public static void showMessageDialog(final Component parent, final String message, final String title, final int messageType) {
        if (SwingUtilities.isEventDispatchThread()) {
            JOptionPane.showMessageDialog(parent, message, title, messageType);
        } else {
            SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(parent, message, title, messageType));
        }
    }
}
