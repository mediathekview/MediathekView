package mediathek.tool;

import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

@SuppressWarnings("serial")
public class EscapeKeyHandler {

    private static final String CANCEL_KEY_HANDLER = "key_cancel";

    private EscapeKeyHandler(JFrame frame, Runnable action) {
        JRootPane rootPane = frame.getRootPane();
        installHandler(rootPane, action);
    }

    private EscapeKeyHandler(JDialog dialog, Runnable action) {
        JRootPane rootPane = dialog.getRootPane();
        installHandler(rootPane, action);
    }

    public static void installHandler(JDialog dialog, Runnable action) {
        new EscapeKeyHandler(dialog, action);
    }

    public static void installHandler(JFrame frame, Runnable action) {
        new EscapeKeyHandler(frame, action);
    }

    private void installHandler(JRootPane rootPane, Runnable action) {
        final var inputMap = rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        // ESC zum Beenden
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), CANCEL_KEY_HANDLER);
        // für den Mac
        if (SystemUtils.IS_OS_MAC_OSX) {
            inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()), CANCEL_KEY_HANDLER);
        }
        rootPane.getActionMap().put(CANCEL_KEY_HANDLER, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                action.run();
            }
        });
    }
}
