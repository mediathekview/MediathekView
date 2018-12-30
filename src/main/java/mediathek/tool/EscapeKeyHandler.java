/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

@SuppressWarnings("serial")
public class EscapeKeyHandler {

    private static final String CANCEL_KEY_HANDLER = "key_cancel";

    private EscapeKeyHandler(JFrame frame, IAction action) {
        JRootPane rootPane = frame.getRootPane();
        installHandler(rootPane, action);
    }

    private EscapeKeyHandler(JDialog dialog, IAction action) {
        JRootPane rootPane = dialog.getRootPane();
        installHandler(rootPane, action);
    }

    public static void installHandler(JDialog dialog, IAction action) {
        new EscapeKeyHandler(dialog, action);
    }

    public static void installHandler(JFrame frame, IAction action) {
        new EscapeKeyHandler(frame, action);
    }

    private void installHandler(JRootPane rootPane, IAction action) {
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
                action.perform();
            }
        });
    }

    public interface IAction {
        void perform();
    }
}
