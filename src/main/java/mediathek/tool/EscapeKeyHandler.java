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

import com.jidesoft.utils.SystemInfo;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

@SuppressWarnings("serial")
public class EscapeKeyHandler {

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
        // ESC zum Beenden
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "x");
        rootPane.getActionMap().put("x", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                action.perform();
            }
        });
        // für den Mac
        if (SystemInfo.isMacOSX()) {
            rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mac-cancel");
            rootPane.getActionMap().put("mac-cancel", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    action.perform();
                }
            });
        }
    }

    public interface IAction {
        void perform();
    }
}
