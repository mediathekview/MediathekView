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

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

public class EscBeenden {

    public EscBeenden(JFrame frame) {
        // ESC zum Beenden
        frame.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "x");
        frame.getRootPane().getActionMap().put("x", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden_();
            }
        });
        // für den Mac
        frame.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mac-cancel");
        frame.getRootPane().getActionMap().put("mac-cancel", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden_();
            }
        });
    }
   public EscBeenden(JDialog dialog) {
        // ESC zum Beenden
        dialog.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "x");
        dialog.getRootPane().getActionMap().put("x", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden_();
            }
        });
        // für den Mac
        dialog.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mac-cancel");
        dialog.getRootPane().getActionMap().put("mac-cancel", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden_();
            }
        });
    }

    public void beenden_() {
    }
}
