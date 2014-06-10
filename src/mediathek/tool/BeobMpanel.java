/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import mediathek.res.GetIcon;

public class BeobMpanel implements ActionListener {

    private int hoehe = -1;
    private final JCheckBox box;
    private final JPanel panel;

    public BeobMpanel(JCheckBox bbox, JPanel ppanel, String text) {
        box = bbox;
        box.setSelected(true);
        panel = ppanel;
        box.setText(text);
        box.setIcon(GetIcon.getIcon("abschnitt_zu_16.png"));
        box.setFont(new java.awt.Font("Dialog", 0, 11));
    }

    private void setPanel() {
        if (hoehe == -1) {
            hoehe = panel.getSize().height;
        }
        if (box.isSelected()) {
            panel.setSize(panel.getSize().width, hoehe);
            panel.setPreferredSize(new Dimension(panel.getSize().width, hoehe));
            box.setIcon(GetIcon.getIcon("abschnitt_zu_16.png"));
        } else {
            panel.setSize(panel.getSize().width, box.getSize().height + 2);
            panel.setPreferredSize(new Dimension(panel.getSize().width, box.getSize().height + 2));
            box.setIcon(GetIcon.getIcon("abschnitt_auf_16.png"));
        }
        panel.updateUI();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        setPanel();
    }
}
