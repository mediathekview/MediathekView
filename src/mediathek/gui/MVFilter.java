/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import mediathek.daten.Daten;

/**
 *
 * @author emil
 */
public interface MVFilter {

    public JButton get_jButtonFilterLoeschen();

    public JButton get_jButtonHilfe();

    public JCheckBox get_jCheckBoxKeineAbos();

    public JCheckBox get_jCheckBoxKeineGesehenen();

    public JCheckBox get_jCheckBoxNurHd();

    public JComboBox get_jComboBoxFilterSender();

    public JComboBox get_jComboBoxFilterThema();

    public JComboBox get_jComboBoxZeitraum();

    public JSlider get_jSliderMinuten();

    public JTextField get_jTextFieldFilterIrgendwo();

    public JTextField get_jTextFieldFilterMinuten();

    public JTextField get_jTextFieldFilterThemaTitel();

    public JTextField get_jTextFieldFilterTitel();

    public JToggleButton get_jToggleButtonLivestram();

    public JToggleButton get_jToggleButtonNeue();

    public void setDaten(Daten d);

    public void setVisible(boolean v);

}
