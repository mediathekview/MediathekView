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

import javax.swing.*;

/**
 *
 * @author emil
 */
public interface MVFilter {

    public static final int MAX_FILTER = 5;

    public void removeAllListener();

    public JButton get_jButtonFilterLoeschen();

    public JButton get_jButtonClearAll();

    public JCheckBox get_jCheckBoxKeineAbos();

    public JCheckBox get_jCheckBoxKeineGesehenen();

    public JCheckBox get_jCheckBoxNurHd();

    public JCheckBox get_jCheckBoxNurUt();

    public JComboBox<String> get_jComboBoxFilterSender();

    public JComboBox<String> get_jComboBoxFilterThema();

    public JSlider get_jSliderTage();

    public JTextField get_jTextFieldFilterTage();

    public JSlider get_jSliderMinuten();

    public JRadioButton get_rbMin();

    public JRadioButton get_rbMax();

    public JTextField get_jTextFieldFilterMinuten();

    public boolean getThemaTitel();

    public void setThemaTitel(boolean set);

    public JRadioButton get_jRadioButtonTT();

    public JRadioButton get_JRadioButtonIrgendwo();

    public JTextField get_jTextFieldFilterThemaTitel();

    public JTextField get_jTextFieldFilterTitel();

    public JToggleButton get_jToggleButtonLivestram();

    public JCheckBox get_jCheckBoxNeue();

    public JToggleButton get_jToggleButtonHistory();

    public void setVisible(boolean v);

    public void mvFdeleteFilter(int i);

    public void mvFsaveFilter(int i);

    public void mvFfilter(int i);

    public void enableFilter(boolean enable);

}
