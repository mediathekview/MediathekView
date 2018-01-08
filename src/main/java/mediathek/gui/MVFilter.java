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

    int MAX_FILTER = 5;

    void removeAllListener();

    JButton get_jButtonFilterLoeschen();

    JButton get_jButtonClearAll();

    JComboBox<String> get_jComboBoxFilterSender();

    JComboBox<String> get_jComboBoxFilterThema();

    JSlider get_jSliderTage();

    JTextField get_jTextFieldFilterTage();

    JSlider get_jSliderMinuten();

    JRadioButton get_rbMin();

    JRadioButton get_rbMax();

    JTextField get_jTextFieldFilterMinuten();

    boolean getThemaTitel();

    public void setThemaTitel(boolean set);

    public JRadioButton get_jRadioButtonTT();

    public JRadioButton get_JRadioButtonIrgendwo();

    public JTextField get_jTextFieldFilterThemaTitel();

    public JTextField get_jTextFieldFilterTitel();

    public JToggleButton get_jToggleButtonLivestram();

    public JToggleButton get_jToggleButtonHistory();

    public void setVisible(boolean v);

    public void mvFdeleteFilter(int i);

    public void mvFsaveFilter(int i);

    public void mvFfilter(int i);
}
