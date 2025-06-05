/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.swing;

import com.jidesoft.swing.ComboBoxSearchable;

import javax.swing.*;

public class NoFireOnKeyComboBoxSearchable extends ComboBoxSearchable {
    protected boolean _preventActionEvent;
    protected boolean _noActionOnKeyNavigation;

    public NoFireOnKeyComboBoxSearchable(JComboBox<?> comboBox) {
        super(comboBox);
    }

    public boolean isPreventActionEvent() {
        return _preventActionEvent;
    }

    public void setNoActionOnKeyNavigation(boolean val) {
        _noActionOnKeyNavigation = val;
    }

    @Override
    protected void setSelectedIndex(int index, boolean incremental) {
        Object propTableCellEditor = _component.getClientProperty("JComboBox.isTableCellEditor");
        Object propNoActionOnKeyNavigation = UIManager.get("ComboBox.noActionOnKeyNavigation");
        if ((propTableCellEditor instanceof Boolean && (Boolean) propTableCellEditor) ||
                (propNoActionOnKeyNavigation instanceof Boolean && (Boolean) propNoActionOnKeyNavigation) ||
                _noActionOnKeyNavigation) {
            _preventActionEvent = true;
        }
        try {
            super.setSelectedIndex(index, incremental);
        } finally {
            _preventActionEvent = false;
        }
    }
}
