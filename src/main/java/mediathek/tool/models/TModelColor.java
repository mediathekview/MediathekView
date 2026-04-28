/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.tool.models;

import mediathek.config.MVColor;
import mediathek.tool.MVC;

import javax.swing.table.AbstractTableModel;

public class TModelColor extends AbstractTableModel {
    private static final String[] COLUMN_NAMES = {"Beschreibung", "Farbe"};
    private final boolean darkMode;

    public TModelColor(boolean darkMode) {
        this.darkMode = darkMode;
    }

    @Override
    public int getRowCount() {
        return MVColor.getColors().size();
    }

    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        MVC mvc = getEntry(rowIndex);
        if (columnIndex == MVColor.MVC_TEXT) {
            return mvc.getText();
        }
        if (columnIndex == MVColor.MVC_COLOR) {
            return mvc;
        }
        return null;
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        if (columnIndex == MVColor.MVC_COLOR) {
            result = MVC.class;
        } else {
            result = String.class;
        }
        return result;
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    public MVC getEntry(int rowIndex) {
        return MVColor.get(rowIndex);
    }

    public boolean isDarkMode() {
        return darkMode;
    }
}
