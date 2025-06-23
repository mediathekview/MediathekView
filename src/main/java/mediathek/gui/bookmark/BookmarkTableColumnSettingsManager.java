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

package mediathek.gui.bookmark;

import ca.odell.glazedlists.swing.TableComparatorChooser;
import mediathek.swing.IconUtils;
import mediathek.swing.IconizedCheckBoxMenuItem;
import mediathek.swing.table.ATableColumnSettingsManager;
import org.kordamp.ikonli.materialdesign2.MaterialDesignE;
import org.kordamp.ikonli.materialdesign2.MaterialDesignN;

import javax.swing.*;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.util.Optional;

public class BookmarkTableColumnSettingsManager<E> extends ATableColumnSettingsManager {
    protected final TableComparatorChooser<E> comparatorChooser;

    public BookmarkTableColumnSettingsManager(JTable table, String configPrefix, TableComparatorChooser<E> comparatorChooser) {
        super(table, configPrefix);
        this.comparatorChooser = comparatorChooser;
    }

    private JCheckBoxMenuItem createMenuItem(String columnName, boolean visible) {
        JCheckBoxMenuItem item;
        if (columnName.equalsIgnoreCase("Gesehen")) {
            item = new IconizedCheckBoxMenuItem(IconUtils.of(MaterialDesignE.EYE), visible);
        }
        else if (columnName.equalsIgnoreCase("Notiz")) {
            item = new IconizedCheckBoxMenuItem(IconUtils.of(MaterialDesignN.NOTE), visible);
        }
        else {
            item = new JCheckBoxMenuItem(columnName, visible);
        }

        return item;
    }

    @Override
    public void installContextMenu() {
        // Apply current settings
        load();
        JPopupMenu popup = new JPopupMenu();

        // Toggle visibility per column
        for (TableColumn col : allColumns) {
            String columnName = col.getIdentifier().toString();
            Optional<ColumnSetting> csOpt = lastSettings.stream()
                    .filter(s -> s.id.equals(columnName))
                    .findFirst();
            boolean visible = csOpt.map(s -> s.visible).orElse(true);
            var item = createMenuItem(columnName, visible);
            item.addActionListener(_ -> {
                TableColumnModel m = table.getColumnModel();
                csOpt.ifPresent(s -> s.visible = item.isSelected());
                if (item.isSelected()) {
                    if (!isInModel(col)) {
                        m.addColumn(col);
                        int lastIndex = m.getColumnCount() - 1;
                        int target = csOpt.map(s -> s.position).orElse(lastIndex);
                        m.moveColumn(lastIndex, Math.max(0, Math.min(target, lastIndex)));
                    }
                }
                else {
                    if (isInModel(col)) {
                        int idx = m.getColumnIndex(columnName);
                        csOpt.ifPresent(s -> s.position = idx);
                        m.removeColumn(col);
                    }
                }
                save();
            });
            popup.add(item);
        }
        popup.addSeparator();
        var item = new JMenuItem("Sortierschlüssel zurücksetzen");
        item.addActionListener(_ -> comparatorChooser.clearComparator());
        popup.add(item);

        table.getTableHeader().setComponentPopupMenu(popup);
    }
}
