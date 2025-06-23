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

package mediathek.swing.table;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.table.TableColumn;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Helper to persist and restore a JTable's column order, widths, and visibility
 * using Jackson POJOs, plus a header context menu for toggling column visibility.
 * Hidden columns remember their last position.
 * <p>
 * Usage:
 * TableColumnSettingsManager mgr = new TableColumnSettingsManager(table, settingsFile);
 * mgr.load();
 * mgr.installContextMenu();
 * dialog.addWindowListener(e -> mgr.save());
 */
public abstract class ATableColumnSettingsManager {
    private static final Logger LOG = LogManager.getLogger();
    private static final String COLUMN_SETTINGS = ".colummn-settings";
    protected final JTable table;
    protected final List<TableColumn> allColumns = new ArrayList<>();
    protected final List<ColumnSetting> lastSettings = new ArrayList<>();
    private final ObjectMapper mapper = new ObjectMapper();
    private final String configPrefix;

    public ATableColumnSettingsManager(JTable table, String configPrefix) {
        this.table = table;
        this.configPrefix = configPrefix;

        // Initialize from table's current model
        var columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            var col = columnModel.getColumn(i);
            allColumns.add(col);
            ColumnSetting cs = new ColumnSetting(
                    col.getIdentifier().toString(),
                    i,
                    col.getWidth(),
                    true
            );
            lastSettings.add(cs);
        }
    }

    /**
     * Load and apply saved column settings (visibility, order, width).
     * Merges file settings into runtime defaults to preserve any in-memory changes.
     */
    public void load() {
        try {
            // Read file into temporary list
            List<ColumnSetting> fileSettings;
            var config = ApplicationConfiguration.getConfiguration();
            config.lock(LockMode.READ);
            try {
                var str = config.getString(configPrefix + COLUMN_SETTINGS);
                fileSettings = mapper.readValue(str, new TypeReference<>() {
                });
            }
            finally {
                config.unlock(LockMode.READ);
            }
            // Merge fileSettings into lastSettings
            for (var fs : fileSettings) {
                Optional<ColumnSetting> existing = lastSettings.stream()
                        .filter(ls -> ls.id.equals(fs.id))
                        .findFirst();
                if (existing.isPresent()) {
                    var ls = existing.get();
                    ls.position = fs.position;
                    ls.width = fs.width;
                    ls.visible = fs.visible;
                }
                else {
                    // New column entry
                    lastSettings.add(new ColumnSetting(fs.id, fs.position, fs.width, fs.visible));
                }
            }
            // Remove any lastSettings entries not present in allColumns
            var validIds = allColumns.stream()
                    .map(c -> c.getIdentifier().toString())
                    .toList();
            lastSettings.removeIf(ls -> !validIds.contains(ls.id));
        }
        catch (Exception ex) {
            LOG.error("Failed to load column settings.", ex);
        }

        // Apply settings to table
        var columnModel = table.getColumnModel();
        while (columnModel.getColumnCount() > 0) {
            columnModel.removeColumn(columnModel.getColumn(0));
        }
        lastSettings.stream()
                .filter(s -> s.visible)
                .sorted(Comparator.comparingInt(a -> a.position))
                .forEach(s -> allColumns.stream()
                        .filter(col -> col.getIdentifier().toString().equals(s.id))
                        .findFirst()
                        .ifPresent(col -> {
                            columnModel.addColumn(col);
                            col.setPreferredWidth(s.width);
                        }));
    }

    /**
     * Save current column settings (visibility, order, width) to disk.
     * Hidden columns preserve their last known position.
     */
    public void save() {
        var columnModel = table.getColumnModel();
        for (var ls : lastSettings) {
            Optional<TableColumn> colOpt = allColumns.stream()
                    .filter(c -> c.getIdentifier().toString().equals(ls.id))
                    .findFirst();
            if (colOpt.isEmpty())
                continue;
            var col = colOpt.get();
            boolean visible = isInModel(col);
            ls.visible = visible;
            if (visible) {
                ls.position = columnModel.getColumnIndex(ls.id);
                ls.width = col.getWidth();
            }
        }

        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.WRITE);
        try {
            var output = mapper.writeValueAsString(lastSettings);
            config.setProperty(configPrefix + COLUMN_SETTINGS, output);
        }
        catch (JsonProcessingException ex) {
            LOG.error("Failed to save column settings.", ex);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    /**
     * Install a header context menu to toggle column visibility.
     */
    public abstract void installContextMenu();

    /**
     * Helper to check if a column is currently visible.
     * @param col The column to be tested.
     * @return true if column is in model, false otherwise.
     */
    protected boolean isInModel(TableColumn col) {
        var columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            if (columnModel.getColumn(i) == col)
                return true;
        }
        return false;
    }

    public static class ColumnSetting {
        public String id;
        public int position;
        public int width;
        public boolean visible;

        @SuppressWarnings("unused")
        public ColumnSetting() {
        }

        public ColumnSetting(String id, int position, int width, boolean visible) {
            this.id = id;
            this.position = position;
            this.width = width;
            this.visible = visible;
        }
    }
}
