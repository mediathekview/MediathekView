/*
 * Copyright (c) 2025-2026 derreisende77.
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
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.JsonStringUtils;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kordamp.ikonli.materialdesign2.MaterialDesignE;
import org.kordamp.ikonli.materialdesign2.MaterialDesignN;

import javax.swing.*;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BookmarkTableColumnSettingsManager<E> {
    private static final Logger LOG = LogManager.getLogger();
    private static final String COLUMN_SETTINGS = ".colummn-settings";
    private static final Pattern ID_PATTERN = Pattern.compile("\"id\"\\s*:\\s*\"((?:\\\\.|[^\"])*)\"");
    private static final Pattern POSITION_PATTERN = Pattern.compile("\"position\"\\s*:\\s*(-?\\d+)");
    private static final Pattern WIDTH_PATTERN = Pattern.compile("\"width\"\\s*:\\s*(-?\\d+)");
    private static final Pattern VISIBLE_PATTERN = Pattern.compile("\"visible\"\\s*:\\s*(true|false)");
    protected final JTable table;
    protected final List<TableColumn> allColumns = new ArrayList<>();
    protected final List<ColumnSetting> lastSettings = new ArrayList<>();
    protected final TableComparatorChooser<E> comparatorChooser;
    private final String configPrefix;

    public BookmarkTableColumnSettingsManager(JTable table, String configPrefix, TableComparatorChooser<E> comparatorChooser) {
        this.table = table;
        this.configPrefix = configPrefix;
        this.comparatorChooser = comparatorChooser;

        var columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            var col = columnModel.getColumn(i);
            allColumns.add(col);
            var cs = new ColumnSetting(
                    col.getIdentifier().toString(),
                    i,
                    col.getWidth(),
                    true
            );
            lastSettings.add(cs);
        }
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

    public void load() {
        try {
            List<ColumnSetting> fileSettings;
            var config = ApplicationConfiguration.getConfiguration();
            config.lock(LockMode.READ);
            try {
                var str = config.getString(configPrefix + COLUMN_SETTINGS);
                fileSettings = parseColumnSettingsJson(str);
            }
            finally {
                config.unlock(LockMode.READ);
            }

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
                    lastSettings.add(new ColumnSetting(fs.id, fs.position, fs.width, fs.visible));
                }
            }

            var validIds = allColumns.stream()
                    .map(c -> c.getIdentifier().toString())
                    .toList();
            lastSettings.removeIf(ls -> !validIds.contains(ls.id));
        }
        catch (Exception ex) {
            LOG.error("Failed to load column settings.", ex);
        }

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
            var output = toColumnSettingsJson(lastSettings);
            config.setProperty(configPrefix + COLUMN_SETTINGS, output);
        }
        catch (Exception ex) {
            LOG.error("Failed to save column settings.", ex);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    public void installContextMenu() {
        load();
        JPopupMenu popup = new JPopupMenu();

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

    protected boolean isInModel(TableColumn col) {
        var columnModel = table.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            if (columnModel.getColumn(i) == col)
                return true;
        }
        return false;
    }

    private List<ColumnSetting> parseColumnSettingsJson(String json) {
        List<ColumnSetting> result = new ArrayList<>();
        if (json == null)
            return result;

        String trimmed = json.trim();
        if (!trimmed.startsWith("[") || !trimmed.endsWith("]"))
            return result;

        int i = 1;
        while (i < trimmed.length() - 1) {
            char c = trimmed.charAt(i);
            if (Character.isWhitespace(c) || c == ',') {
                i++;
                continue;
            }
            if (c != '{') {
                i++;
                continue;
            }

            int objectStart = i;
            int depth = 0;
            boolean inString = false;
            boolean escaping = false;
            int objectEnd = -1;
            for (; i < trimmed.length() - 1; i++) {
                char ch = trimmed.charAt(i);
                if (escaping) {
                    escaping = false;
                    continue;
                }
                if (ch == '\\' && inString) {
                    escaping = true;
                    continue;
                }
                if (ch == '"') {
                    inString = !inString;
                    continue;
                }
                if (inString)
                    continue;

                if (ch == '{')
                    depth++;
                else if (ch == '}') {
                    depth--;
                    if (depth == 0) {
                        objectEnd = i;
                        break;
                    }
                }
            }

            if (objectEnd < 0)
                break;

            String objectJson = trimmed.substring(objectStart, objectEnd + 1);
            ColumnSetting setting = parseColumnSettingObject(objectJson);
            if (setting != null)
                result.add(setting);
            i = objectEnd + 1;
        }
        return result;
    }

    private ColumnSetting parseColumnSettingObject(String objectJson) {
        String id = extractString(ID_PATTERN, objectJson);
        Integer position = extractInt(POSITION_PATTERN, objectJson);
        Integer width = extractInt(WIDTH_PATTERN, objectJson);
        Boolean visible = extractBoolean(VISIBLE_PATTERN, objectJson);

        if (id == null || position == null || width == null || visible == null)
            return null;
        return new ColumnSetting(id, position, width, visible);
    }

    private String toColumnSettingsJson(List<ColumnSetting> settings) {
        StringBuilder result = new StringBuilder();
        result.append('[');
        for (int i = 0; i < settings.size(); i++) {
            ColumnSetting s = settings.get(i);
            if (i > 0)
                result.append(',');
            result.append('{');
            result.append("\"id\":\"").append(JsonStringUtils.escapeJsonString(s.id)).append('"');
            result.append(",\"position\":").append(s.position);
            result.append(",\"width\":").append(s.width);
            result.append(",\"visible\":").append(s.visible);
            result.append('}');
        }
        result.append(']');
        return result.toString();
    }

    private String extractString(Pattern pattern, String json) {
        Matcher matcher = pattern.matcher(json);
        if (!matcher.find())
            return null;
        return JsonStringUtils.unescapeJsonString(matcher.group(1));
    }

    private Integer extractInt(Pattern pattern, String json) {
        Matcher matcher = pattern.matcher(json);
        if (!matcher.find())
            return null;
        try {
            return Integer.parseInt(matcher.group(1));
        }
        catch (NumberFormatException ex) {
            return null;
        }
    }

    private Boolean extractBoolean(Pattern pattern, String json) {
        Matcher matcher = pattern.matcher(json);
        if (!matcher.find())
            return null;
        return Boolean.parseBoolean(matcher.group(1));
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
