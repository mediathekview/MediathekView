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

import ca.odell.glazedlists.swing.TableComparatorChooser;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import mediathek.tool.ApplicationConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Persist and restore the sort-keys from a GlazedLists TableComparatorChooser.
 */
public class GlazedSortKeysPersister<E> {
    private static final String CONFIG_KEY = ".sortKeys";
    private static final Logger LOG = LogManager.getLogger();
    private final ObjectMapper mapper = new ObjectMapper();
    private final String configPrefix;
    private final TableComparatorChooser<E> chooser;

    public GlazedSortKeysPersister(String configPrefix, TableComparatorChooser<E> chooser) {
        this.configPrefix = configPrefix;
        this.chooser = chooser;
    }

    public void saveSortState() {
        var sortedCols = chooser.getSortingColumns();            // :contentReference[oaicite:0]{index=0}
        List<SortKeyInfo> infos = new ArrayList<>(sortedCols.size());
        for (int col : sortedCols) {
            boolean desc = chooser.isColumnReverse(col);           // :contentReference[oaicite:1]{index=1}
            infos.add(new SortKeyInfo(col, 0, desc));
        }
        try {
            String json = mapper.writeValueAsString(infos);
            ApplicationConfiguration.getConfiguration().setProperty(configPrefix + CONFIG_KEY, json);
        }
        catch (IOException e) {
            LOG.error("Failed to save sort keys", e);
        }
    }

    public void restoreSortState() {
        try {
            String json = ApplicationConfiguration.getConfiguration().getString(configPrefix + CONFIG_KEY);
            if (json.isEmpty())
                return;

            List<SortKeyInfo> infos = mapper.readValue(json, new TypeReference<>() {
            });
            for (SortKeyInfo info : infos) {
                chooser.appendComparator(info.column, info.comparatorIndex, info.descending);
            }
        }
        catch (NoSuchElementException ignored) {
        }
        catch (Exception e) {
            LOG.error("Failed to restore sort keys", e);
        }
    }

    /**
     * DTO for one sorted-column entry.
     */
    public static class SortKeyInfo {
        public int column;
        public int comparatorIndex;
        public boolean descending;

        // Jackson needs a no-arg ctor:
        public SortKeyInfo() {
        }

        public SortKeyInfo(int column, int comparatorIndex, boolean descending) {
            this.column = column;
            this.comparatorIndex = comparatorIndex;
            this.descending = descending;
        }
    }
}
