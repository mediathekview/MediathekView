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

package mediathek.gui.tabs.tab_film.filter_selection;

import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

public class FilterSelectionComboBoxModel extends DefaultComboBoxModel<FilterDTO> {
    private final FilterConfiguration filterConfiguration = new FilterConfiguration();
    private final List<FilterDTO> availableFilters = new ArrayList<>();

    @Override
    public void setSelectedItem(Object anObject) {
        super.setSelectedItem(anObject);
        if (anObject != null) {
            filterConfiguration.setCurrentFilter((FilterDTO) anObject);
        }
    }

    @Override
    public Object getSelectedItem() {
        return filterConfiguration.getCurrentFilter();
    }

    public FilterSelectionComboBoxModel() {
        availableFilters.addAll(filterConfiguration.getAvailableFilters());
        FilterConfiguration.addAvailableFiltersObserver(() -> {
            //System.out.println("FILTER LIST CHANGED");
            SwingUtilities.invokeLater(() -> {
                availableFilters.clear();
                availableFilters.addAll(filterConfiguration.getAvailableFilters());
                this.fireContentsChanged(this, 0, availableFilters.size());
            });
        });
        FilterConfiguration.addCurrentFiltersObserver(filterDTO -> SwingUtilities.invokeLater(() -> {
            if (getSelectedItem() != filterDTO) {
                //System.out.println("CURRENT FILTER CHANGED");
                fireContentsChanged(this, 0, availableFilters.size());
            }
        }));
    }

    @Override
    public int getSize() {
        return availableFilters.size();
    }

    @Override
    public FilterDTO getElementAt(int index) {
        return availableFilters.get(index);
    }
}
