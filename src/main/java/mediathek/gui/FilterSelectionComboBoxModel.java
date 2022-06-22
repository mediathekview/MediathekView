package mediathek.gui;

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
