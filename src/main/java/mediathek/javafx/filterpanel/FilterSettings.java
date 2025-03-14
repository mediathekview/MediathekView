package mediathek.javafx.filterpanel;

import java.util.*;

public class FilterSettings {
    private Map<UUID, List<String>> filterCheckedItemsMap = new HashMap<>();

    public void saveCheckedItemsForFilters(List<UUID> filterIds, List<String> checkedItems) {
        for (UUID filterId : filterIds) {
            filterCheckedItemsMap.put(filterId, new ArrayList<>(checkedItems));
        }
    }

    public List<String> getCheckedItemsForFilter(UUID filterId) {
        return filterCheckedItemsMap.getOrDefault(filterId, Collections.emptyList());
    }
}
