package mediathek.tool;

import org.apache.commons.configuration2.XMLConfiguration;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class FilterConfigTest {

  @Test
  public void getCurrentFilterId_noFiltersExist_IdOfNewlyCreatedFilter() {
    assertThat(new FilterConfiguration(new XMLConfiguration()).getCurrentFilterID()).isNotNull();
  }

  @Test
  public void clearCurrentFilter_clear_CurrentFilterConfigCleared() {}

  @Test
  public void saveFilter_unloadAndLoadAgain_AllFilterSettingsLoaded() {}

  @Test
  public void initializeFilterConfig_migrateConfig_OldFilterConfigMigrated() {}
}
