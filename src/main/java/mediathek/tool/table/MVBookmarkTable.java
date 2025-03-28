package mediathek.tool.table;

import mediathek.config.MVConfig.Configs;
import org.jetbrains.annotations.NotNull;
import java.util.Optional;

public class MVBookmarkTable extends MVTable{

    public MVBookmarkTable(int maxColumns, boolean @NotNull [] visibleColumStore, @NotNull Optional<Configs> showIconsConfigKey, @NotNull Optional<Configs> smallSenderIconConfigKey, @NotNull Optional<Configs> columnConfigurationDataConfigKey) {

        super(maxColumns, visibleColumStore, showIconsConfigKey, smallSenderIconConfigKey, columnConfigurationDataConfigKey);
    }

    @Override
    protected void spaltenAusschalten() {

    }

}
