package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.swing.IconUtils;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.kordamp.ikonli.materialdesign2.MaterialDesignL;
import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ToggleBlacklistAction extends AbstractAction {
    private final FontIcon enabledIcon;
    private final FontIcon disabledIcon;
    private boolean blacklist_is_on;

    public ToggleBlacklistAction() {
        enabledIcon = IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_STATUS);
        disabledIcon = IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_STATUS, Color.RED);

        blacklist_is_on = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.BLACKLIST_IS_ON, false);
        setupState();

        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        //config was changed outside
        SwingUtilities.invokeLater(() -> {
            blacklist_is_on = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.BLACKLIST_IS_ON, false);
            setupState();
        });
    }

    private void setupState() {
        if (blacklist_is_on) {
            putValue(NAME, "Blacklist ausschalten");
            putValue(SHORT_DESCRIPTION, "Blacklist ausschalten");
            putValue(SMALL_ICON, enabledIcon);
        } else {
            putValue(NAME, "Blacklist einschalten");
            putValue(SHORT_DESCRIPTION, "Blacklist einschalten");
            putValue(SMALL_ICON, disabledIcon);
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        blacklist_is_on = !blacklist_is_on;

        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.BLACKLIST_IS_ON, blacklist_is_on);
        Daten.getInstance().getListeBlacklist().filterListe();
        MessageBus.getMessageBus().publishAsync(new BlacklistChangedEvent());
    }
}
