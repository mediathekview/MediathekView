package mediathek.gui.actions;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ToggleBlacklistAction extends AbstractAction {
    private final FlatSVGIcon enabledIcon;
    private final FlatSVGIcon disabledIcon;
    private boolean blacklist_is_on;

    public ToggleBlacklistAction() {
        enabledIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/list-check.svg");

        disabledIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/list-check.svg");
        disabledIcon.setColorFilter(new FlatSVGIcon.ColorFilter(color -> Color.RED));

        blacklist_is_on = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
        setupState();

        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        //config was changed outside
        SwingUtilities.invokeLater(() -> {
            blacklist_is_on = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
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

        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(blacklist_is_on));
        Daten.getInstance().getListeBlacklist().filterListe();
        MessageBus.getMessageBus().publishAsync(new BlacklistChangedEvent());
    }
}
