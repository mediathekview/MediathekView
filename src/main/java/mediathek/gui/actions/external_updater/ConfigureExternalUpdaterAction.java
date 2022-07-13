package mediathek.gui.actions.external_updater;

import com.install4j.api.update.UpdateSchedule;
import com.install4j.api.update.UpdateScheduleRegistry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ConfigureExternalUpdaterAction extends AbstractAction {
    private static final Logger logger = LogManager.getLogger();
    public ConfigureExternalUpdaterAction() {
        putValue(Action.NAME, "install4j Updater konfigurieren...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        UpdateScheduleRegistry.setUpdateSchedule(UpdateSchedule.NEVER);
        var schedule = UpdateScheduleRegistry.getUpdateSchedule();
        if (schedule != null) {
            logger.trace("I4j schedule: " + schedule);
        }
        else
            logger.warn("Install4j update schedule is null -> cannot configure");
    }
}
