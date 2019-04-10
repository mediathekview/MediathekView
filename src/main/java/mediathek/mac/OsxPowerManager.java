package mediathek.mac;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

/**
 * Prevents system sleep on macOS
 */
public class OsxPowerManager {
    private Process caffeinateProcess = null;
    private static final Logger logger = LogManager.getLogger(OsxPowerManager.class);

    public void disablePowerManagement() {
        //we already have pm disabled..
        if (caffeinateProcess != null)
            return;

        try {
            ProcessBuilder pb = new ProcessBuilder("/usr/bin/caffeinate");
            caffeinateProcess = pb.start();
            logger.trace("power management disabled");
        }
        catch (IOException e) {
            caffeinateProcess = null;
            logger.error("disabling power management failed", e);
            e.printStackTrace();
        }
    }

    public void enablePowerManagement() {
            if (caffeinateProcess != null) {
                caffeinateProcess.destroy();
            }

            caffeinateProcess = null;
            logger.trace("power management enabled");
    }
}
