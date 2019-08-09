package mediathek.tool;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

import static mediathek.tool.Functions.getOs;

public class GenericShutdownComputerCommand extends ShutdownComputerCommand {
    private final Logger logger = LogManager.getLogger(ShutdownComputerCommand.class);
    private String strShutdownCommand = "";

    @Override
    public void execute() {

        switch (getOs()) {
            case LINUX:
                //strShutdownCommand = "shutdown -h now";
                strShutdownCommand = MVConfig.get(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN);
                if (strShutdownCommand.isEmpty()) {
                    // sicherheitshalber
                    strShutdownCommand = Konstanten.SHUTDOWN_LINUX;
                    MVConfig.add(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
                }
                break;

            case WIN32:
            case WIN64:
                strShutdownCommand = "shutdown.exe -s -t 0";
                break;

            default:
                logger.error("Shutdown unsupported operating system ...");
                break;
        }

        runCommand();
    }

    private void runCommand() {
//only run if we have a proper shutdown command...
        if (!strShutdownCommand.isEmpty()) {
            try {
                logger.info("Shutdown: {}", strShutdownCommand);
                Runtime.getRuntime().exec(strShutdownCommand);
            } catch (IOException ex) {
                logger.error(ex);
            }
        }
    }
}
