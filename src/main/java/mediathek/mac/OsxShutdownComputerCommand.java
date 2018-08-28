package mediathek.mac;

import mediathek.tool.ShutdownComputerCommand;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class OsxShutdownComputerCommand extends ShutdownComputerCommand {
    private final Logger logger = LogManager.getLogger(OsxShutdownComputerCommand.class);

    @Override
    public void execute() {
        //we cannot shutdown the system while we are running...
        //MV (or java) will prevent OS X shutdown process and there seems to be no way around it.
        //NASTY WORKAROUND:
        //use applescript to execute a scriptlet application which will wait 5 seconds until it
        //asks the system to shut down
        //meanwhile we MUST terminate MV WITHIN 5 seconds in order not to interrupt the
        //shutdown process :(
        //AND this whole shit works ONLY with osascript, not with the java script engine...
        //Scriptlet(executable) content:
        //delay 5
        //tell application "system events" to shut down
        //EOF
        //The OSX_Shutdown scriptlet application is provided in the official MV app bundle.
        try {
            final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
            builder.command().add("tell application \"OSX_Shutdown\" to activate");
            builder.start();
        } catch (Exception ex) {
            logger.error(ex);
        }
    }
}
