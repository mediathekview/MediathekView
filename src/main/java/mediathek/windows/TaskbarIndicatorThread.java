package mediathek.windows;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinBase;
import mediathek.tool.threads.IndicatorThread;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.TimeUnit;

public class TaskbarIndicatorThread extends IndicatorThread {
    private final Taskbar taskbar;
    private final JFrame parent;
    private static final Logger logger = LogManager.getLogger(TaskbarIndicatorThread.class);


    public TaskbarIndicatorThread(MediathekGuiWindows parent) {
        super();
        setName("TaskbarIndicatorThread");
        taskbar = Taskbar.getTaskbar();
        this.parent = parent;

    }

    private void disableStandby() {
        int success = Kernel32.INSTANCE.SetThreadExecutionState(WinBase.ES_CONTINUOUS | WinBase.ES_SYSTEM_REQUIRED);
        if (success == 0)
            logger.error("disableStandby() failed!");
    }

    private void enableStandby() {
        int success = Kernel32.INSTANCE.SetThreadExecutionState(WinBase.ES_CONTINUOUS);
        if (success == 0)
            logger.error("enableStandby() failed!");
    }

    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                final int percentage = (int) calculateOverallPercentage();
                taskbar.setWindowProgressValue(parent,percentage);
                taskbar.setWindowProgressState(parent,Taskbar.State.NORMAL);

                disableStandby();

                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (InterruptedException ignored) {
        } finally {
            //when we are finished, stop progress
            taskbar.setWindowProgressState(parent, Taskbar.State.OFF);
            enableStandby();
        }
    }
}
