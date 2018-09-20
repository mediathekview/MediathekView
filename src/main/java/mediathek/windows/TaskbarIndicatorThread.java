package mediathek.windows;

import mediathek.tool.threads.IndicatorThread;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.TimeUnit;

public class TaskbarIndicatorThread extends IndicatorThread {
    private final Taskbar taskbar;
    private final JFrame parent;

    public TaskbarIndicatorThread(MediathekGuiWindows parent) {
        super();
        setName("TaskbarIndicatorThread");
        taskbar = Taskbar.getTaskbar();
        this.parent = parent;

    }

    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                final int percentage = (int) calculateOverallPercentage();
                taskbar.setWindowProgressValue(parent,percentage);
                taskbar.setWindowProgressState(parent,Taskbar.State.NORMAL);

                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (InterruptedException ignored) {
        } finally {
            //when we are finished, stop progress
            taskbar.setWindowProgressState(parent, Taskbar.State.OFF);
        }
    }
}
