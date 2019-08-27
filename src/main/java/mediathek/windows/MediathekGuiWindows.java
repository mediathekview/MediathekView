package mediathek.windows;

import mediathek.mainwindow.MediathekGui;
import mediathek.tool.threads.IndicatorThread;

@SuppressWarnings("serial")
public class MediathekGuiWindows extends MediathekGui {
    public MediathekGuiWindows() {
        super();
    }

    @Override
    protected IndicatorThread createProgressIndicatorThread() {
        return new TaskbarIndicatorThread(this);
    }
}
