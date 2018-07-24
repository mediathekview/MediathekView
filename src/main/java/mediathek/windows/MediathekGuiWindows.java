package mediathek.windows;

import mediathek.MediathekGui;
import mediathek.tool.threads.IndicatorThread;

@SuppressWarnings("serial")
public class MediathekGuiWindows extends MediathekGui {
    public MediathekGuiWindows(String... args) {
        super(args);
    }

    @Override
    protected IndicatorThread createProgressIndicatorThread() {
        return new WinIndicatorThread(this);
    }
}
