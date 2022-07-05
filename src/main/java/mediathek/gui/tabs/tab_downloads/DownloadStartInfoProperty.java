package mediathek.gui.tabs.tab_downloads;

import mediathek.config.Daten;
import mediathek.daten.DownloadStartInfo;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

public class DownloadStartInfoProperty {
    private final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    private DownloadStartInfo info;

    public DownloadStartInfoProperty() {
        setInfo(Daten.getInstance().getListeDownloads().getStarts());

        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        setInfo(Daten.getInstance().getListeDownloads().getStarts());
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        setInfo(Daten.getInstance().getListeDownloads().getStarts());
    }

    public DownloadStartInfo getInfo() {
        return info;
    }

    public void setInfo(DownloadStartInfo info) {
        var oldValue = this.info;
        this.info = info;
        this.pcs.firePropertyChange("info", oldValue, this.info);
    }

    public void addStartInfoChangeListener(PropertyChangeListener listener) {
        this.pcs.addPropertyChangeListener(listener);
    }
}
