package mediathek.gui.messages;

public class DownloadRateLimitChangedEvent extends BaseEvent {
    /**
     * new limit in KBytes
     */
    public int newLimit;
}
