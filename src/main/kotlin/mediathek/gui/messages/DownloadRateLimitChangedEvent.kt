package mediathek.gui.messages

class DownloadRateLimitChangedEvent : BaseEvent() {
    /**
     * New limit in KBytes.
     */
    var newLimit: Int = 0
    var active: Boolean = false
}
