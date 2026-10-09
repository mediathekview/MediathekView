package mediathek.gui.messages

import mediathek.daten.DatenDownload

class DownloadFinishedEvent(
    val download: DatenDownload,
) : BaseEvent()
