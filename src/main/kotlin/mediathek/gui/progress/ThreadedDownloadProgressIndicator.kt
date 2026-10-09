/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.progress

import mediathek.tool.threads.IndicatorThread
import java.util.concurrent.atomic.AtomicInteger

abstract class ThreadedDownloadProgressIndicator(
    private val indicatorThreadFactory: () -> IndicatorThread,
) : DownloadProgressIndicator {
    private val activeDownloads = AtomicInteger()
    private var indicatorThread: IndicatorThread? = null

    @Synchronized
    override fun downloadStarted() {
        val activeDownloadCount = activeDownloads.incrementAndGet()
        if (indicatorThread == null) {
            startIndicatorThread()
        }
        onDownloadStarted(activeDownloadCount)
    }

    @Synchronized
    override fun downloadFinished() {
        var activeDownloadCount = activeDownloads.decrementAndGet()
        if (activeDownloadCount <= 0) {
            activeDownloads.set(0)
            activeDownloadCount = 0
            stopIndicatorThread()
        }
        onDownloadFinished(activeDownloadCount)
    }

    protected open fun onDownloadStarted(activeDownloadCount: Int) {
    }

    protected open fun onDownloadFinished(activeDownloadCount: Int) {
    }

    private fun startIndicatorThread() {
        try {
            indicatorThread = indicatorThreadFactory().also { it.start() }
        } catch (_: Exception) {
        }
    }

    private fun stopIndicatorThread() {
        indicatorThread?.interrupt()
        indicatorThread = null
    }

    @Synchronized
    override fun close() {
        activeDownloads.set(0)
        stopIndicatorThread()
    }
}
