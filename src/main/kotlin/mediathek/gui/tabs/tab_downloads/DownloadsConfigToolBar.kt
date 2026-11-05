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

package mediathek.gui.tabs.tab_downloads

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.DownloadRateLimitChangedEvent
import mediathek.gui.messages.ParallelDownloadNumberChangedEvent
import mediathek.i18n.UiMessages
import mediathek.tool.MessageBus.messageBus
import net.engio.mbassy.listener.Handler
import java.awt.Dimension
import javax.swing.*

class DownloadsConfigToolBar : JToolBar() {
    private fun scheduleDownloadRateLimitChangedEvent() {
        if (downloadRateLimitChangeTimer.isRunning) {
            downloadRateLimitChangeTimer.restart()
        } else {
            downloadRateLimitChangeTimer.start()
        }
    }

    private fun setupDownloadRateLimitCheckBox() {
        val config = ApplicationConfiguration.getInstance()
        val active = config.downloadRateLimitActive
        cbMaxBandwidth.isSelected = active
        cbMaxBandwidth.addActionListener {
            config.downloadRateLimitActive = cbMaxBandwidth.isSelected
            downloadRateLimitChangeTimer.stop()
            fireDownloadRateLimitChangedEvent()
        }
    }

    private fun fireDownloadRateLimitChangedEvent() {
        val downloadLimit = (spinnerMaxBandwidth.value as Number).toInt()
        ApplicationConfiguration.getInstance().downloadRateLimit = downloadLimit.toLong()
        val evt = DownloadRateLimitChangedEvent()
        evt.newLimit = downloadLimit
        evt.active = cbMaxBandwidth.isSelected
        messageBus.publishAsync(evt)
    }

    private fun setupDownloadRateLimitSpinner() {
        spinnerMaxBandwidth.putClientProperty("JComponent.roundRect", true)
        spinnerMaxBandwidth.model = SpinnerNumberModel(0, 0, 1048576, 1)
        spinnerMaxBandwidth.limitToolbarWidth(100)
        spinnerMaxBandwidth.toolTipText =
            UiMessages.text("downloads.config.bandwidthHint") //NON-NLS

        //restore spinner setting from config
        val oldDownloadLimit = ApplicationConfiguration.getInstance().downloadRateLimit
        spinnerMaxBandwidth.value = oldDownloadLimit.toInt()
        spinnerMaxBandwidth.addChangeListener { scheduleDownloadRateLimitChangedEvent() }
    }

    private fun setupNumDownloadsSpinner() {
        val config = ApplicationConfiguration.getInstance()
        spinnerNumDownloads.putClientProperty("JComponent.roundRect", true)
        spinnerNumDownloads.model = SpinnerNumberModel(1, 1, 9, 1)
        spinnerNumDownloads.limitToolbarWidth(80)
        spinnerNumDownloads.toolTipText = UiMessages.text("downloads.config.parallel")
        spinnerNumDownloads.value = config.maxSimultaneousDownloads
        spinnerNumDownloads.addChangeListener {
            val maxNumDownloads = (spinnerNumDownloads.model.value as Number).toInt()
            config.maxSimultaneousDownloads = maxNumDownloads
            messageBus.publishAsync(ParallelDownloadNumberChangedEvent())
        }
    }

    private fun setupBrDirectDownloadCheckBox() {
        val config = ApplicationConfiguration.getInstance()
        cbUseCdnAwareDirectDownloader.isSelected = config.useCdnAwareDirectDownload
        cbUseCdnAwareDirectDownloader.addActionListener {
            config.useCdnAwareDirectDownload = cbUseCdnAwareDirectDownloader.isSelected
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleParallelDownloadNumberChange(e: ParallelDownloadNumberChangedEvent) {
        SwingUtilities.invokeLater {
            val maxNumDownloads = ApplicationConfiguration.getInstance().maxSimultaneousDownloads
            spinnerNumDownloads.value = maxNumDownloads
        }
    }

    private fun initComponents() {
        isFloatable = true
        name = UiMessages.text("downloads.config.title")
        setDownloadsToolBarId("config")

        add(JLabel("Downloads:"))
        add(spinnerNumDownloads)
        addSeparator()
        add(cbMaxBandwidth)
        add(spinnerMaxBandwidth)
        add(JLabel("KiB/s"))
        addSeparator()
        add(cbUseCdnAwareDirectDownloader)
    }

    private val spinnerNumDownloads: JSpinner = JSpinner()
    private val cbMaxBandwidth: JCheckBox = JCheckBox()
    private val cbUseCdnAwareDirectDownloader: JCheckBox = JCheckBox(UiMessages.text("downloads.config.cdn"))
    private val spinnerMaxBandwidth: JSpinner = JSpinner()
    private val downloadRateLimitChangeTimer = Timer(300) { fireDownloadRateLimitChangedEvent() }.apply {
        isRepeats = false
    }

    init {
        initComponents()
        cbMaxBandwidth.apply {
            toolTipText = UiMessages.text("downloads.config.limit")
            text = "Limit:"
            isFocusable = false
        }
        cbUseCdnAwareDirectDownloader.apply {
            isFocusable = false
            toolTipText =
                UiMessages.text("downloads.config.cdnHint")
        }
        setupNumDownloadsSpinner()
        setupDownloadRateLimitCheckBox()
        setupDownloadRateLimitSpinner()
        setupBrDirectDownloadCheckBox()
        messageBus.subscribe(this)
    }

    private fun JSpinner.limitToolbarWidth(width: Int) {
        preferredSize = Dimension(width, preferredSize.height)
        minimumSize = preferredSize
        maximumSize = preferredSize
    }
}
