package mediathek.gui.tabs.tab_downloads

import mediathek.gui.messages.DownloadRateLimitChangedEvent
import mediathek.gui.messages.ParallelDownloadNumberChangedEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus.messageBus
import net.engio.mbassy.listener.Handler
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import javax.swing.*
import javax.swing.border.TitledBorder

/**
 * @author Christian Franzke
 */
class DownloadsConfigPanel : JPanel() {
    private fun setupDownloadRateLimitCheckBox() {
        val config = ApplicationConfiguration.getConfiguration()
        val active = config.getBoolean(
            ApplicationConfiguration.DownloadRateLimiter.ACTIVE, false)
        cbMaxBandwidth.isSelected = active
        cbMaxBandwidth.addActionListener {
            config.setProperty(ApplicationConfiguration.DownloadRateLimiter.ACTIVE, cbMaxBandwidth.isSelected)
            fireDownloadRateLimitChangedEvent()
        }
    }

    private fun fireDownloadRateLimitChangedEvent() {
        val downloadLimit = spinnerMaxBandwidth.value as Int
        ApplicationConfiguration.getConfiguration()
            .setProperty(ApplicationConfiguration.DownloadRateLimiter.LIMIT, downloadLimit)
        val evt = DownloadRateLimitChangedEvent()
        evt.newLimit = downloadLimit
        evt.active = cbMaxBandwidth.isSelected
        messageBus.publishAsync(evt)
    }

    private fun setupDownloadRateLimitSpinner() {
        spinnerMaxBandwidth.putClientProperty("JComponent.roundRect", true)
        spinnerMaxBandwidth.model = SpinnerNumberModel(0, 0, 1048576, 1)
        spinnerMaxBandwidth.toolTipText =
            "<html>Bandbreitenbegrenzung eines Downloads in XX Kilobytes pro Sekunde.\n<b><br><u>WICHTIG:</u><br>ENTWEDER<br>den Wert \u00fcber die Pfeiltasten \u00e4ndern<br>ODER<br>Zahlen eingeben UND ENTER-Taste dr\u00fccken!</b>\n</html>" //NON-NLS

        //restore spinner setting from config
        val oldDownloadLimit =
            ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DownloadRateLimiter.LIMIT, 0)
        spinnerMaxBandwidth.value = oldDownloadLimit
        spinnerMaxBandwidth.addChangeListener { fireDownloadRateLimitChangedEvent() }
    }

    private fun setupNumDownloadsSpinner() {
        val config = ApplicationConfiguration.getConfiguration()
        spinnerNumDownloads.putClientProperty("JComponent.roundRect", true)
        spinnerNumDownloads.model = SpinnerNumberModel(1, 1, 9, 1)
        spinnerNumDownloads.value = config.getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, 1)
        spinnerNumDownloads.addChangeListener {
            val maxNumDownloads = (spinnerNumDownloads.model.value as Number).toInt()
            config.setProperty(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, maxNumDownloads)
            messageBus.publishAsync(ParallelDownloadNumberChangedEvent())
        }
    }

    @Handler
    private fun handleParallelDownloadNumberChange(e: ParallelDownloadNumberChangedEvent) {
        SwingUtilities.invokeLater {
            val maxNumDownloads = ApplicationConfiguration.getConfiguration()
                .getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, 1)
            spinnerNumDownloads.value = maxNumDownloads
        }
    }

    private fun initComponents() {
        val label1 = JLabel()
        val label2 = JLabel()
        val label3 = JLabel()

        border = TitledBorder("Downloads")
        layout = MigLayout(
            LC().insets("0").hideMode(3).align("center", "center"),
            // columns
            AC()
                .align("center").gap()
                .align("label").gap()
                .align("left").gap()
                .align("center"),
            // rows
            AC()
                .gap()
        )

        label1.text = "gleichzeitig:" //NON-NLS
        add(label1, CC().cell(1, 0))
        add(spinnerNumDownloads, CC().cell(2, 0).width("80:100")) //NON-NLS
        add(cbMaxBandwidth, CC().cell(0, 1))

        label2.text = "max. Bandbreite:" //NON-NLS
        add(label2, CC().cell(1, 1))
        add(spinnerMaxBandwidth, CC().cell(2, 1).width("80:100")) //NON-NLS

        label3.text = "KiB/s" //NON-NLS
        add(label3, CC().cell(3, 1))
    }

    private val spinnerNumDownloads: JSpinner = JSpinner()
    private val cbMaxBandwidth: JCheckBox = JCheckBox()
    private val spinnerMaxBandwidth: JSpinner = JSpinner()

    init {
        initComponents()
        cbMaxBandwidth.toolTipText = "Bandbreitenbegrenzung aktiviert?"
        setupNumDownloadsSpinner()
        setupDownloadRateLimitCheckBox()
        setupDownloadRateLimitSpinner()
        messageBus.subscribe(this)
    }
}