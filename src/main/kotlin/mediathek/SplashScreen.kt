package mediathek

import mediathek.config.Konstanten
import mediathek.tool.TimerPool.timerPool
import mediathek.tool.UIProgressState
import org.apache.commons.lang3.SystemUtils
import java.awt.Color
import java.awt.Cursor
import java.awt.Dimension
import java.awt.Font
import java.util.*
import java.util.concurrent.TimeUnit
import javax.swing.*
import kotlin.math.roundToInt

class SplashScreen : JWindow() {
    private var versionLabel: JLabel? = null
    private var curSteps = 0.0
    private var appTitleLabel: JLabel? = null
    private var imageLabel: JLabel? = null
    private var progressBar: JProgressBar? = null
    private var statusLabel: JLabel? = null

    init {
        initComponents()
        contentPane.background = Color.BLACK
        val res = String.format("Version: %s (%s %s)", Konstanten.MVVERSION, osName, SystemUtils.OS_ARCH)
        versionLabel!!.text = res
        progressBar!!.value = 0
        setLocationRelativeTo(null)

        //strange behaviour on win where window will not come to front or stay there...
        if (SystemUtils.IS_OS_WINDOWS) {
            if (isAlwaysOnTopSupported) isAlwaysOnTop = true
        }
    }

    fun update(state: UIProgressState) {
        curSteps++
        val pct = (100 * (curSteps / MAXIMUM_STEPS)).roundToInt()
        updateStatus(state.toString(), pct)
    }

    fun close() {
        timerPool.schedule({ SwingUtilities.invokeLater { isVisible = false } }, 2, TimeUnit.SECONDS)
        Main.splashScreen = Optional.empty()
    }

    /**
     * Return "modern" macOS string for mac instead of legacy "Mac OS X".
     * According to apple dev docs even "old" 10.6 is now named macOS.
     *
     * @return "macOS" for mac otherwise the java OS name
     */
    private val osName: String
        get() {
            return if (SystemUtils.IS_OS_MAC_OSX) "macOS" else SystemUtils.OS_NAME
        }

    /**
     * Updates the percent complete bar and the associated status text.
     *
     * @param statusText      The new status text to display.
     * @param percentComplete The new percentage.
     */
    private fun updateStatus(statusText: String?, percentComplete: Int) {
        appTitleLabel!!.paintImmediately(0, 0, appTitleLabel!!.width, appTitleLabel!!.height)
        imageLabel!!.paintImmediately(0, 0, imageLabel!!.width, imageLabel!!.height)
        versionLabel!!.paintImmediately(0, 0, versionLabel!!.width, versionLabel!!.height)
        statusLabel!!.text = statusText
        statusLabel!!.paintImmediately(0, 0, statusLabel!!.width, statusLabel!!.height)
        progressBar!!.value = percentComplete
        progressBar!!.paintImmediately(0, 0, progressBar!!.width, progressBar!!.height)
    }

    private fun initComponents() {
        appTitleLabel = JLabel()
        versionLabel = JLabel()
        imageLabel = JLabel()
        progressBar = JProgressBar()
        statusLabel = JLabel()
        minimumSize = Dimension(640, 480)
        background = Color.black
        cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
        isAutoRequestFocus = false
        foreground = Color.black
        val contentPane = contentPane
        appTitleLabel!!.text = Konstanten.PROGRAMMNAME
        appTitleLabel!!.font = appTitleLabel!!.font.deriveFont(
            appTitleLabel!!.font.style or Font.BOLD,
            appTitleLabel!!.font.size + 45f
        )
        appTitleLabel!!.foreground = Color.white
        appTitleLabel!!.background = Color.black
        appTitleLabel!!.isOpaque = true
        versionLabel!!.text = "Version"
        versionLabel!!.isOpaque = true
        versionLabel!!.foreground = Color.white
        versionLabel!!.background = Color.black
        imageLabel!!.icon = ImageIcon(javaClass.getResource("/mediathek/res/MediathekView.png"))
        imageLabel!!.background = Color.black
        imageLabel!!.isOpaque = true
        progressBar!!.value = 50
        progressBar!!.preferredSize = Dimension(146, 10)
        statusLabel!!.text = "Status Text Message is here"
        statusLabel!!.foreground = Color.white
        statusLabel!!.background = Color.black
        statusLabel!!.isOpaque = true
        val contentPaneLayout = GroupLayout(contentPane)
        contentPane.layout = contentPaneLayout
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(
                    contentPaneLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(
                            contentPaneLayout.createParallelGroup()
                                .addComponent(versionLabel, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE.toInt())
                                .addComponent(progressBar, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE.toInt())
                                .addGroup(
                                    contentPaneLayout.createSequentialGroup()
                                        .addComponent(appTitleLabel)
                                        .addGap(0, 257, Short.MAX_VALUE.toInt())
                                )
                                .addComponent(statusLabel, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE.toInt())
                                .addGroup(
                                    GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                                        .addGap(0, 372, Short.MAX_VALUE.toInt())
                                        .addComponent(imageLabel)
                                )
                        )
                        .addContainerGap()
                )
        )
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(
                    contentPaneLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(appTitleLabel)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(versionLabel)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 94, Int.MAX_VALUE)
                        .addComponent(imageLabel)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(statusLabel)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(
                            progressBar,
                            GroupLayout.PREFERRED_SIZE,
                            GroupLayout.DEFAULT_SIZE,
                            GroupLayout.PREFERRED_SIZE
                        )
                        .addContainerGap()
                )
        )
        pack()
    }

    companion object {
        private val MAXIMUM_STEPS = EnumSet.allOf(UIProgressState::class.java).size - 1.0
    }
}