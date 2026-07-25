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

package mediathek.gui.watchlist

import mediathek.daten.watchlist.WatchlistNotification
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignC
import java.awt.*
import javax.swing.*

private const val NOTIFICATION_ROW_PREFERRED_WIDTH = 600
private const val NOTIFICATION_PANEL_INSET = 5
private const val NOTIFICATION_PANEL_PREFERRED_WIDTH = NOTIFICATION_ROW_PREFERRED_WIDTH + (NOTIFICATION_PANEL_INSET * 2)
private const val NOTIFICATION_PANEL_PREFERRED_HEIGHT = 320
private const val SCREEN_EDGE_MARGIN = 10

/**
 * Shows pending watchlist "new episode" notifications as stacked row cards, modelled
 * after the audiothek download manager panel. Every row offers a context menu with the
 * available actions and an 'x' button that removes the notification immediately.
 */
class WatchlistNotificationPanel : JPanel(BorderLayout()) {
    private val contentPanel = JPanel().apply {
        layout = BoxLayout(this, BoxLayout.Y_AXIS)
        isOpaque = false
        border = BorderFactory.createEmptyBorder(
            NOTIFICATION_PANEL_INSET,
            NOTIFICATION_PANEL_INSET,
            NOTIFICATION_PANEL_INSET,
            NOTIFICATION_PANEL_INSET
        )
    }
    private val scrollPane = JScrollPane(contentPanel)
    private var showInFilmTableListener: ((WatchlistNotification) -> Unit)? = null
    private var recordFilmListener: ((WatchlistNotification) -> Unit)? = null
    private var removeEntryListener: ((WatchlistNotification) -> Unit)? = null
    private var removeNotificationListener: ((WatchlistNotification) -> Unit)? = null
    private var emptyListener: (() -> Unit)? = null
    private var previousNotificationCount = 0
    private var currentNotifications: List<WatchlistNotification> = emptyList()
    private val rowPanels = LinkedHashMap<WatchlistNotification, WatchlistNotificationRowPanel>()

    init {
        preferredSize = Dimension(NOTIFICATION_PANEL_PREFERRED_WIDTH, NOTIFICATION_PANEL_PREFERRED_HEIGHT)
        minimumSize = Dimension(NOTIFICATION_PANEL_INSET * 2, 120)
        val popoverBackground = UIManager.getColor("Panel.background") ?: background
        background = popoverBackground
        isOpaque = true
        contentPanel.background = popoverBackground

        scrollPane.border = BorderFactory.createEmptyBorder()
        scrollPane.viewport.background = popoverBackground
        scrollPane.horizontalScrollBarPolicy = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER

        add(scrollPane, BorderLayout.CENTER)
    }

    /**
     * Fits the popup into the owner's screen and returns its screen location. Must be
     * called before the popup is shown with the returned coordinates.
     */
    fun fitToScreen(owner: Component): Point {
        val graphicsConfiguration = owner.graphicsConfiguration
        val ownerBounds = Rectangle(owner.locationOnScreen, owner.size)
        if (graphicsConfiguration == null) {
            preferredSize = Dimension(NOTIFICATION_PANEL_PREFERRED_WIDTH, NOTIFICATION_PANEL_PREFERRED_HEIGHT)
            return ownerBounds.location
        }

        val popupBounds = calculatePopupBounds(
            ownerBounds,
            graphicsConfiguration.bounds,
            Toolkit.getDefaultToolkit().getScreenInsets(graphicsConfiguration),
        )
        preferredSize = popupBounds.size
        return popupBounds.location
    }

    internal fun calculatePopupBounds(
        ownerBounds: Rectangle,
        screenBounds: Rectangle,
        screenInsets: Insets
    ): Rectangle {
        val usableLeft = screenBounds.x + screenInsets.left + SCREEN_EDGE_MARGIN
        val usableTop = screenBounds.y + screenInsets.top + SCREEN_EDGE_MARGIN
        val usableRight = screenBounds.x + screenBounds.width - screenInsets.right - SCREEN_EDGE_MARGIN
        val usableBottom = screenBounds.y + screenBounds.height - screenInsets.bottom - SCREEN_EDGE_MARGIN
        val usableWidth = (usableRight - usableLeft).coerceAtLeast(1)
        val spaceAbove = (ownerBounds.y - usableTop).coerceAtLeast(0)
        val spaceBelow = (usableBottom - ownerBounds.y - ownerBounds.height).coerceAtLeast(0)
        val showBelow = NOTIFICATION_PANEL_PREFERRED_HEIGHT <= spaceBelow || spaceBelow >= spaceAbove
        val availableHeight = if (showBelow) spaceBelow else spaceAbove
        val width = NOTIFICATION_PANEL_PREFERRED_WIDTH.coerceAtMost(usableWidth)
        val height = NOTIFICATION_PANEL_PREFERRED_HEIGHT.coerceAtMost(availableHeight.coerceAtLeast(1))
        val x = ownerBounds.x.coerceIn(usableLeft, usableRight - width)
        val y = if (showBelow) ownerBounds.y + ownerBounds.height else ownerBounds.y - height
        return Rectangle(x, y, width, height)
    }

    fun setNotifications(notifications: List<WatchlistNotification>) {
        val hadRows = currentNotifications.isNotEmpty()
        val shouldScrollToEnd = notifications.size > previousNotificationCount
        previousNotificationCount = notifications.size
        currentNotifications = notifications

        val removedNotifications = rowPanels.keys - notifications.toSet()
        removedNotifications.forEach(rowPanels::remove)

        notifications.forEach { notification ->
            if (!rowPanels.containsKey(notification)) {
                rowPanels[notification] = WatchlistNotificationRowPanel(
                    notification,
                    showInFilmTableListener,
                    recordFilmListener,
                    removeEntryListener,
                    removeNotificationListener,
                )
            }
        }

        contentPanel.removeAll()
        if (notifications.isEmpty()) {
            contentPanel.add(createEmptyHint())
        } else {
            notifications.forEachIndexed { index, notification ->
                contentPanel.add(rowPanels.getValue(notification))
                if (index < notifications.lastIndex) {
                    contentPanel.add(Box.createVerticalStrut(8))
                }
            }
        }
        contentPanel.add(Box.createVerticalGlue())
        contentPanel.revalidate()
        contentPanel.repaint()

        if (shouldScrollToEnd) {
            SwingUtilities.invokeLater {
                scrollPane.verticalScrollBar.value = scrollPane.verticalScrollBar.maximum
            }
        }
        if (hadRows && notifications.isEmpty()) {
            emptyListener?.invoke()
        }
    }

    internal fun rowPanelFor(notification: WatchlistNotification): WatchlistNotificationRowPanel? =
        rowPanels[notification]

    private fun createEmptyHint(): JComponent {
        val hintLabel = JLabel("Keine neuen Folgen", SwingConstants.CENTER)
        hintLabel.alignmentX = CENTER_ALIGNMENT
        hintLabel.foreground = UIManager.getColor("Label.disabledForeground") ?: hintLabel.foreground
        return hintLabel
    }

    fun addShowInFilmTableListener(listener: (WatchlistNotification) -> Unit) {
        showInFilmTableListener = listener
    }

    fun addRecordFilmListener(listener: (WatchlistNotification) -> Unit) {
        recordFilmListener = listener
    }

    fun addRemoveEntryListener(listener: (WatchlistNotification) -> Unit) {
        removeEntryListener = listener
    }

    fun addRemoveNotificationListener(listener: (WatchlistNotification) -> Unit) {
        removeNotificationListener = listener
    }

    fun addEmptyListener(listener: () -> Unit) {
        emptyListener = listener
    }
}

internal class WatchlistNotificationRowPanel(
    private val notification: WatchlistNotification,
    private val showInFilmTableListener: ((WatchlistNotification) -> Unit)?,
    private val recordFilmListener: ((WatchlistNotification) -> Unit)?,
    private val removeEntryListener: ((WatchlistNotification) -> Unit)?,
    private val removeNotificationListener: ((WatchlistNotification) -> Unit)?,
) : JPanel(GridBagLayout()) {
    private val titleLabel = JLabel()
    private val subtitleLabel = JLabel()

    val removeButton: JButton = JButton(IconUtils.of(MaterialDesignC.CLOSE_CIRCLE_OUTLINE, 18)).apply {
        toolTipText = "Benachrichtigung entfernen"
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
        isBorderPainted = false
        isContentAreaFilled = false
        isFocusPainted = false
        addActionListener { removeNotificationListener?.invoke(notification) }
    }

    init {
        val borderColor = UIManager.getColor("Component.borderColor") ?: Color.LIGHT_GRAY
        border = BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(borderColor, 1, true),
            BorderFactory.createEmptyBorder(8, 12, 8, 12)
        )
        background = UIManager.getColor("Panel.background") ?: background
        preferredSize = Dimension(NOTIFICATION_ROW_PREFERRED_WIDTH, 64)
        maximumSize = Dimension(Int.MAX_VALUE, 64)
        alignmentX = LEFT_ALIGNMENT
        isFocusable = true

        titleLabel.text = ellipsize(notification.title, 90)
        titleLabel.toolTipText = notification.title
        titleLabel.font = titleLabel.font.deriveFont(Font.BOLD)

        subtitleLabel.text = ellipsize(subtitle(), 100)
        subtitleLabel.toolTipText = subtitle()

        val gbc = GridBagConstraints().apply {
            insets = Insets(2, 2, 2, 2)
            fill = GridBagConstraints.HORIZONTAL
            anchor = GridBagConstraints.WEST
        }

        gbc.gridx = 0
        gbc.gridy = 0
        gbc.weightx = 1.0
        add(titleLabel, gbc)

        gbc.gridy = 1
        add(subtitleLabel, gbc)

        gbc.gridx = 1
        gbc.gridy = 0
        gbc.gridheight = 2
        gbc.weightx = 0.0
        gbc.fill = GridBagConstraints.VERTICAL
        gbc.anchor = GridBagConstraints.CENTER
        gbc.insets = Insets(2, 8, 2, 2)
        add(removeButton, gbc)

        installContextMenu()
    }

    private fun subtitle(): String =
        listOf(notification.sender, notification.thema, notification.sendeDatum)
            .filter { part -> part.isNotEmpty() }
            .joinToString(" · ")

    /**
     * Uses [setComponentPopupMenu] instead of a mouse listener: the labels register mouse
     * listeners through their tooltips, so a plain listener on this panel would never see
     * a right click on the row text. Inheriting the menu also enables the keyboard menu key.
     */
    private fun installContextMenu() {
        componentPopupMenu = createContextMenu()
        titleLabel.inheritsPopupMenu = true
        subtitleLabel.inheritsPopupMenu = true
        removeButton.inheritsPopupMenu = true
    }

    private fun createContextMenu(): JPopupMenu {
        val popupMenu = JPopupMenu()

        val showItem = JMenuItem("In Filmliste anzeigen")
        showItem.addActionListener { showInFilmTableListener?.invoke(notification) }
        popupMenu.add(showItem)

        val recordItem = JMenuItem("Film aufzeichnen...")
        recordItem.addActionListener { recordFilmListener?.invoke(notification) }
        popupMenu.add(recordItem)

        popupMenu.addSeparator()

        val removeEntryItem = JMenuItem("Sendung von Watchlist entfernen")
        removeEntryItem.addActionListener { removeEntryListener?.invoke(notification) }
        popupMenu.add(removeEntryItem)

        return popupMenu
    }

    private fun ellipsize(text: String, maxChars: Int): String {
        if (text.length <= maxChars) {
            return text
        }
        return text.take(maxChars - 3) + "..."
    }
}
