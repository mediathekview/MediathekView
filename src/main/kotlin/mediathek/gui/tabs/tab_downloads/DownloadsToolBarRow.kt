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

import com.jidesoft.popup.JidePopup
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignC
import java.awt.BorderLayout
import java.awt.Component
import java.awt.FlowLayout
import java.awt.Insets
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.ContainerAdapter
import java.awt.event.ContainerEvent
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JToolBar
import javax.swing.SwingUtilities
import javax.swing.plaf.basic.BasicToolBarUI

class DownloadsToolBarRow(
    private val primaryToolBar: JToolBar,
    private vararg val overflowToolBars: JToolBar
) : JPanel(FlowLayout(FlowLayout.LEFT, 0, 0)) {
    private val overflowButton = JButton(IconUtils.of(MaterialDesignC.CHEVRON_DOUBLE_RIGHT)).apply {
        toolTipText = "Weitere Download-Optionen"
        isFocusable = false
        isBorderPainted = false
        isContentAreaFilled = false
        isFocusPainted = false
        margin = Insets(0, 4, 0, 4)
        addActionListener { showOverflowPopup() }
    }
    private val overflowContent = JPanel(FlowLayout(FlowLayout.LEFT, 0, 0))
    private val overflowPopup = JidePopup().apply {
        contentPane.layout = BorderLayout()
        contentPane.add(overflowContent, BorderLayout.CENTER)
        owner = overflowButton
        isMovable = false
        isResizable = false
        isAttachable = false
        isTransient = true
        isFocusable = true
        isKeepPreviousSize = false
        defaultMoveOperation = JidePopup.HIDE_ON_MOVED
    }
    private var visibleOverflowToolBarCount = overflowToolBars.size
    private var updatePending = false
    private val statePersistence = DownloadsToolBarStatePersistence(this, listOf(primaryToolBar, *overflowToolBars))

    init {
        add(primaryToolBar)
        overflowToolBars.forEach(::add)
        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent?) {
                updateOverflowStateLater()
            }
        })
        addContainerListener(object : ContainerAdapter() {
            override fun componentAdded(e: ContainerEvent?) {
                updateOverflowStateLater()
            }
        })
        statePersistence.restoreLater()
    }

    override fun doLayout() {
        super.doLayout()
        updateOverflowStateLater()
    }

    private fun updateOverflowStateLater() {
        if (updatePending) {
            return
        }

        updatePending = true
        SwingUtilities.invokeLater {
            updatePending = false
            updateOverflowState()
        }
    }

    private fun updateOverflowState() {
        if (width <= 0) {
            return
        }

        val dockedOverflowToolBars = dockedOverflowToolBars()
        val newVisibleOverflowToolBarCount = calculateVisibleOverflowToolBarCount(dockedOverflowToolBars)
        val expectedComponents = expectedComponents(dockedOverflowToolBars, newVisibleOverflowToolBarCount)
        if (newVisibleOverflowToolBarCount == visibleOverflowToolBarCount && components.toList() == expectedComponents) {
            return
        }

        visibleOverflowToolBarCount = newVisibleOverflowToolBarCount
        overflowPopup.hidePopup()
        removeAll()
        expectedComponents.forEach(::add)

        revalidate()
        repaint()
    }

    private fun calculateVisibleOverflowToolBarCount(dockedOverflowToolBars: List<JToolBar>): Int {
        val fullWidth = primaryToolBar.dockedPreferredWidth() +
                dockedOverflowToolBars.sumOf { it.preferredSize.width }
        if (fullWidth <= width) {
            return dockedOverflowToolBars.size
        }

        var visibleToolBarCount = 0
        var usedWidth = primaryToolBar.dockedPreferredWidth() + overflowButton.preferredSize.width
        for (toolbar in dockedOverflowToolBars) {
            if (usedWidth + toolbar.preferredSize.width > width) {
                break
            }

            usedWidth += toolbar.preferredSize.width
            visibleToolBarCount++
        }
        return visibleToolBarCount
    }

    private fun showOverflowPopup() {
        overflowContent.removeAll()
        hiddenOverflowToolBars(dockedOverflowToolBars(), visibleOverflowToolBarCount).forEach { toolbar ->
            overflowContent.add(toolbar)
        }
        overflowContent.revalidate()
        overflowContent.repaint()
        overflowPopup.owner = overflowButton
        overflowPopup.showPopup(overflowButton)
    }

    private fun hiddenOverflowToolBars(dockedOverflowToolBars: List<JToolBar>, visibleToolBarCount: Int): List<JToolBar> =
        dockedOverflowToolBars.drop(visibleToolBarCount)

    private fun expectedComponents(dockedOverflowToolBars: List<JToolBar>, visibleToolBarCount: Int): List<Component> =
        buildList {
            if (!primaryToolBar.isFloating()) {
                add(primaryToolBar)
            }
            addAll(dockedOverflowToolBars.take(visibleToolBarCount))
            if (hiddenOverflowToolBars(dockedOverflowToolBars, visibleToolBarCount).isNotEmpty()) {
                add(overflowButton)
            }
        }

    private fun dockedOverflowToolBars(): List<JToolBar> =
        overflowToolBars.filterNot { it.isFloating() }

    private fun JToolBar.dockedPreferredWidth(): Int =
        if (isFloating()) 0 else preferredSize.width

    private fun JToolBar.isFloating(): Boolean =
        (ui as? BasicToolBarUI)?.isFloating == true
}
