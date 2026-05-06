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

package mediathek.mainwindow

import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.extras.FlatAnimatedLafChange
import com.formdev.flatlaf.ui.FlatUIUtils
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.gui.messages.FontSizeChangedEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import org.apache.commons.configuration2.sync.LockMode
import java.awt.Component
import java.awt.GraphicsEnvironment
import java.awt.Toolkit
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.*
import javax.swing.text.StyleContext
import kotlin.math.max

/**
 * Helper class to globally change to L&F font sizes.
 */
class FontManager(private val fontMenu: JMenu) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val availableFontFamilyNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
        .availableFontFamilyNames
        .copyOf()
        .also { it.sort() }
    private val restoreFontMenuItem = JMenuItem()
    private val incrFontMenuItem = JMenuItem()
    private val decrFontMenuItem = JMenuItem()

    init {
        initStandardMenuEntries()
        createStandardMenuEntries()
        updateFontMenuItems()
    }

    private fun initStandardMenuEntries() {
        restoreFontMenuItem.text = "Schrift zurücksetzen"
        restoreFontMenuItem.accelerator = KeyStroke.getKeyStroke(
            KeyEvent.VK_0,
            Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx,
        )
        restoreFontMenuItem.addActionListener { runFontUpdate(::resetFont) }

        incrFontMenuItem.text = "Schrift vergrößern"
        incrFontMenuItem.accelerator = KeyStroke.getKeyStroke(
            KeyEvent.VK_PLUS,
            Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx,
        )
        incrFontMenuItem.addActionListener { runFontUpdate(::increaseFontSize) }

        decrFontMenuItem.text = "Schrift verkleinern"
        decrFontMenuItem.accelerator = KeyStroke.getKeyStroke(
            KeyEvent.VK_MINUS,
            Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx,
        )
        decrFontMenuItem.addActionListener { runFontUpdate(::decreaseFontSize) }
    }

    private fun runFontUpdate(action: () -> Unit) {
        uiScope.launch {
            action()
        }
    }

    private fun createStandardMenuEntries() {
        fontMenu.add(restoreFontMenuItem)
        fontMenu.add(incrFontMenuItem)
        fontMenu.add(decrFontMenuItem)
    }

    /**
     * Reset used L&F font back to default.
     */
    private fun resetFont() {
        applyFontChange(updateMenuItems = true) {
            UIManager.put(KEY_DEFAULT_FONT, null)
        }

        clearConfigData()
        publishFontSizeChanged()
    }

    private fun increaseFontSize() {
        applyFontChange(updateMenuItems = true) {
            val font = UIManager.getFont(KEY_DEFAULT_FONT)
            UIManager.put(KEY_DEFAULT_FONT, font.deriveFont((font.size + 1).toFloat()))
        }
        writeConfigData()
        publishFontSizeChanged()
    }

    private fun decreaseFontSize() {
        applyFontChange(updateMenuItems = true) {
            val font = UIManager.getFont(KEY_DEFAULT_FONT)
            UIManager.put(KEY_DEFAULT_FONT, font.deriveFont(max(font.size - 1, 10).toFloat()))
        }
        writeConfigData()
        publishFontSizeChanged()
    }

    /**
     * Store the font data in configuration.
     */
    private fun writeConfigData() {
        val currentFont = UIManager.getFont(KEY_LABEL_FONT)
        val currentFamily = currentFont.family
        val currentSize = currentFont.size

        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.WRITE)
            config.setProperty(CONFIG_DEFAULT_FONT_FAMILY, currentFamily)
            config.setProperty(CONFIG_DEFAULT_FONT_SIZE, currentSize)
        } finally {
            config.unlock(LockMode.WRITE)
        }
    }

    private fun clearConfigData() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.WRITE)
            config.clearProperty(CONFIG_DEFAULT_FONT_SIZE)
            config.clearProperty(CONFIG_DEFAULT_FONT_FAMILY)
        } finally {
            config.unlock(LockMode.WRITE)
        }
    }

    fun restoreConfigData() {
        FlatAnimatedLafChange.showSnapshot()

        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.READ)
            val currentFamily = config.getString(CONFIG_DEFAULT_FONT_FAMILY)
            val currentSize = config.getInt(CONFIG_DEFAULT_FONT_SIZE)

            val font = UIManager.getFont(KEY_DEFAULT_FONT)
            var newFont = StyleContext.getDefaultStyleContext().getFont(currentFamily, font.style, currentSize)
            newFont = FlatUIUtils.nonUIResource(newFont)
            UIManager.put(KEY_DEFAULT_FONT, newFont)
        } catch (_: Exception) {
        } finally {
            config.unlock(LockMode.READ)
        }

        FlatLaf.updateUI()
        FlatAnimatedLafChange.hideSnapshotWithAnimation()

        updateFontMenuItems()
    }

    private fun updateFontMenuItems() {
        fontMenu.removeAll()
        createStandardMenuEntries()
        fontMenu.addSeparator()

        val currentFont = UIManager.getFont(KEY_LABEL_FONT)
        val currentFamily = currentFont.family
        val currentSize = currentFont.size.toString()

        val families = preferredFontFamilies()
        if (!families.contains(currentFamily)) {
            families.add(currentFamily)
        }
        families.sortWith(String.CASE_INSENSITIVE_ORDER)

        val familiesGroup = ButtonGroup()
        for (family in families) {
            if (availableFontFamilyNames.binarySearch(family) < 0) {
                continue
            }

            val item = JCheckBoxMenuItem(family)
            item.isSelected = family == currentFamily
            item.addActionListener(::fontFamilyChanged)
            fontMenu.add(item)

            familiesGroup.add(item)
        }

        fontMenu.addSeparator()
        val sizes = preferredFontSizes()
        if (!sizes.contains(currentSize)) {
            sizes.add(currentSize)
        }
        sizes.sortWith(String.CASE_INSENSITIVE_ORDER)

        val sizesGroup = ButtonGroup()
        for (size in sizes) {
            val item = JCheckBoxMenuItem(size)
            item.isSelected = size == currentSize
            item.addActionListener(::fontSizeChanged)
            fontMenu.add(item)

            sizesGroup.add(item)
        }

        val enabled = UIManager.getLookAndFeel() is FlatLaf
        for (item: Component in fontMenu.menuComponents) {
            item.isEnabled = enabled
        }
    }

    private fun fontFamilyChanged(event: ActionEvent) {
        val fontFamily = event.actionCommand

        runFontUpdate {
            applyFontChange {
                val font = UIManager.getFont(KEY_DEFAULT_FONT)
                var newFont = StyleContext.getDefaultStyleContext().getFont(fontFamily, font.style, font.size)
                newFont = FlatUIUtils.nonUIResource(newFont)
                UIManager.put(KEY_DEFAULT_FONT, newFont)
            }
            writeConfigData()
            publishFontSizeChanged()
        }
    }

    private fun fontSizeChanged(event: ActionEvent) {
        val fontSize = event.actionCommand.toInt()

        runFontUpdate {
            applyFontChange {
                val font = UIManager.getFont(KEY_DEFAULT_FONT)
                UIManager.put(KEY_DEFAULT_FONT, font.deriveFont(fontSize.toFloat()))
            }
            writeConfigData()
            publishFontSizeChanged()
        }
    }

    private fun applyFontChange(
        updateMenuItems: Boolean = false,
        change: () -> Unit,
    ) {
        FlatAnimatedLafChange.showSnapshot()

        change()
        if (updateMenuItems) {
            updateFontMenuItems()
        }
        FlatLaf.updateUI()

        FlatAnimatedLafChange.hideSnapshotWithAnimation()
    }

    private fun publishFontSizeChanged() {
        MessageBus.messageBus.publishAsync(FontSizeChangedEvent())
    }

    private fun preferredFontFamilies() = mutableListOf(
        "Arial", "Cantarell", "Comic Sans MS", "DejaVu Sans",
        "Dialog", "Liberation Sans", "Noto Sans", "Roboto",
        "SansSerif", "Segoe UI", "Serif", "Tahoma", "Ubuntu", "Verdana",
    )

    private fun preferredFontSizes() = mutableListOf("10", "11", "12", "13", "14", "16", "18", "20", "24", "28")

    private companion object {
        const val KEY_DEFAULT_FONT = "defaultFont"
        const val KEY_LABEL_FONT = "Label.font"
        const val CONFIG_DEFAULT_FONT_SIZE = "ui.default_font.size"
        const val CONFIG_DEFAULT_FONT_FAMILY = "ui.default_font.family"
    }
}
