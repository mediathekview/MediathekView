/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.swing

import java.awt.Dimension
import java.awt.Rectangle
import javax.swing.*
import javax.swing.plaf.UIResource
import javax.swing.text.DefaultCaret

class MultilineLabel : JTextArea() {
    private fun initComponents() {
        adjustUI()
    }

    override fun updateUI() {
        super.updateUI()
        adjustUI()
    }

    /**
     * Adjusts UI to make sure it looks like a label instead of a text area.
     */
    private fun adjustUI() {
        lineWrap = true
        wrapStyleWord = true
        isEditable = false
        isRequestFocusEnabled = false
        isFocusable = false
        setComponentTransparent(this)
        caret = object : DefaultCaret() {
            override fun adjustVisibility(nloc: Rectangle) {}
        }
        LookAndFeel.installBorder(this, "Label.border")
        val fg = foreground
        if (fg == null || fg is UIResource) {
            foreground = UIManager.getColor("Label.foreground")
        }
        val f = font
        if (f == null || f is UIResource) {
            font = UIManager.getFont("Label.font")
        }
        background = null
    }

    override fun getMinimumSize(): Dimension {
        return preferredSize
    }

    private fun setComponentTransparent(component: JComponent) {
        component.isOpaque = false
        component.putClientProperty("Nimbus.Overrides.InheritDefaults", false)
        component.putClientProperty("Nimbus.Overrides", UIDefaults())
    }

    init {
        initComponents()
    }
}