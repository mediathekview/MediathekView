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

package mediathek.swing

import java.awt.Component
import java.awt.Container
import java.awt.Point
import java.awt.Rectangle
import java.awt.event.MouseEvent
import javax.swing.AbstractButton
import javax.swing.SwingUtilities
import javax.swing.UIManager

/**
 * A JIDE checkbox list whose mouse hit area follows the checkbox icon painted by the cell renderer.
 */
open class CheckBoxList : com.jidesoft.swing.CheckBoxList() {
    override fun createHandler(): Handler = PreciseCheckBoxHandler(this)

    private fun checkBoxIconBounds(point: Point): Rectangle? {
        val index = locationToIndex(point)
        if (index < 0) {
            return null
        }

        val cellBounds = getCellBounds(index, index) ?: return null
        if (!cellBounds.contains(point)) {
            return null
        }

        val rendererComponent = cellRenderer.getListCellRendererComponent(
            this,
            model.getElementAt(index),
            index,
            isSelectedIndex(index),
            hasFocus() && leadSelectionIndex == index,
        )
        rendererComponent.setBounds(0, 0, cellBounds.width, cellBounds.height)
        layoutRecursively(rendererComponent)

        val checkBox = findCheckBox(rendererComponent) ?: return null
        val icon = checkBox.icon ?: UIManager.getIcon("CheckBox.icon") ?: return null
        val iconBounds = Rectangle()
        SwingUtilities.layoutCompoundLabel(
            checkBox,
            checkBox.getFontMetrics(checkBox.font),
            checkBox.text.orEmpty(),
            icon,
            checkBox.verticalAlignment,
            checkBox.horizontalAlignment,
            checkBox.verticalTextPosition,
            checkBox.horizontalTextPosition,
            SwingUtilities.calculateInnerArea(checkBox, Rectangle()),
            iconBounds,
            Rectangle(),
            checkBox.iconTextGap,
        )

        val checkBoxLocation = SwingUtilities.convertPoint(checkBox, Point(), rendererComponent)
        iconBounds.translate(
            cellBounds.x + checkBoxLocation.x,
            cellBounds.y + checkBoxLocation.y,
        )
        return iconBounds
    }

    private fun layoutRecursively(component: Component) {
        if (component is Container) {
            component.doLayout()
            component.components.forEach(::layoutRecursively)
        }
    }

    private fun findCheckBox(component: Component): AbstractButton? {
        if (component is AbstractButton) {
            return component
        }
        if (component is Container) {
            component.components.forEach { child ->
                findCheckBox(child)?.let { return it }
            }
        }
        return null
    }

    private class PreciseCheckBoxHandler(
        private val list: CheckBoxList,
    ) : Handler(list) {
        override fun clicksInCheckBox(event: MouseEvent): Boolean {
            return list.checkBoxIconBounds(event.point)?.contains(event.point) == true
        }
    }
}
