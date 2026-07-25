package mediathek.swing

import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.themes.FlatMacLightLaf
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import java.awt.Point
import java.awt.Rectangle
import java.awt.event.KeyListener
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.AbstractButton
import javax.swing.DefaultListModel
import javax.swing.SwingUtilities
import javax.swing.UIManager

class CheckBoxListTest {
    @Test
    fun `clicking each corner of painted checkbox toggles it`() {
        withFlatMacLightLookAndFeel {
            val list = createList()
            val iconBounds = renderedCheckBoxIconBounds(list)
            val corners = listOf(
                Point(iconBounds.x, iconBounds.y),
                Point(iconBounds.x + iconBounds.width - 1, iconBounds.y),
                Point(iconBounds.x, iconBounds.y + iconBounds.height - 1),
                Point(iconBounds.x + iconBounds.width - 1, iconBounds.y + iconBounds.height - 1),
            )

            corners.forEach { point ->
                list.checkBoxListSelectionModel.clearSelection()
                val event = mousePressed(list, point)

                checkBoxMouseHandler(list).mousePressed(event)

                assertTrue(
                    list.checkBoxListSelectionModel.isSelectedIndex(0),
                    "painted checkbox corner $point must toggle",
                )
                assertTrue(event.isConsumed, "painted checkbox corner $point must consume the event")
            }
        }
    }

    @Test
    fun `clicking immediately beside painted checkbox does not toggle it`() {
        withFlatMacLightLookAndFeel {
            val list = createList()
            val iconBounds = renderedCheckBoxIconBounds(list)
            val centerX = iconBounds.x + iconBounds.width / 2
            val centerY = iconBounds.y + iconBounds.height / 2
            val outsidePoints = listOf(
                Point(iconBounds.x - 1, centerY),
                Point(iconBounds.x + iconBounds.width, centerY),
                Point(centerX, iconBounds.y - 1),
                Point(centerX, iconBounds.y + iconBounds.height),
            )

            outsidePoints.forEach { point ->
                list.checkBoxListSelectionModel.clearSelection()
                val event = mousePressed(list, point)

                checkBoxMouseHandler(list).mousePressed(event)

                assertFalse(
                    list.checkBoxListSelectionModel.isSelectedIndex(0),
                    "point $point outside the painted checkbox must not toggle",
                )
                assertFalse(event.isConsumed, "point $point outside the painted checkbox must not consume the event")
            }
        }
    }

    private fun createList(): CheckBoxList {
        return CheckBoxList().apply {
            model = DefaultListModel<String>().apply { addElement("Sender") }
            setSize(240, 40)
        }
    }

    private fun mousePressed(list: CheckBoxList, point: Point): MouseEvent {
        return MouseEvent(
            list,
            MouseEvent.MOUSE_PRESSED,
            System.currentTimeMillis(),
            0,
            point.x,
            point.y,
            1,
            false,
            MouseEvent.BUTTON1,
        )
    }

    private fun renderedCheckBoxIconBounds(list: CheckBoxList): Rectangle {
        val cellBounds = requireNotNull(list.getCellBounds(0, 0))
        val renderer = list.cellRenderer.getListCellRendererComponent(list, list.model.getElementAt(0), 0, false, false)
        renderer.setBounds(0, 0, cellBounds.width, cellBounds.height)
        layoutRecursively(renderer)

        val checkBox = requireNotNull(findCheckBox(renderer))
        val icon = checkBox.icon ?: requireNotNull(UIManager.getIcon("CheckBox.icon"))
        val checkBoxLocation = SwingUtilities.convertPoint(checkBox, Point(), renderer)
        return Rectangle(
            cellBounds.x + checkBoxLocation.x + checkBox.insets.left,
            cellBounds.y + checkBoxLocation.y + (checkBox.height - icon.iconHeight) / 2,
            icon.iconWidth,
            icon.iconHeight,
        )
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

    private fun checkBoxMouseHandler(list: CheckBoxList): MouseListener {
        return list.mouseListeners.first { it is KeyListener }
    }

    private fun withFlatMacLightLookAndFeel(action: () -> Unit) {
        SwingUtilities.invokeAndWait {
            val previousLookAndFeel = UIManager.getLookAndFeel()
            try {
                FlatLaf.setup(FlatMacLightLaf())
                action()
            } finally {
                UIManager.setLookAndFeel(previousLookAndFeel)
            }
        }
    }
}
