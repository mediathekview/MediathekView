package org.jdesktop.swingx

import java.awt.Component
import java.awt.Container
import java.awt.Dimension
import java.awt.LayoutManager
import java.io.Serializable
import kotlin.math.max

abstract class AbstractLayoutManager : LayoutManager, Serializable {
    override fun addLayoutComponent(name: String, comp: Component) {
        //do nothing
    }

    override fun removeLayoutComponent(comp: Component) {
        // do nothing
    }

    override fun minimumLayoutSize(parent: Container): Dimension {
        return preferredLayoutSize(parent)
    }

    companion object {
        private const val serialVersionUID = 1446292747820044161L
    }
}

/**
 * SwingX VerticalLayout implementation recreated in Kotlin.
 * Unfortunately SwingX is not maintained anymore :(
 */
class VerticalLayout
@JvmOverloads constructor(var gap: Int = 0) : AbstractLayoutManager() {
    internal class Separator(private var next: Int, private val separator: Int) {
        fun get(): Int {
            val result = next
            next = separator
            return result
        }
    }

    override fun preferredLayoutSize(parent: Container): Dimension {
        val pref = Dimension(0, 0)
        val sep = Separator(0, gap)
        var i = 0
        val c = parent.componentCount
        while (i < c) {
            val m = parent.getComponent(i)
            if (m.isVisible) {
                val componentPreferredSize = parent.getComponent(i).preferredSize
                pref.height += componentPreferredSize.height + sep.get()
                pref.width = max(pref.width, componentPreferredSize.width)
            }
            i++
        }
        val insets = parent.insets
        pref.width += insets.left + insets.right
        pref.height += insets.top + insets.bottom
        return pref
    }

    override fun layoutContainer(parent: Container) {
        val insets = parent.insets
        val size = parent.size
        val width = size.width - insets.left - insets.right
        var height = insets.top
        var i = 0
        val c = parent.componentCount
        while (i < c) {
            val m = parent.getComponent(i)
            if (m.isVisible) {
                m.setBounds(insets.left, height, width, m.preferredSize.height)
                height += m.size.height + gap
            }
            i++
        }
    }

    companion object {
        private const val serialVersionUID = 5342270033773736441L
    }
}