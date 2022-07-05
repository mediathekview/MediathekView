package org.jdesktop.swingx

import java.awt.Component
import java.awt.Container
import java.awt.Dimension
import java.awt.LayoutManager
import java.io.Serial
import java.io.Serializable

/**
 * A simple abstract class to handle common layout implementations. Package-private as we do NOT
 * want to export this as part of the public API.
 *
 * @author kschaefer
 */
abstract class AbstractLayoutManager : LayoutManager, Serializable {
    /**
     * {@inheritDoc}
     *
     *
     * This implementation does nothing.
     */
    override fun addLayoutComponent(name: String, comp: Component) {
        //does nothing
    }

    /**
     * {@inheritDoc}
     *
     *
     * This implementation does nothing.
     */
    override fun removeLayoutComponent(comp: Component) {
        // does nothing
    }

    /**
     * {@inheritDoc}
     *
     *
     * This implementation defers to [.preferredLayoutSize].
     */
    override fun minimumLayoutSize(parent: Container): Dimension {
        return preferredLayoutSize(parent)
    }

    companion object {
        @Serial
        private val serialVersionUID = 1446292747820044161L
    }
}