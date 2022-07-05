/*
 * $Id: Highlighter.java 3100 2008-10-14 22:33:10Z rah003 $
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
package org.jdesktop.swingx.decorator;

import javax.swing.event.ChangeListener;
import java.awt.*;

/**
 * <code>Highlighter</code> provide a mechanism to modify visual attributes of
 * cell rendering components. The mechanism is uniform across both rendered and
 * rendering component types: it is the same for SwingX collection views
 * (JXTable, JXList, JXTree/Table) and independent of the concrete component
 * type used for rendering the cell. The view cell state is factored into a
 * <code>ComponentAdapter</code>.
 * <p>
 * 
 * For example, in data visualization components that support multiple columns
 * with potentially different types of data, a <code>ColorHighlighter</code>
 * imparts the same background color consistently across <em>all</em> columns
 * of the rendered component regardless of the actual cell renderer registered
 * for any specific column.
 * <p>
 * 
 * The highlightable properties are basically defined by the renderer in use:
 * only attributes the renderer guarantees to reset on every call are safe to
 * alter. For SwingX renderering support these are listed in
 * <code>ComponentProvider</code>.
 * 
 * 
 * Implementations supporting mutable internal state which effects the
 * decoration must notify its listeners about the change. Typically, the
 * rendered component installs a listener to its <code>Highlighter</code>s
 * and triggeres a repaint on notification.
 * 
 * @see ComponentAdapter
 * @see org.jdesktop.swingx.renderer.ComponentProvider
 * 
 * @author Ramesh Gupta
 * @author Jeanette Winzenburg
 */
public interface Highlighter {

    /**
     * Decorates the specified component for the given component
     * adapter.
     *
     * @param renderer the cell rendering component that is to be decorated
     * @param adapter the ComponentAdapter for this decorate operation
     * @return the decorated cell rendering component
     */
    Component highlight(Component renderer, ComponentAdapter adapter);

    /**
     * Adds a <code>ChangeListener</code> which are 
     * notified after changes of any attribute. 
     *
     * @param l the ChangeListener to add
     * @see #removeChangeListener
     */
    void addChangeListener(ChangeListener l);

    /**
     * Removes a <code>ChangeListener</code>.
     *
     * @param l the <code>ChangeListener</code> to remove
     * @see #addChangeListener
     */
    void removeChangeListener(ChangeListener l);

    /**
     * Returns an array of all the change listeners
     * registered on this <code>LegacyHighlighter</code>.
     *
     * @return all of this model's <code>ChangeListener</code>s 
     *         or an empty
     *         array if no change listeners are currently registered
     *
     * @see #addChangeListener
     * @see #removeChangeListener
     *
     * @since 1.4
     */
    ChangeListener[] getChangeListeners();

}