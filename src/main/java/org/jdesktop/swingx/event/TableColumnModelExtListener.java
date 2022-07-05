/*
 * $Id: TableColumnModelExtListener.java 1395 2006-09-14 14:40:12Z kleopatra $
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
package org.jdesktop.swingx.event;

import javax.swing.event.TableColumnModelListener;
import java.beans.PropertyChangeEvent;

/**
 * Extended <code>TableColumnModelListener</code> which is interested 
 * in property changes of contained <code>TableColumn</code>s. <p>
 * 
 * Enhanced <code>TableColumnModelExt</code> guarantees to notify 
 * these extended column listeners. An example of a client which 
 * adjusts itself based on <code>headerValue</code> property of visible columns: 
 * <pre><code>
 * TableColumnModelExtListener l = new TableColumnModelExtListener() {
 * 
 *     public void columnPropertyChange(PropertyChangeEvent event) {
 *         if (&quot;headerValue&quot;.equals(event.getPropertyName())) {
 *             TableColumn column = (TableColumn) event.getSource();
 *             if ((column instanceof TableColumnExt)
 *                     &amp;&amp; !((TableColumnExt) column).isVisible()) {
 *                 return;
 *             }
 *             resizeAndRepaint();
 *         }
 *     }
 * 
 *     public void columnAdded(TableColumnModelEvent e) {
 *     }
 * 
 *     public void columnMarginChanged(ChangeEvent e) {
 *     }
 * 
 *     public void columnMoved(TableColumnModelEvent e) {
 *     }
 * 
 *     public void columnRemoved(TableColumnModelEvent e) {
 *     }
 * 
 *     public void columnSelectionChanged(ListSelectionEvent e) {
 *     }
 * 
 * };
 * columnModel.addColumnModelListener(l);
 * </code></pre>
 * 
 * @author Jeanette Winzenburg
 * @see org.jdesktop.swingx.table.TableColumnModelExt
 */
public interface TableColumnModelExtListener extends TableColumnModelListener {

    /**
     * Notifies listeners about property changes of contained columns.
     * The event is the original as fired from the <code>TableColumn</code>.
     * @param event a <code>PropertyChangeEvent</code> fired by a <code>TableColumn</code>
     *   contained in a <code>TableColumnModel</code>
     */
    void columnPropertyChange(PropertyChangeEvent event);
}
