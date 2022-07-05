/*
 * $Id: TreeTableNode.java 3927 2011-02-22 16:34:11Z kleopatra $
 *
 * Copyright 2004 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.treetable;

import javax.swing.tree.TreeNode;
import java.util.Enumeration;

/**
 * Defines the requirements for an object that can be used as a tree node in a
 * {@code JXTreeTable}.
 * 
 * @author Karl Schaefer
 */
public interface TreeTableNode extends TreeNode {
    /**
     * Returns an enumeration this node's children.
     * 
     * @return an enumeration of {@code TreeTableNode}s
     */
    @Override
    Enumeration<? extends TreeTableNode> children();

    /**
     * Gets the value for this node that corresponds to a particular tabular
     * column.
     * 
     * @param column
     *            the column to query
     * @return the value for the queried column
     * @throws IndexOutOfBoundsException
     *             if {@code column} is not a valid column index
     */
    Object getValueAt(int column);

    /**
     * Overridden to specify the return type. Returns the child {@code TreeNode}
     * at index {@code childIndex}. Models that utilize this node should verify
     * the column count before querying this node, since nodes may return
     * differing sizes even for the same model.
     * 
     * @param childIndex
     *            the index of the child
     * @return the {@code TreeTableNode} corresponding to the specified index
     */
    @Override
    TreeTableNode getChildAt(int childIndex);

    /**
     * Returns the number of columns supported by this {@code TreeTableNode}.
     * 
     * @return the number of columns this node supports
     */
    int getColumnCount();

    /**
     * Overridden to specify the return type. Returns the parent
     * {@code TreeTableNode} of the receiver.
     * 
     * @return the parent {@code TreeTableNode} or {@code null} if this node has
     *         no parent (such nodes are usually root nodes).
     */
    @Override
    TreeTableNode getParent();

    /**
     * Determines whether the specified column is editable.
     * 
     * @param column
     *            the column to query
     * @return {@code true} if the column is editable, {@code false} otherwise
     */
    boolean isEditable(int column);

    /**
     * Sets the value for the given {@code column}.
     * 
     * @param aValue
     *            the value to set
     * @param column
     *            the column to set the value on
     */
    void setValueAt(Object aValue, int column);
    
    /**
     * Returns this node's user object.
     * 
     * @return the Object stored at this node by the user
     */
    Object getUserObject();
    
    /**
     * Sets the user object stored in this node.
     * 
     * @param userObject
     *                the object to store
     */
    void setUserObject(Object userObject);
}
