/*
 * $Id: MutableTreeTableNode.java 3927 2011-02-22 16:34:11Z kleopatra $
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

import java.util.Enumeration;

/**
 * Defines the requirements for a tree table node object that can change -- by
 * adding or removing child nodes, or by changing the contents of a user object
 * stored in the node.
 * <p>
 * Note this does not extend {@code MutableTreeNode} to minimize the contract
 * breakage, cf. {@link TreeTableNode#getIndex(javax.swing.tree.TreeNode)}.
 * 
 * @see javax.swing.tree.MutableTreeNode
 * 
 * @author Karl Schaefer
 */
public interface MutableTreeTableNode extends TreeTableNode {
    /**
     * Returns an enumeration this node's children.
     * 
     * @return an enumeration of {@code TreeTableNode}s
     */
    @Override
    Enumeration<? extends MutableTreeTableNode> children();
    
    /**
     * Adds the {@code child} to this node at the specified {@code index}. This
     * method calls {@code setParent} on {@code child} with {@code this} as the
     * parameter.
     * 
     * @param child
     *            the node to add as a child
     * @param index
     *            the index of the child
     * @throws IndexOutOfBoundsException
     *             if {@code index} is not a valid index
     */
    void insert(MutableTreeTableNode child, int index);

    /**
     * Removes the child node at the specified {@code index} from this node.
     * This method calls {@code setParent} on {@code child} with a {@code null}
     * parameter.
     * 
     * @param index
     *            the index of the child
     * @throws IndexOutOfBoundsException
     *             if {@code index} is not a valid index
     */
    void remove(int index);

    /**
     * Removes the specified child {@code node} from this node.
     * This method calls {@code setParent} on {@code child} with a {@code null}
     * parameter.
     * 
     * @param node
     *            the index of the child
     */
    void remove(MutableTreeTableNode node);

    /**
     * Removes this node from it's parent. Most implementations will use
     * {@code getParent().remove(this)}.
     * 
     * @throws NullPointerException
     *             if {@code getParent()} returns {@code null}
     */
    void removeFromParent();

    /**
     * Sets the parent of this node to {@code newParent}. This methods remove
     * the node from its old parent.
     * 
     * @param newParent
     *            the new parent for this node
     */
    void setParent(MutableTreeTableNode newParent);
}
