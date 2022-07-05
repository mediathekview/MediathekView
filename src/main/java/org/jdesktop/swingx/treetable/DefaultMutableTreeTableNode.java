/*
 * $Id: DefaultMutableTreeTableNode.java 3780 2010-09-09 16:17:41Z kschaefe $
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

/**
 * A default implementation of an {@code AbstractMutableTreeTableNode} that
 * returns {@code getUserObject().toString()} for all value queries. This
 * implementation is designed mainly for testing. It is NOT recommended to use
 * this implementation. Any user that needs to create {@code TreeTableNode}s
 * should consider directly extending {@code AbstractMutableTreeTableNode} or
 * directly implementing the interface.
 * 
 * @author Karl Schaefer
 */
public class DefaultMutableTreeTableNode extends AbstractMutableTreeTableNode {
    
    /**
     * 
     */
    public DefaultMutableTreeTableNode() {
        super();
    }

    /**
     * @param userObject
     */
    public DefaultMutableTreeTableNode(Object userObject) {
        super(userObject);
    }

    /**
     * @param userObject
     * @param allowsChildren
     */
    public DefaultMutableTreeTableNode(Object userObject, boolean allowsChildren) {
        super(userObject, allowsChildren);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getValueAt(int column) {
        return getUserObject();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEditable(int column) {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setValueAt(Object aValue, int column) {
        setUserObject(aValue);
    }
}
