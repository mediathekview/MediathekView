/*
 * $Id: JXFindBar.java 4147 2012-02-01 17:13:24Z kschaefe $
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
package org.jdesktop.swingx;

import org.jdesktop.beans.JavaBean;
import org.jdesktop.swingx.search.Searchable;

import javax.swing.*;
import java.awt.*;

/**
 * A simple low-intrusion default widget for incremental search.
 * 
 * Actions registered (in addition to super):
 * <ul>
 * <li> {@link JXDialog#CLOSE_ACTION_COMMAND} - an action bound to this
 * component's cancel method. The method itself is an empty implementation:
 * Subclassing clients can override the method, all clients can register a
 * custom action.
 * </ul>
 * 
 * Key bindings:
 * <ul>
 * <li> ESCAPE - calls action registered for
 * {@link JXDialog#CLOSE_ACTION_COMMAND}
 * </ul>
 * 
 * This implementation uses textfield coloring as not-found visualization.
 * 
 * <p>
 * PENDING: the coloring needs to be read from the UIManager instead of
 * hardcoding.
 * 
 * <p>
 * PENDING: the state transition of found/non-found coloring needs clean-up -
 * there are spurious problems when re-using the same instance (as SearchFactory
 * does).
 * 
 * @author Jeanette Winzenburg
 * 
 */
@JavaBean
public class JXFindBar extends JXFindPanel {

    protected Color previousBackgroundColor;

    protected Color previousForegroundColor;

    // PENDING: need to read from UIManager
    protected Color notFoundBackgroundColor = Color.decode("#FF6666");

    protected Color notFoundForegroundColor = Color.white;

    protected JButton findNext;

    protected JButton findPrevious;

    public JXFindBar() {
        this(null);
    }

    public JXFindBar(Searchable searchable) {
        super(searchable);
        getPatternModel().setIncremental(true);
        getPatternModel().setWrapping(true);
    }

    @Override
    public void setSearchable(Searchable searchable) {
        super.setSearchable(searchable);
        match();
    }

    /**
     * here: set textfield colors to not-found colors.
     */
    @Override
    protected void showNotFoundMessage() {
        //JW: quick hack around #487-swingx - NPE in setSearchable
        if (searchField ==  null) return;
        searchField.setForeground(notFoundForegroundColor);
        searchField.setBackground(notFoundBackgroundColor);
    }

    /**
     * here: set textfield colors to normal.
     */
    @Override
    protected void showFoundMessage() {
        //JW: quick hack around #487-swingx - NPE in setSearchable
        if (searchField ==  null) return;
        searchField.setBackground(previousBackgroundColor);
        searchField.setForeground(previousForegroundColor);
    }

    @Override
    public void addNotify() {
        super.addNotify();
        if (previousBackgroundColor == null) {
            previousBackgroundColor = searchField.getBackground();
            previousForegroundColor = searchField.getForeground();
        } else {
            searchField.setBackground(previousBackgroundColor);
            searchField.setForeground(previousForegroundColor);
        }
    }

    // --------------------------- action call back
    /**
     * Action callback method for bound action JXDialog.CLOSE_ACTION_COMMAND.
     * 
     * Here: does nothing. Subclasses can override to define custom "closing"
     * behaviour. Alternatively, any client can register a custom action with
     * the actionMap.
     * 
     * 
     */
    public void cancel() {
    }

    // -------------------- init

    @Override
    protected void initExecutables() {
        getActionMap().put(JXDialog.CLOSE_ACTION_COMMAND,
                createBoundAction(JXDialog.CLOSE_ACTION_COMMAND, "cancel"));
        super.initExecutables();
    }

    @Override
    protected void bind() {
        super.bind();
        searchField
                .addActionListener(getAction(JXDialog.EXECUTE_ACTION_COMMAND));
        findNext.setAction(getAction(FIND_NEXT_ACTION_COMMAND));
        findPrevious.setAction(getAction(FIND_PREVIOUS_ACTION_COMMAND));
        KeyStroke stroke = KeyStroke.getKeyStroke("ESCAPE");
        getInputMap(WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(stroke,
                JXDialog.CLOSE_ACTION_COMMAND);
    }

    @Override
    protected void build() {
        setLayout(new FlowLayout(SwingConstants.LEADING));
        add(searchLabel);
        add(new JLabel(":"));
        add(new JLabel("  "));
        add(searchField);
        add(findNext);
        add(findPrevious);
    }

    @Override
    protected void initComponents() {
        super.initComponents();
        findNext = new JButton();
        findPrevious = new JButton();
    }

}
