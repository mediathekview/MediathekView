/*
 * $Id: package-info.java 3933 2011-03-02 19:02:29Z kschaefe $
 *
 * Copyright 2007 Sun Microsystems, Inc., 4150 Network Circle,
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
 *
 */

/**
 * Contains extensions to the Swing GUI toolkit, including new and enhanced
 * components that provide functionality commonly required by rich,
 * data-centric client applications. Many of these features will eventually
 * be incorporated into the Swing toolkit, although API compatibility will
 * not be guaranteed.
 * <p>
 * 
 * <h2>New or Enhanced Functionality</h2>
 * 
 * <h3>Auto-completion for TextFields and ComboBoxes</h3>
 * 
 * For more information, see the
 * <a href="autocomplete/package.html">AutoComplete</a> documentation.
 * 
 * <h3>Enhanced Rendering Support for Collection Components</h3>
 * 
 * <h3>Built-In Search Support for Collection Components and JXEditorPane</h3>
 * 
 * <h3>Login/Authentication Framework</h3>
 * 
 * <h3>Painter-Enabled Components</h3>
 * 
 * Components that use painters for background rendering alter the functionality
 * of how {@link java.awt.Component#setBackground(java.awt.Color)} works.  
 * Setting the background color of a painter-enabled component effectively sets 
 * the background painter to paint the requested color.
 * <p>
 * Look and Feel implementors should note that setting a 
 * {@link java.swing.plaf.UIResource} to {@code setBackground} will cause a 
 * {@code Painter} {@code UIResource} to be installed.  This means that 
 * implementors should set the background before setting the painter as the last 
 * one set wins. 
 * 
 * <h2>New and Enhanced components</h2>
 * 
 * <h3>Buttons and Labels</h3>
 * <ul>
 * <li> {@link org.jdesktop.swingx.JXButton}
 * <li> {@link org.jdesktop.swingx.JXHyperlink Hyperlink}
 * <li> {@link org.jdesktop.swingx.JXLabel}
 * <li> {@link org.jdesktop.swingx.JXBusyLabel}
 * <li> {@link org.jdesktop.swingx.JXRadioGroup}
 * </ul>
 * 
 * 
 * <h3>Collection Components</h3>
 * 
 * These are sortable/filterable (with the exception of hierarchical
 * components) with consistent and uniform SwingX rendering, highlighting,
 * searching and rollover support.
 * <ul>
 * <li> {@link org.jdesktop.swingx.JXTable Table} uses the enhanced
 * {@link org.jdesktop.swingx.JXTableHeader TableHeader}
 * <li> {@link org.jdesktop.swingx.JXList List} - rollover and sort/filter
 * functionality is disabled by default
 * <li> {@link org.jdesktop.swingx.JXTree Tree}
 * <li> {@link org.jdesktop.swingx.JXTreeTable TreeTable} - a new
 * hierarchical component with support of tabular node properties
 * </ul>
 * 
 * <h3>Top-level Windows, General and Special Purpose Containers</h3>
 * <ul>
 * <li>Enhanced {@link org.jdesktop.swingx.JXFrame Frame} using an extended
 * {@link org.jdesktop.swingx.JXRootPane RootPane RootPane} to support a
 * {@link org.jdesktop.swingx.JXStatusBar StatusBar}
 * <li> {@link org.jdesktop.swingx.JXDialog Dialog}
 * <li> {@link org.jdesktop.swingx.JXPanel Panel}
 * <li> {@link org.jdesktop.swingx.JXErrorPane ErrorPane}
 * <li> {@link org.jdesktop.swingx.JXLoginPane LoginPane}
 * 
 * <li>Search components: {@link org.jdesktop.swingx.JXFindBar FindBar} used
 * for incremental search (similar to FireFox),
 * {@link org.jdesktop.swingx.JXFindPanel FindPanel} used in a find dialog,
 * and {@link org.jdesktop.swingx.JXSearchPanel SearchPanel} used for what
 * was it?
 * <li>Nested SplitPane {@link org.jdesktop.swingx.JXMultiSplitPane
 * MultiSplitPane}
 * <li>Vertical collapsing/expansion functionality is provided by a
 * {@link org.jdesktop.swingx.JXCollapsiblePane CollapsiblePane}. A special
 * purpose collapsible is the {@link org.jdesktop.swingx.JXTaskPane
 * TaskPane} which typically is used to group buttons/hyperlinks which
 * perform related tasks. A special
 * {@link org.jdesktop.swingx.JXTaskPaneContainer TaskPaneContainer} is
 * responsible for the layout of several TaskPanes.
 * <li>Easily configurable {@link org.jdesktop.swingx.JXTipOfTheDay
 * TipOfTheDay}
 * <li> {@link org.jdesktop.swingx.JXTitledPanel TitledPanel}
 * 
 * </ul>
 * 
 * <h3>Miscellaneous Components</h3>
 * 
 * <ul>
 * <li>New calendar components: the {@link org.jdesktop.swingx.JXDatePicker
 * DatePicker} allows to select a single Date and a
 * {@link org.jdesktop.swingx.JXMonthView MonthView} showing the overview of
 * one or more months.
 * 
 * <li> {@link org.jdesktop.swingx.JXHeader Header}
 * <li> {@link org.jdesktop.swingx.JXTitledSeparator TitledSeparator}
 * 
 * <li> {@link org.jdesktop.swingx.JXColorSelectionButton}
 * <li> {@link org.jdesktop.swingx.JXEditorPane}
 * <li> {@link org.jdesktop.swingx.JXGradientChooser}
 * <li> {@link org.jdesktop.swingx.JXGraph}
 * <li>Image containers {@link org.jdesktop.swingx.JXImageView ImageView}
 * and {@link org.jdesktop.swingx.JXImagePanel ImagePanel} (PENDING JW:
 * merge/remove one?)
 * <li> {@link org.jdesktop.swingx.JXMultiThumbSlider MultiThumbSlider}
 * 
 * </ul>
 * 
 * <h2>External Information Sources</h2>
 * 
 * <a href="http://wiki.java.net/bin/view/Javadesktop/SwingX">SwingX Twiki</a>
 * <a href="http://wiki.java.net/bin/view/Javadesktop/SwingXChanges">Change History</a>
 * <a href="http://forums.java.net/jive/forum.jspa?forumID=73">SwingLabs User and 
 * Developer Discussion Forum</a>
 */
package org.jdesktop.swingx;

