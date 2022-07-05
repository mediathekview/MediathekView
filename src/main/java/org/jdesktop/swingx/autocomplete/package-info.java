/*
 * $Id: package-info.java 4045 2011-07-19 18:39:17Z kschaefe $
 *
 * Copyright 2009 Sun Microsystems, Inc., 4150 Network Circle,
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
 * Contains classes to enable automatic completion for JComboBox and other
 * components.
 * <p>
 * The automatic completion feature allows the user to enter a few characters
 * using the keyboard - meanwhile, the computer &quot;guesses&quot; what the
 * user intents to enter. Take a look at the example below to get an idea of the
 * resulting user experience. Suppose the user types 'J','O','R' and 'G'...
 * </p>
 * <p>
 * <img src="doc-files/example.gif"/>
 * </p>
 * <p>
 * The easiest way to get automatic completion for a component is to use the
 * {@link org.jdesktop.swingx.autocomplete.AutoCompleteDecorator AutoCompleteDecorator}.
 * </p>
 * <p>Enabling automatic completion for e.g. a JComboBox is only one line of
 * code:</p>
 * <p><code>
 * <span class="keyword">import</span> org.jdesktop.swingx.autocomplete.AutoCompleteDecorator;<br/>
 * [...]<br/>
 * JComboBox comboBox = [...];<br/>
 * AutoCompleteDecorator.<b>decorate</b>(comboBox);
 * </code></p>
 * <p>When the combo box is not editable when calling
 * {@link org.jdesktop.swingx.autocomplete.AutoCompleteDecorator#decorate(JComboBox) decorate},
 * the automatic completion will be strict (only allowing items contained in
 * the combo box). When the combo box is editable it will also be possible to
 * enter items that are not contained in the combo box.
 * </p>
 * <p>Take care when enabling automatic completion for a JComboBox that is used
 * as the cell editor for a JTable. You need to use the special
 * {@link org.jdesktop.swingx.autocomplete.ComboBoxCellEditor ComboBoxCellEditor}
 * instead of the standard DefaultCellEditor:
 * </p>
 * <p><code>
 * JTable table = [...];<br/>
 * JComboBox comboBox = [...];<br/>
 * [...]<br/>
 * TableColumn column = table.<b>getColumnModel()</b>.<b>getColumn</b>([...]);<br/>
 * column.<b>setCellEditor</b>(<span class="keyword">new</span> <b>ComboBoxCellEditor</b>(comboBox));
 * </code></p>
 * <p>
 * If you want to enable automatic completion for a component that is not 
 * supported by the {@link org.jdesktop.swingx.autocomplete.AutoCompleteDecorator AutoCompleteDecorator}, you
 * need to implement {@link org.jdesktop.swingx.autocomplete.AbstractAutoCompleteAdaptor AbstractAutoCompleteAdaptor}. For
 * an example see {@link org.jdesktop.swingx.autocomplete.ComboBoxAdaptor ComboBoxAdaptor}
 * and {@link org.jdesktop.swingx.autocomplete.ListAdaptor ListAdaptor}.
 * </p>
 * <p>
 * The automatic completion works only for subclasses of
 * {@link javax.swing.text.JTextComponent JTextComponent}. So you either use a component
 * that contains a JTextComponent (e.g. JComboBox) or you connect a
 * JTextComponent with another component (e.g. a JTextField and a JList).
 * Of course, it's also possible to enable automatic completion for a
 * JTextComponent without another visual component.
 * </p>
 * <p>Once you have a custom implementation of
 * {@link org.jdesktop.swingx.autocomplete.AbstractAutoCompleteAdaptor AbstractAutoCompleteAdaptor},
 * you normally would only have to make three more calls:
 * </p>
 * <p>
 * <code>
 * AbstractAutoCompleteAdaptor adaptor = <span class="keyword">new</span> <b>YourAdaptor</b>([...]);<br/>
 * AutoCompleteDocument document = <span class="keyword">new</span> <b>AutoCompleteDocument</b>(adaptor, true); <span class="comment">// or false if you need non-strict matching</span><br/>
 * AutoCompleteDecorator.<b>decorate</b>(yourTextComponent, document, adaptor);
 * </code>
 * </p>
 */
package org.jdesktop.swingx.autocomplete;
