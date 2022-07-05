/*
 * $Id: AutoCompleteDecorator.java 4051 2011-07-19 20:17:05Z kschaefe $
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
package org.jdesktop.swingx.autocomplete;

import org.jdesktop.swingx.autocomplete.workarounds.MacOSXPopupLocationFix;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.plaf.UIResource;
import javax.swing.text.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeListener;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

/**
 * This class contains only static utility methods that can be used to set up
 * automatic completion for some Swing components.
 * <p>Usage examples:</p>
 * <p><pre><code>
 * JComboBox comboBox = [...];
 * AutoCompleteDecorator.<b>decorate</b>(comboBox);
 * 
 * List items = [...];
 * JTextField textField = [...];
 * AutoCompleteDecorator.<b>decorate</b>(textField, items);
 * 
 * JList list = [...];
 * JTextField textField = [...];
 * AutoCompleteDecorator.<b>decorate</b>(list, textField);
 * </code></pre></p>
 *
 * @author Thomas Bierhance
 * @author Karl Schaefer
 */
@SuppressWarnings({"nls", "serial"})
public class AutoCompleteDecorator {
    //these keys were pulled from BasicComboBoxUI from Sun JDK 1.6.0_20
    private static final List<String> COMBO_BOX_ACTIONS = unmodifiableList(asList("selectNext",
            "selectNext2", "selectPrevious", "selectPrevious2", "pageDownPassThrough",
            "pageUpPassThrough", "homePassThrough", "endPassThrough"));
    /**
     * A TextAction that provides an error feedback for the text component that invoked
     * the action. The error feedback is most likely a "beep".
     */
    private static final Object errorFeedbackAction = new TextAction("provide-error-feedback") {
        @Override
        public void actionPerformed(ActionEvent e) {
            UIManager.getLookAndFeel().provideErrorFeedback(getTextComponent(e));
        }
    };
    
    private AutoCompleteDecorator() {
        //prevents instantiation
    }
    
    private static void installMap(InputMap componentMap, boolean strict) {
        InputMap map = new AutoComplete.InputMap();
        
        if (strict) {
            map.put(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_BACK_SPACE, 0), DefaultEditorKit.selectionBackwardAction);
            // ignore VK_DELETE and CTRL+VK_X and beep instead when strict matching
            map.put(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0), errorFeedbackAction);
            map.put(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, java.awt.event.InputEvent.CTRL_DOWN_MASK), errorFeedbackAction);
        } else {
            // VK_BACKSPACE will move the selection to the left if the selected item is in the list
            // it will delete the previous character otherwise
            map.put(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_BACK_SPACE, 0), "nonstrict-backspace");
            // leave VK_DELETE and CTRL+VK_X as is
        }
        
        map.setParent(componentMap.getParent());
        componentMap.setParent(map);
    }
    
    static AutoCompleteDocument createAutoCompleteDocument(
            AbstractAutoCompleteAdaptor adaptor, boolean strictMatching,
            ObjectToStringConverter stringConverter, Document delegate) {
        if (delegate instanceof StyledDocument) {
            return new AutoCompleteStyledDocument(adaptor, strictMatching,
                    stringConverter, (StyledDocument) delegate);
        }
        
        return new AutoCompleteDocument(adaptor, strictMatching,
                stringConverter, delegate);
    }
    
    /**
     * Enables automatic completion for the given JComboBox. The automatic
     * completion will be strict (only items from the combo box can be selected)
     * if the combo box is not editable.
     * @param comboBox a combo box
     * @see #decorate(JComboBox, ObjectToStringConverter)
     */
    public static void decorate(JComboBox comboBox) {
        decorate(comboBox, null);
    }
    
    /**
     * Enables automatic completion for the given JComboBox. The automatic
     * completion will be strict (only items from the combo box can be selected)
     * if the combo box is not editable.
     * <p>
     * <b>Note:</b> the {@code AutoCompleteDecorator} will alter the state of
     * the {@code JComboBox} to be editable. This can cause side effects with
     * layouts and sizing. {@code JComboBox} caches the size, which differs
     * depending on the component's editability. Therefore, if the component's
     * size is accessed prior to being decorated and then the cached size is
     * forced to be recalculated, the size of the component will change.
     * <p>
     * Because the size of the component can be altered (recalculated), the
     * decorator does not attempt to set any sizes on the supplied
     * {@code JComboBox}. Users that need to ensure sizes of supplied combos
     * should take measures to set the size of the combo.
     * 
     * @param comboBox
     *                a combo box
     * @param stringConverter
     *                the converter used to transform items to strings
     */
    public static void decorate(JComboBox comboBox, ObjectToStringConverter stringConverter) {
        undecorate(comboBox);
        
        boolean strictMatching = !comboBox.isEditable();
        // has to be editable
        comboBox.setEditable(true);
        // fix the popup location
        MacOSXPopupLocationFix.install(comboBox);

        // configure the text component=editor component
        JTextComponent editorComponent = (JTextComponent) comboBox.getEditor().getEditorComponent();
        final AbstractAutoCompleteAdaptor adaptor = new ComboBoxAdaptor(comboBox);
        final AutoCompleteDocument document = createAutoCompleteDocument(adaptor, strictMatching,
                stringConverter, editorComponent.getDocument());
        decorate(editorComponent, document, adaptor);
        
        editorComponent.addKeyListener(new AutoComplete.KeyAdapter(comboBox));
        
        //set before adding the listener for the editor
        comboBox.setEditor(new AutoCompleteComboBoxEditor(comboBox.getEditor(), document.stringConverter));
        
        // Changing the l&f can change the combobox' editor which in turn
        // would not be autocompletion-enabled. The new editor needs to be set-up.
        AutoComplete.PropertyChangeListener pcl = new AutoComplete.PropertyChangeListener(comboBox);
        comboBox.addPropertyChangeListener("editor", pcl);
        comboBox.addPropertyChangeListener("enabled", pcl);
        
        if (!strictMatching) {
            ActionMap map = comboBox.getActionMap();
            
            for (String key : COMBO_BOX_ACTIONS) {
                Action a = map.get(key);
                map.put(key, new AutoComplete.SelectionAction(a));
            }
        }
    }

    static void undecorate(JComboBox comboBox) {
        JTextComponent editorComponent = (JTextComponent) comboBox.getEditor().getEditorComponent();
        
        if (editorComponent.getDocument() instanceof AutoCompleteDocument) {
            AutoCompleteDocument doc = (AutoCompleteDocument) editorComponent.getDocument();
            
            if (doc.strictMatching) {
                ActionMap map = comboBox.getActionMap();
                
                for (String key : COMBO_BOX_ACTIONS) {
                    map.put(key, null);
                }
            }
            
            //remove old property change listener
            for (PropertyChangeListener l : comboBox.getPropertyChangeListeners("editor")) {
                if (l instanceof AutoComplete.PropertyChangeListener) {
                    comboBox.removePropertyChangeListener("editor", l);
                }
            }
            
            for (PropertyChangeListener l : comboBox.getPropertyChangeListeners("enabled")) {
                if (l instanceof AutoComplete.PropertyChangeListener) {
                    comboBox.removePropertyChangeListener("enabled", l);
                }
            }
            
            AutoCompleteComboBoxEditor editor = (AutoCompleteComboBoxEditor) comboBox.getEditor();
            comboBox.setEditor(editor.wrapped);
            
            //remove old key listener
            for (KeyListener l : editorComponent.getKeyListeners()) {
                if (l instanceof AutoComplete.KeyAdapter) {
                    editorComponent.removeKeyListener(l);
                    break;
                }
            }
            
            undecorate(editorComponent);
            
            for (ActionListener l : comboBox.getActionListeners()) {
                if (l instanceof ComboBoxAdaptor) {
                    comboBox.removeActionListener(l);
                    break;
                }
            }
            
            //TODO remove aqua fix
            
            //TODO reset editibility
        }
    }
    
    /**
     * Enables automatic completion for the given JTextComponent based on the
     * items contained in the given JList. The two components will be
     * synchronized. The automatic completion will always be strict.
     * @param list a <tt>JList</tt> containing the items for automatic completion
     * @param textComponent the text component that will be enabled for automatic
     * completion
     */
    public static void decorate(JList list, JTextComponent textComponent) {
        decorate(list, textComponent, null);
    }
    
    /**
     * Enables automatic completion for the given JTextComponent based on the
     * items contained in the given JList. The two components will be
     * synchronized. The automatic completion will always be strict.
     * @param list a <tt>JList</tt> containing the items for automatic completion
     * @param textComponent the text component that will be used for automatic
     * completion
     * @param stringConverter the converter used to transform items to strings
     */
    public static void decorate(JList list, JTextComponent textComponent, ObjectToStringConverter stringConverter) {
        undecorate(list);
        
        AbstractAutoCompleteAdaptor adaptor = new ListAdaptor(list, textComponent, stringConverter);
        AutoCompleteDocument document = createAutoCompleteDocument(adaptor, true, stringConverter, textComponent.getDocument());
        decorate(textComponent, document, adaptor);
    }

    static void undecorate(JList list) {
        for (ListSelectionListener l : list.getListSelectionListeners()) {
            if (l instanceof ListAdaptor) {
                list.removeListSelectionListener(l);
                break;
            }
        }
    }
    
    /**
     * Enables automatic completion for the given JTextComponent based on the
     * items contained in the given <tt>List</tt>.
     * @param textComponent the text component that will be used for automatic
     * completion.
     * @param items contains the items that are used for autocompletion
     * @param strictMatching <tt>true</tt>, if only given items should be allowed to be entered
     */
    public static void decorate(JTextComponent textComponent, List<?> items, boolean strictMatching) {
        decorate(textComponent, items, strictMatching, null);
    }
    
    /**
     * Enables automatic completion for the given JTextComponent based on the
     * items contained in the given <tt>List</tt>.
     * @param items contains the items that are used for autocompletion
     * @param textComponent the text component that will be used for automatic
     * completion.
     * @param strictMatching <tt>true</tt>, if only given items should be allowed to be entered
     * @param stringConverter the converter used to transform items to strings
     */
    public static void decorate(JTextComponent textComponent, List<?> items, boolean strictMatching, ObjectToStringConverter stringConverter) {
        AbstractAutoCompleteAdaptor adaptor = new TextComponentAdaptor(textComponent, items);
        AutoCompleteDocument document = createAutoCompleteDocument(adaptor, strictMatching, stringConverter, textComponent.getDocument());
        decorate(textComponent, document, adaptor);
    }
    
    /**
     * Decorates a given text component for automatic completion using the
     * given AutoCompleteDocument and AbstractAutoCompleteAdaptor.
     * 
     * @param textComponent a text component that should be decorated
     * @param document the AutoCompleteDocument to be installed on the text component
     * @param adaptor the AbstractAutoCompleteAdaptor to be used
     */
    public static void decorate(JTextComponent textComponent, AutoCompleteDocument document, AbstractAutoCompleteAdaptor adaptor) {
        undecorate(textComponent);
        
        // install the document on the text component
        textComponent.setDocument(document);
        
        // mark entire text when the text component gains focus
        // otherwise the last mark would have been retained which is quiet confusing
        textComponent.addFocusListener(new AutoComplete.FocusAdapter(adaptor));
        
        // Tweak some key bindings
        InputMap editorInputMap = textComponent.getInputMap();
        
        while (editorInputMap != null) {
            InputMap parent = editorInputMap.getParent();
            
            if (parent instanceof UIResource) {
                installMap(editorInputMap, document.isStrictMatching());
                break;
            }
            
            editorInputMap = parent;
        }
        
        ActionMap editorActionMap = textComponent.getActionMap();
        editorActionMap.put("nonstrict-backspace", new NonStrictBackspaceAction(
                editorActionMap.get(DefaultEditorKit.deletePrevCharAction),
                editorActionMap.get(DefaultEditorKit.selectionBackwardAction),
                adaptor));
    }
    
    static void undecorate(JTextComponent textComponent) {
        Document doc = textComponent.getDocument();
        
        if (doc instanceof AutoCompleteDocument) {
            //remove autocomplete key/action mappings
            InputMap map = textComponent.getInputMap();
            
            while (map.getParent() != null) {
                InputMap parent = map.getParent();
                
                if (parent instanceof AutoComplete.InputMap) {
                    map.setParent(parent.getParent());
                }
                
                map = parent;
            }
            
            textComponent.getActionMap().put("nonstrict-backspace", null);
            
            //remove old focus listener
            for (FocusListener l : textComponent.getFocusListeners()) {
                if (l instanceof AutoComplete.FocusAdapter) {
                    textComponent.removeFocusListener(l);
                    break;
                }
            }
            
            //reset to original document
            textComponent.setDocument(((AutoCompleteDocument) doc).delegate);
        }
    }
    
    static class NonStrictBackspaceAction extends TextAction {
        Action backspace;
        Action selectionBackward;
        AbstractAutoCompleteAdaptor adaptor;
        
        public NonStrictBackspaceAction(Action backspace, Action selectionBackward, AbstractAutoCompleteAdaptor adaptor) {
            super("nonstrict-backspace");
            this.backspace = backspace;
            this.selectionBackward = selectionBackward;
            this.adaptor = adaptor;
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            if (adaptor.listContainsSelectedItem()) {
                selectionBackward.actionPerformed(e);
            } else {
                backspace.actionPerformed(e);
            }
        }
    }
}
