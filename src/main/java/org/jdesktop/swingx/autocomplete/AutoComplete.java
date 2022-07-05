/*
 * $Id: AutoComplete.java 4051 2011-07-19 20:17:05Z kschaefe $
 *
 * Copyright 2010 Sun Microsystems, Inc., 4150 Network Circle,
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

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;

import static org.jdesktop.swingx.autocomplete.AutoCompleteDecorator.*;

/**
 * 
 * @author kschaefer
 */
final class AutoComplete {
    static class InputMap extends javax.swing.InputMap {
        private static final long serialVersionUID = 1L;
    }

    static class FocusAdapter extends java.awt.event.FocusAdapter {
        private AbstractAutoCompleteAdaptor adaptor;

        public FocusAdapter(AbstractAutoCompleteAdaptor adaptor) {
            this.adaptor = adaptor;
        }

        @Override
        public void focusGained(FocusEvent e) {
            adaptor.markEntireText();
        }
    }
    
    static class KeyAdapter extends java.awt.event.KeyAdapter {
        private JComboBox comboBox;
        
        public KeyAdapter(JComboBox comboBox) {
            this.comboBox = comboBox;
        }
        
        @Override
        public void keyPressed(KeyEvent keyEvent) {
            // don't popup on action keys (cursor movements, etc...)
            if (keyEvent.isActionKey()) {
                return;
            }
            
            // don't popup if the combobox isn't visible or empty anyway
            if (comboBox.isDisplayable() && !comboBox.isPopupVisible() && comboBox.getModel().getSize() != 0) {
                int keyCode = keyEvent.getKeyCode();
                // don't popup when the user hits shift,ctrl or alt
                if (keyCode==KeyEvent.VK_SHIFT || keyCode==KeyEvent.VK_CONTROL || keyCode==KeyEvent.VK_ALT) return;
                // don't popup when the user hits escape (see issue #311)
                if (keyCode==KeyEvent.VK_ENTER || keyCode==KeyEvent.VK_ESCAPE) return;
                comboBox.setPopupVisible(true);
            }
        }
    }

    static class PropertyChangeListener implements java.beans.PropertyChangeListener {
        private JComboBox comboBox;
        
        public PropertyChangeListener(JComboBox comboBox) {
            this.comboBox = comboBox;
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        @SuppressWarnings("nls")
        public void propertyChange(PropertyChangeEvent evt) {
            if ("editor".equals(evt.getPropertyName())) {
                handleEditor(evt);
            } else if ("enabled".equals(evt.getPropertyName())) {
                handleEnabled(evt);
            }
        }
        
        private void handleEnabled(PropertyChangeEvent evt) {
            if (Boolean.TRUE.equals(evt.getNewValue())) {
                comboBox.setEditable(true);
            } else {
                JTextComponent textComponent = (JTextComponent) comboBox.getEditor().getEditorComponent();
                boolean strictMatching = ((AutoCompleteDocument) textComponent.getDocument()).strictMatching;
                
                comboBox.setEditable(!strictMatching);
            }
        }

        private void handleEditor(PropertyChangeEvent evt) {
            if (evt.getNewValue() instanceof AutoCompleteComboBoxEditor) {
                return;
            }
            
            AutoCompleteComboBoxEditor acEditor = (AutoCompleteComboBoxEditor) evt.getOldValue();
            boolean strictMatching = false;
            
            if (acEditor.getEditorComponent() != null) {
                JTextComponent textComponent = (JTextComponent) acEditor.getEditorComponent();
                strictMatching = ((AutoCompleteDocument) textComponent.getDocument()).strictMatching;
                
                undecorate(textComponent);
                
                for (KeyListener l : textComponent.getKeyListeners()) {
                    if (l instanceof KeyAdapter) {
                        textComponent.removeKeyListener(l);
                        break;
                    }
                }
            }

            JTextComponent editorComponent = (JTextComponent) comboBox.getEditor().getEditorComponent();
            AbstractAutoCompleteAdaptor adaptor = new ComboBoxAdaptor(comboBox);
            AutoCompleteDocument document = createAutoCompleteDocument(adaptor, strictMatching,
                    acEditor.stringConverter, editorComponent.getDocument());
            decorate(editorComponent, document, adaptor);
            
            editorComponent.addKeyListener(new KeyAdapter(comboBox));
            
            //set before adding the listener for the editor
            comboBox.setEditor(new AutoCompleteComboBoxEditor(comboBox.getEditor(), document.stringConverter));
        }
    }

    static class SelectionAction implements Action {
        private Action delegate;
        
        public SelectionAction(Action delegate) {
            this.delegate = delegate;
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        public void actionPerformed(ActionEvent e) {
            JComboBox comboBox = (JComboBox) e.getSource();
            JTextComponent textComponent = (JTextComponent) comboBox.getEditor().getEditorComponent();
            AutoCompleteDocument doc = (AutoCompleteDocument) textComponent.getDocument();
            
            // doing this prevents the updating of the selected item to "" during the remove prior
            // to the insert in JTextComponent.setText
            doc.strictMatching = true;
            try {
                delegate.actionPerformed(e);
            } finally {
                doc.strictMatching = false;
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void addPropertyChangeListener(java.beans.PropertyChangeListener listener) {
            delegate.addPropertyChangeListener(listener);
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        public void removePropertyChangeListener(java.beans.PropertyChangeListener listener) {
            delegate.removePropertyChangeListener(listener);
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        public Object getValue(String key) {
            return delegate.getValue(key);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void putValue(String key, Object value) {
            delegate.putValue(key, value);
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isEnabled() {
            return delegate.isEnabled();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setEnabled(boolean b) {
            delegate.setEnabled(b);
        }
    }
    
    private AutoComplete() {
        // prevent instantiation
    }
}
