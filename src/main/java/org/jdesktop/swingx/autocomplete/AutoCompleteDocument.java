/*
 * $Id: AutoCompleteDocument.java 4051 2011-07-19 20:17:05Z kschaefe $
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

import org.jdesktop.swingx.util.Contract;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.Comparator;

import static org.jdesktop.swingx.autocomplete.ObjectToStringConverter.DEFAULT_IMPLEMENTATION;

/**
 * A document that can be plugged into any JTextComponent to enable automatic completion.
 * It finds and selects matching items using any implementation of the AbstractAutoCompleteAdaptor.
 */
@SuppressWarnings("nls")
public class AutoCompleteDocument implements Document {
    private class Handler implements DocumentListener, UndoableEditListener {
        private final EventListenerList listenerList = new EventListenerList();

        public void addDocumentListener(DocumentListener listener) {
            listenerList.add(DocumentListener.class, listener);
        }

        public void addUndoableEditListener(UndoableEditListener listener) {
            listenerList.add(UndoableEditListener.class, listener);
        }

        /**
         * {@inheritDoc}
         */
        public void removeDocumentListener(DocumentListener listener) {
            listenerList.remove(DocumentListener.class, listener);
        }

        /**
         * {@inheritDoc}
         */
        public void removeUndoableEditListener(UndoableEditListener listener) {
            listenerList.remove(UndoableEditListener.class, listener);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void changedUpdate(DocumentEvent e) {
            e = new DelegatingDocumentEvent(AutoCompleteDocument.this, e);

            // Guaranteed to return a non-null array
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length-2; i>=0; i-=2) {
                if (listeners[i]==DocumentListener.class) {
                    // Lazily create the event:
                    // if (e == null)
                    // e = new ListSelectionEvent(this, firstIndex, lastIndex);
                    ((DocumentListener)listeners[i+1]).changedUpdate(e);
                }
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void insertUpdate(DocumentEvent e) {
            e = new DelegatingDocumentEvent(AutoCompleteDocument.this, e);

            // Guaranteed to return a non-null array
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length-2; i>=0; i-=2) {
                if (listeners[i]==DocumentListener.class) {
                    // Lazily create the event:
                    // if (e == null)
                    // e = new ListSelectionEvent(this, firstIndex, lastIndex);
                    ((DocumentListener)listeners[i+1]).insertUpdate(e);
                }
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void removeUpdate(DocumentEvent e) {
            e = new DelegatingDocumentEvent(AutoCompleteDocument.this, e);

            // Guaranteed to return a non-null array
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length-2; i>=0; i-=2) {
                if (listeners[i]==DocumentListener.class) {
                    // Lazily create the event:
                    // if (e == null)
                    // e = new ListSelectionEvent(this, firstIndex, lastIndex);
                    ((DocumentListener)listeners[i+1]).removeUpdate(e);
                }
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void undoableEditHappened(UndoableEditEvent e) {
            e = new UndoableEditEvent(AutoCompleteDocument.this, e.getEdit());

            // Guaranteed to return a non-null array
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length-2; i>=0; i-=2) {
                if (listeners[i]==UndoableEditListener.class) {
                // Lazily create the event:
                // if (e == null)
                // e = new ListSelectionEvent(this, firstIndex, lastIndex);
                ((UndoableEditListener)listeners[i+1]).undoableEditHappened(e);
                }
            }
        }
    }

    /**
     * true, if only items from the adaptors's list can be entered
     * false, otherwise (selected item might not be in the adaptors's list)
     */
    protected boolean strictMatching;

    protected final Document delegate;

    /** Flag to indicate if adaptor.setSelectedItem has been called.
     * Subsequent calls to remove/insertString should be ignored
     * as they are likely have been caused by the adapted Component that
     * is trying to set the text for the selected component.*/
    boolean selecting = false;

    /**
     * The adaptor that is used to find and select items.
     */
    AbstractAutoCompleteAdaptor adaptor;

    ObjectToStringConverter stringConverter;

    private final Handler handler;

    // Note: these comparators do not impose any ordering - e.g. they do not ensure that sgn(compare(x, y)) == -sgn(compare(y, x))
    private static final Comparator<String> EQUALS_IGNORE_CASE = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            return o1.equalsIgnoreCase(o2) ? 0 : -1;
        }
    };

    private static final Comparator<String> STARTS_WITH_IGNORE_CASE = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            if (o1.length() < o2.length()) return -1;
            return o1.regionMatches(true, 0, o2, 0, o2.length()) ? 0 : -1;
        }
    };

    private static final Comparator<String> EQUALS = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            return o1.equals(o2) ? 0 : -1;
        }
    };

    private static final Comparator<String> STARTS_WITH = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            return o1.startsWith(o2) ? 0 : -1;
        }
    };

    /**
     * Creates a new AutoCompleteDocument for the given AbstractAutoCompleteAdaptor.
     * @param adaptor The adaptor that will be used to find and select matching
     * items.
     * @param strictMatching true, if only items from the adaptor's list should
     * be allowed to be entered
     * @param stringConverter the converter used to transform items to strings
     * @param delegate the {@code Document} delegate backing this document
     */
    public AutoCompleteDocument(AbstractAutoCompleteAdaptor adaptor, boolean strictMatching,
            ObjectToStringConverter stringConverter, Document delegate) {
        this.adaptor = Contract.asNotNull(adaptor, "adaptor cannot be null");
        this.strictMatching = strictMatching;
        this.stringConverter = stringConverter == null ? DEFAULT_IMPLEMENTATION : stringConverter;
        this.delegate = delegate == null ? createDefaultDocument() : delegate;

        handler = new Handler();
        this.delegate.addDocumentListener(handler);

        // Handle initially selected object
        Object selected = adaptor.getSelectedItem();
        if (selected != null) {
            String itemAsString = this.stringConverter.getPreferredStringForItem(selected);
            setText(itemAsString);
            adaptor.setSelectedItemAsString(itemAsString);
        }
        this.adaptor.markEntireText();
    }


    /**
     * Creates a new AutoCompleteDocument for the given AbstractAutoCompleteAdaptor.
     * @param adaptor The adaptor that will be used to find and select matching
     * items.
     * @param strictMatching true, if only items from the adaptor's list should
     * be allowed to be entered
     * @param stringConverter the converter used to transform items to strings
     */
    public AutoCompleteDocument(AbstractAutoCompleteAdaptor adaptor, boolean strictMatching, ObjectToStringConverter stringConverter) {
        this(adaptor, strictMatching, stringConverter, null);
    }

    /**
     * Creates a new AutoCompleteDocument for the given AbstractAutoCompleteAdaptor.
     * @param strictMatching true, if only items from the adaptor's list should
     * be allowed to be entered
     * @param adaptor The adaptor that will be used to find and select matching
     * items.
     */
    public AutoCompleteDocument(AbstractAutoCompleteAdaptor adaptor, boolean strictMatching) {
        this(adaptor, strictMatching, null);
    }

    /**
     * Creates the default backing document when no delegate is passed to this
     * document.
     *
     * @return the default backing document
     */
    protected Document createDefaultDocument() {
        return new PlainDocument();
    }

    @Override
    public void remove(int offs, int len) throws BadLocationException {
        // return immediately when selecting an item
        if (selecting) return;
        delegate.remove(offs, len);
        if (!strictMatching) {
            setSelectedItem(getText(0, getLength()), getText(0, getLength()));
            adaptor.getTextComponent().setCaretPosition(offs);
        }
    }

    @Override
    public void insertString(int offs, String str, AttributeSet a) throws BadLocationException {
        // return immediately when selecting an item
        if (selecting) return;
        // insert the string into the document
        delegate.insertString(offs, str, a);
        // lookup and select a matching item
        LookupResult lookupResult;
        String pattern = getText(0, getLength());

        if(pattern == null || pattern.length() == 0) {
            lookupResult = new LookupResult(null, "");
            setSelectedItem(lookupResult.matchingItem, lookupResult.matchingString);
        } else {
            lookupResult = lookupItem(pattern);
        }

        if (lookupResult.matchingItem != null) {
            setSelectedItem(lookupResult.matchingItem, lookupResult.matchingString);
        } else {
            if (strictMatching) {
                // keep old item selected if there is no match
                lookupResult.matchingItem = adaptor.getSelectedItem();
                lookupResult.matchingString = adaptor.getSelectedItemAsString();
                // imitate no insert (later on offs will be incremented by
                // str.length(): selection won't move forward)
                offs = str == null ? offs : offs - str.length();

                if (str != null && !str.isEmpty()) {
                    // provide feedback to the user that his input has been received but can not be accepted
                    UIManager.getLookAndFeel().provideErrorFeedback(adaptor.getTextComponent());
                }
            } else {
                // no item matches => use the current input as selected item
                lookupResult.matchingItem=getText(0, getLength());
                lookupResult.matchingString=getText(0, getLength());
                setSelectedItem(lookupResult.matchingItem, lookupResult.matchingString);
            }
        }

        setText(lookupResult.matchingString);

        // select the completed part
        int len = str == null ? 0 : str.length();
        offs = lookupResult.matchingString == null ? 0 : offs + len;
        adaptor.markText(offs);
    }

    /**
     * Sets the text of this AutoCompleteDocument to the given text.
     *
     * @param text the text that will be set for this document
     */
    private void setText(String text) {
        try {
            // remove all text and insert the completed string
            delegate.remove(0, getLength());
            delegate.insertString(0, text, null);
        } catch (BadLocationException e) {
            throw new RuntimeException(e.toString());
        }
    }

    /**
     * Selects the given item using the AbstractAutoCompleteAdaptor.
     * @param itemAsString string representation of the item to be selected
     * @param item the item that is to be selected
     */
    private void setSelectedItem(Object item, String itemAsString) {
        selecting = true;
        adaptor.setSelectedItem(item);
        adaptor.setSelectedItemAsString(itemAsString);
        selecting = false;
    }

    /**
     * Searches for an item that matches the given pattern. The AbstractAutoCompleteAdaptor
     * is used to access the candidate items. The match is not case-sensitive
     * and will only match at the beginning of each item's string representation.
     *
     * @param pattern the pattern that should be matched
     * @return the first item that matches the pattern or <code>null</code> if no item matches
     */
    private LookupResult lookupItem(String pattern) {
        Object selectedItem = adaptor.getSelectedItem();

        LookupResult lookupResult;

        // first try: case sensitive

        lookupResult = lookupItem(pattern, EQUALS);
        if (lookupResult != null) return lookupResult;

        lookupResult = lookupOneItem(selectedItem, pattern, STARTS_WITH);
        if (lookupResult != null) return lookupResult;

        lookupResult = lookupItem(pattern, STARTS_WITH);
        if (lookupResult != null) return lookupResult;

        // second try: ignore case

        lookupResult = lookupItem(pattern, EQUALS_IGNORE_CASE);
        if (lookupResult != null) return lookupResult;

        lookupResult = lookupOneItem(selectedItem, pattern, STARTS_WITH_IGNORE_CASE);
        if (lookupResult != null) return lookupResult;

        lookupResult = lookupItem(pattern, STARTS_WITH_IGNORE_CASE);
        if (lookupResult != null) return lookupResult;

        // no item starts with the pattern => return null
        return new LookupResult(null, "");
    }

    private LookupResult lookupOneItem(Object item, String pattern, Comparator<String> comparator) {
        String[] possibleStrings = stringConverter.getPossibleStringsForItem(item);
        if (possibleStrings != null) {
            for (int j = 0; j < possibleStrings.length; j++) {
                if (comparator.compare(possibleStrings[j], pattern) == 0) {
                    return new LookupResult(item, possibleStrings[j]);
                }
            }
        }
        return null;
    }

    private LookupResult lookupItem(String pattern, Comparator<String> comparator) {
        // iterate over all items and return first match
        for (int i = 0, n = adaptor.getItemCount(); i < n; i++) {
            Object currentItem = adaptor.getItem(i);
            LookupResult result = lookupOneItem(currentItem, pattern, comparator);
            if (result != null) return result;
        }
        return null;
    }

    private static class LookupResult {
        Object matchingItem;
        String matchingString;
        public LookupResult(Object matchingItem, String matchingString) {
            this.matchingItem = matchingItem;
            this.matchingString = matchingString;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addDocumentListener(DocumentListener listener) {
        handler.addDocumentListener(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addUndoableEditListener(UndoableEditListener listener) {
        handler.addUndoableEditListener(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Position createPosition(int offs) throws BadLocationException {
        return delegate.createPosition(offs);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Element getDefaultRootElement() {
        return delegate.getDefaultRootElement();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Position getEndPosition() {
        return delegate.getEndPosition();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLength() {
        return delegate.getLength();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getProperty(Object key) {
        return delegate.getProperty(key);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Element[] getRootElements() {
        return delegate.getRootElements();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Position getStartPosition() {
        return delegate.getStartPosition();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getText(int offset, int length) throws BadLocationException {
        return delegate.getText(offset, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void getText(int offset, int length, Segment txt) throws BadLocationException {
        delegate.getText(offset, length, txt);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putProperty(Object key, Object value) {
        delegate.putProperty(key, value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeDocumentListener(DocumentListener listener) {
        handler.removeDocumentListener(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeUndoableEditListener(UndoableEditListener listener) {
        handler.removeUndoableEditListener(listener);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void render(Runnable r) {
        delegate.render(r);
    }

    /**
     * Returns if only items from the adaptor's list should be allowed to be entered.
     * @return if only items from the adaptor's list should be allowed to be entered
     */
    public boolean isStrictMatching() {
        return strictMatching;
    }
}