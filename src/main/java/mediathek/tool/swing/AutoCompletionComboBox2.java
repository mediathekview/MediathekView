/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.swing;

import com.jidesoft.swing.AutoCompletion;
import com.jidesoft.swing.ComboBoxSearchable;

import javax.swing.*;

/**
 * An auto completion combobox.
 * <p/>
 * Since auto-complete has to listen to the key user types, it has to be editable. If you want to limit user to the list
 * available in the combobox model, you can call {@link #setStrict(boolean)} and set it to true.
 * This class is a rewrite of {@link com.jidesoft.swing.AutoCompletionComboBox} and does not send action events when keys
 * are pressed if {@link #setNoActionOnKeyNavigation(boolean)} is set to true.
 */
public class AutoCompletionComboBox2 extends JComboBox<String> {
    protected AutoCompletion _autoCompletion;
    boolean _noActionOnKeyNavigation;
    private boolean _preventActionEvent;

    public AutoCompletionComboBox2() {
        initComponents();
    }

    protected void initComponents() {
        setEditable(true);
        _autoCompletion = createAutoCompletion();
    }

    public void setNoActionOnKeyNavigation(boolean _noActionOnKeyNavigation) {
        this._noActionOnKeyNavigation = _noActionOnKeyNavigation;
    }

    /**
     * Creates the <code>AutoCompletion</code>.
     *
     * @return the <code>AutoCompletion</code>.
     */
    protected AutoCompletion createAutoCompletion() {
        return new AutoCompletion(this, new NoFireOnKeyComboBoxSearchable());
    }

    /**
     * Sets the strict property. If true, it will not allow user to type in anything that is not in the known item list.
     * If false, user can type in whatever he/she wants. If the text can match with a item in the known item list, it
     * will still auto-complete.
     *
     * @param strict true or false.
     */
    public void setStrict(boolean strict) {
        getAutoCompletion().setStrict(strict);
    }

    /**
     * Sets the strict completion property. If true, in case insensitive searching, it will always use the exact item in
     * the Searchable to replace whatever user types. For example, when Searchable has an item "Arial" and user types in
     * "AR", if this flag is true, it will auto-completed as "Arial". If false, it will be auto-completed as "ARial". Of
     * course, this flag will only make a difference if Searchable is case insensitive.
     *
     * @param strictCompletion true or false.
     */
    public void setStrictCompletion(boolean strictCompletion) {
        getAutoCompletion().setStrictCompletion(strictCompletion);
    }

    /**
     * Gets the underlying AutoCompletion class.
     *
     * @return the underlying AutoCompletion.
     */
    public AutoCompletion getAutoCompletion() {
        return _autoCompletion;
    }

    protected void resetCaretPosition() {
        final var tf = (JTextField)getEditor().getEditorComponent();
        final var textLength = tf.getText().length();
        if (textLength != 0)
            tf.setCaretPosition(textLength);
    }

    @Override
    protected void fireActionEvent() {
        if (!_preventActionEvent) {
            resetCaretPosition();
            super.fireActionEvent();
        }
    }

    private class NoFireOnKeyComboBoxSearchable extends ComboBoxSearchable {
        public NoFireOnKeyComboBoxSearchable() {
            super(AutoCompletionComboBox2.this);
        }

        @Override
        protected void setSelectedIndex(int index, boolean incremental) {
            Object propTableCellEditor = AutoCompletionComboBox2.this.getClientProperty("JComboBox.isTableCellEditor");
            Object propNoActionOnKeyNavigation = UIManager.get("ComboBox.noActionOnKeyNavigation");
            if ((propTableCellEditor instanceof Boolean && (Boolean) propTableCellEditor) ||
                    (propNoActionOnKeyNavigation instanceof Boolean && (Boolean) propNoActionOnKeyNavigation) ||
                    _noActionOnKeyNavigation) {
                _preventActionEvent = true;
            }
            try {
                super.setSelectedIndex(index, incremental);
            } finally {
                _preventActionEvent = false;
            }
        }
    }
}