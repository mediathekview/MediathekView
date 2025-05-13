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

import javax.swing.*;

/**
 * An auto completion combobox.
 * <p/>
 * This class is a rewrite of {@link com.jidesoft.swing.AutoCompletionComboBox} and does not send action events when keys
 * are pressed if {@link #setNoActionOnKeyNavigation(boolean)} is set to true.
 */
public class AutoCompletionComboBox2 extends JComboBox<String> {
    protected AutoCompletion _autoCompletion;

    public AutoCompletionComboBox2() {
        initComponents();
    }

    protected void initComponents() {
        setEditable(true);

        _autoCompletion = createAutoCompletion();
        _autoCompletion.setStrict(true);
        _autoCompletion.setStrictCompletion(true);
        setNoActionOnKeyNavigation(true);
    }

    public void setNoActionOnKeyNavigation(boolean value) {
        ((NoFireOnKeyComboBoxSearchable)_autoCompletion.getSearchable()).setNoActionOnKeyNavigation(value);
    }

    /**
     * Creates the <code>AutoCompletion</code>.
     *
     * @return the <code>AutoCompletion</code>.
     */
    protected AutoCompletion createAutoCompletion() {
        return new AutoCompletion(this, new NoFireOnKeyComboBoxSearchable(this));
    }

    protected void resetCaretPosition() {
        final var tf = (JTextField)getEditor().getEditorComponent();
        final var textLength = tf.getText().length();
        if (textLength != 0)
            tf.setCaretPosition(textLength);
    }

    @Override
    protected void fireActionEvent() {
        if (!((NoFireOnKeyComboBoxSearchable)_autoCompletion.getSearchable()).isPreventActionEvent()) {
            resetCaretPosition();
            super.fireActionEvent();
        }
    }

}