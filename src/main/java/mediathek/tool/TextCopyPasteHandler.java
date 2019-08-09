package mediathek.tool;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.event.ActionEvent;

public class TextCopyPasteHandler<T extends JTextComponent> {
    private final T textComponent;
    private final Action cutAction;
    private final Action copyAction;
    private final Action pasteAction;
    private final Action undoAction;
    private final Action selectAllAction;
    private final JPopupMenu popup = new JPopupMenu();
    private String savedString = "";
    private Actions lastActionSelected;

    public TextCopyPasteHandler(T component) {
        this.textComponent = component;

        undoAction = new AbstractAction("Widerrufen") {
            @Override
            public void actionPerformed(ActionEvent ae) {
                textComponent.setText("");
                textComponent.replaceSelection(savedString);

                lastActionSelected = Actions.UNDO;
            }
        };

        popup.add(undoAction);
        popup.addSeparator();

        cutAction = new AbstractAction("Ausschneiden") {
            @Override
            public void actionPerformed(ActionEvent ae) {
                lastActionSelected = Actions.CUT;
                savedString = textComponent.getText();
                textComponent.cut();
            }
        };

        popup.add(cutAction);

        copyAction = new AbstractAction("Kopieren") {
            @Override
            public void actionPerformed(ActionEvent ae) {
                lastActionSelected = Actions.COPY;
                textComponent.copy();
            }
        };

        popup.add(copyAction);

        pasteAction = new AbstractAction("Einfügen") {
            @Override
            public void actionPerformed(ActionEvent ae) {
                lastActionSelected = Actions.PASTE;
                savedString = textComponent.getText();
                textComponent.paste();
            }
        };

        popup.add(pasteAction);
        popup.addSeparator();

        selectAllAction = new AbstractAction("Alles markieren") {
            @Override
            public void actionPerformed(ActionEvent ae) {
                lastActionSelected = Actions.SELECT_ALL;
                textComponent.selectAll();
            }
        };

        popup.add(selectAllAction);

        popup.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                final boolean enabled = textComponent.isEnabled();
                final boolean editable = textComponent.isEditable();
                final boolean nonempty = !(textComponent.getText() == null || textComponent.getText().isEmpty());
                final boolean marked = textComponent.getSelectedText() != null;
                final boolean pasteAvailable = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null).isDataFlavorSupported(DataFlavor.stringFlavor);

                textComponent.requestFocus();

                undoAction.setEnabled(enabled &&
                        editable &&
                        (lastActionSelected == Actions.CUT || lastActionSelected == Actions.PASTE));
                cutAction.setEnabled(enabled && editable && marked);
                copyAction.setEnabled(enabled && marked);
                pasteAction.setEnabled(enabled && editable && pasteAvailable);
                selectAllAction.setEnabled(enabled && nonempty);
            }

            @Override
            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {

            }

            @Override
            public void popupMenuCanceled(PopupMenuEvent e) {

            }
        });
    }

    public JPopupMenu getPopupMenu() {
        return popup;
    }

    private enum Actions {
        UNDO, CUT, COPY, PASTE, SELECT_ALL
    }
}
