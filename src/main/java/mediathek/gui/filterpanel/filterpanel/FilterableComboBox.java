package mediathek.gui.filterpanel.filterpanel;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class FilterableComboBox<E> extends JComboBox<E> {
    private List<E> array;
    private int currentCaretPosition = 0;

    // Konstruktor ohne Übergabeparameter
    public FilterableComboBox() {
        super();
        this.array = new ArrayList<>();
        this.setEditable(true);
        final JTextField textfield = (JTextField) this.getEditor().getEditorComponent();
        textfield.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent ke) {
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        currentCaretPosition = textfield.getCaretPosition();
                        if (textfield.getSelectedText() == null) {
                            textfield.setCaretPosition(0);
                            comboFilter((E) textfield.getText());
                            textfield.setCaretPosition(currentCaretPosition);
                        }
                    }
                });
            }
        });
    }

    public FilterableComboBox(List<E> array) {
        super();
        this.array = array;
        this.setEditable(true);
        final JTextField textfield = (JTextField) this.getEditor().getEditorComponent();
        textfield.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent ke) {
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        currentCaretPosition = textfield.getCaretPosition();
                        if (textfield.getSelectedText() == null) {
                            textfield.setCaretPosition(0);
                            comboFilter((E) textfield.getText());
                            textfield.setCaretPosition(currentCaretPosition);
                        }
                    }
                });
            }
        });

        setModel(new DefaultComboBoxModel<>(array.toArray((E[]) new Object[0])));
    }

    public void setList(List<E> aarray) {
        array = aarray;
        setModel(new DefaultComboBoxModel<>(aarray.toArray((E[]) new Object[0])));
    }

    public void comboFilter(E enteredText) {
        List<E> filterArray = new ArrayList<E>();
        for (int i = 0; i < array.size(); i++) {
            if (array.get(i).toString().toLowerCase().contains(enteredText.toString().toLowerCase())) {
                filterArray.add(array.get(i));
            }
        }
        if (filterArray.size() > 0) {
            setModel(new DefaultComboBoxModel<>(filterArray.toArray((E[]) new Object[0])));
            setSelectedItem(enteredText);
            showPopup();
        } else {
            hidePopup();
        }
    }
}
