package mediathek.gui.filterpanel.filterpanel;

import ca.odell.glazedlists.*;
import ca.odell.glazedlists.event.ListEvent;
import ca.odell.glazedlists.swing.EventListModel;
import mediathek.controller.SenderFilmlistLoadApproverSwing;
import mediathek.tool.GermanStringSorter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SenderBoxNodeSwing extends JPanel {
    private JList<String> senderList;
    private EventList<String> sortedSenderList;

    public SenderBoxNodeSwing() {
        setLayout(new BorderLayout());

        // Gefilterte Liste basierend auf SenderFilmlistLoadApprover
        FilterList<String> filteredSenderList = new FilterList<>(SenderListBoxModelSwing.getProvidedSenderList());
        filteredSenderList.setMatcher(SenderFilmlistLoadApproverSwing::isApproved);

        // Sortierte Liste mit deutschen Sortierregeln
        sortedSenderList = new SortedList<>(new UniqueList<>(filteredSenderList), GermanStringSorter.getInstance());


        // ReadOnlyList Wrapper
        ReadOnlyList readOnlyList = new ReadOnlyList(sortedSenderList);

        // Swing EventListModel für JList
        senderList = new JList<>(new EventListModel<>(readOnlyList));
        senderList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        senderList.setCellRenderer(new CheckListRenderer());

        // Kontextmenü
        JPopupMenu contextMenu = new JPopupMenu();
        JMenuItem miClearChecks = new JMenuItem("Alle Senderfilter zurücksetzen");
        miClearChecks.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                senderList.clearSelection();
            }
        });
        contextMenu.add(miClearChecks);
        senderList.setComponentPopupMenu(contextMenu);

        add(new JScrollPane(senderList), BorderLayout.CENTER);
    }

    // Renderer für Checkboxen
    static class CheckListRenderer extends JCheckBox implements ListCellRenderer<String> {
        @Override
        public Component getListCellRendererComponent(JList<? extends String> list, String value, int index, boolean isSelected, boolean cellHasFocus) {
            setText(value);
            setSelected(isSelected);
            return this;
        }
    }

    // ReadOnlyList-Implementierung
    static class ReadOnlyList extends TransformedList<String, String> {
        public ReadOnlyList(EventList<String> source) {
            super(source);
            source.addListEventListener(this);
        }

        @Override
        public boolean isWritable() {
            return false;
        }

        @Override
        public void listChanged(ListEvent<String> listChanges) {
            updates.forwardEvent(listChanges);
        }
    }
}

