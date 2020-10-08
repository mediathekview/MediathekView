package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.file.GetFile;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.tool.*;
import net.engio.mbassy.listener.Handler;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Objects;

@SuppressWarnings("serial")
public class PanelBlacklist extends JPanel {
    public boolean ok;
    public String ziel;
    private final String name;
    private final Daten daten;
    private final JFrame parentComponent;

    public PanelBlacklist(Daten d, JFrame parentComponent, String nname) {
        daten = d;
        this.parentComponent = parentComponent;

        initComponents();
        name = nname;
        jButtonHilfe.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonTabelleLoeschen.setIcon(Icons.ICON_BUTTON_DEL);
        init_();
        init();

        daten.getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_START_GEAENDERT, name) {
            @Override
            public void ping() {
                jCheckBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, name) {
            @Override
            public void ping() {
                init_();
            }
        });
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                comboThemaLaden();
            }
        });
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        SwingUtilities.invokeLater(this::init_);
    }

    private void init_() {
        jCheckBoxAbo.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO)));
        jCheckBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
        jCheckBoxBlacklistEingeschaltet.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON)));

        jCheckBoxZukunftNichtAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN)));

        var config = ApplicationConfiguration.getConfiguration();
        jCheckBoxGeo.setSelected(config.getBoolean(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS,false));
        jCheckBoxGeo.addActionListener(e -> {
            config.setProperty(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, jCheckBoxGeo.isSelected());
            notifyBlacklistChanged();
        });

        try {
            jSliderMinuten.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)));
        } catch (Exception ex) {
            jSliderMinuten.setValue(0);
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, "0");
        }
        tabelleLaden();
    }

    private void init() {
        jTableBlacklist.addMouseListener(new BeobMausTabelle());
        jTableBlacklist.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                tableSelect();
            }
        });

        jRadioButtonWhitelist.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST)));
        jRadioButtonWhitelist.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, Boolean.toString(jRadioButtonWhitelist.isSelected()));
            notifyBlacklistChanged();
        });
        jRadioButtonBlacklist.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, Boolean.toString(jRadioButtonWhitelist.isSelected()));
            notifyBlacklistChanged();
        });
        jCheckBoxZukunftNichtAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN, Boolean.toString(jCheckBoxZukunftNichtAnzeigen.isSelected()));
            notifyBlacklistChanged();
        });
        jCheckBoxAbo.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO, Boolean.toString(jCheckBoxAbo.isSelected()));
            // bei den Downloads melden
            // damit die Änderungen im Eigenschaftendialog auch übernommen werden
            Listener.notify(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, name);
        });
        jCheckBoxStart.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON, Boolean.toString(jCheckBoxStart.isSelected()));
            Listener.notify(Listener.EREIGNIS_BLACKLIST_START_GEAENDERT, name);
        });
        jCheckBoxBlacklistEingeschaltet.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(jCheckBoxBlacklistEingeschaltet.isSelected()));
            notifyBlacklistChanged();
        });
        jButtonHinzufuegen.addActionListener(e -> {
            String se = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();
            String th = Objects.requireNonNull(jComboBoxThema.getSelectedItem()).toString();
            String ti = jTextFieldTitel.getText().trim();
            String thti = jTextFieldThemaTitel.getText().trim();
            if (!se.isEmpty() || !th.isEmpty() || !ti.isEmpty() || !thti.isEmpty()) {
                daten.getListeBlacklist().add(new BlacklistRule(se, th, ti, thti));
                tabelleLaden();
            }
        });
        jButtonAendern.addActionListener(e -> {
            String se = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();
            String th = Objects.requireNonNull(jComboBoxThema.getSelectedItem()).toString();
            String ti = jTextFieldTitel.getText().trim();
            String thti = jTextFieldThemaTitel.getText().trim();
            if (!se.isEmpty() || !th.isEmpty() || !ti.isEmpty() || !thti.isEmpty()) {
                int selectedTableRow = jTableBlacklist.getSelectedRow();
                if (selectedTableRow >= 0) {
                    int row = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
                    String delNr = jTableBlacklist.getModel().getValueAt(row, BlacklistRule.BLACKLIST_NR).toString();
                    BlacklistRule bl = daten.getListeBlacklist().getRuleByNr(delNr);
                    bl.arr[BlacklistRule.BLACKLIST_SENDER] = se;
                    bl.arr[BlacklistRule.BLACKLIST_THEMA] = th;
                    bl.arr[BlacklistRule.BLACKLIST_TITEL] = ti;
                    bl.arr[BlacklistRule.BLACKLIST_THEMA_TITEL] = thti;
                    tabelleLaden();
                    jTableBlacklist.addRowSelectionInterval(row, row);
                    notifyBlacklistChanged();
                }
            }

        });
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parentComponent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BLACKLIST)).setVisible(true));
        jButtonTabelleLoeschen.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parentComponent, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                daten.getListeBlacklist().clear();
                tabelleLaden();
            }
        });
        jComboBoxSender.addActionListener(e -> comboThemaLaden());

        var documentListener = new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                tus();
            }

            private void tus() {
                Filter.validatePatternInput(jTextFieldThemaTitel);
                Filter.validatePatternInput(jTextFieldTitel);
            }
        };
        jTextFieldTitel.getDocument().addDocumentListener(documentListener);
        jTextFieldThemaTitel.getDocument().addDocumentListener(documentListener);

        try {
            jSliderMinuten.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)));
        } catch (Exception ex) {
            jSliderMinuten.setValue(0);
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, "0");
        }
        jTextFieldMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
        if (jSliderMinuten.getValue() == 0) {
            jTextFieldMinuten.setText("alles");
        }
        jSliderMinuten.addChangeListener(e -> {
            jTextFieldMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
            if (jSliderMinuten.getValue() == 0) {
                jTextFieldMinuten.setText("alles");
            }
            if (!jSliderMinuten.getValueIsAdjusting()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, String.valueOf(jSliderMinuten.getValue()));
                notifyBlacklistChanged();
            }
        });

        jComboBoxSender.setModel(GuiFunktionen.getSenderListComboBoxModel(daten.getListeFilme()));

        comboThemaLaden();

        var handler = new TextCopyPasteHandler<>(jTextFieldThemaTitel);
        jTextFieldThemaTitel.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldTitel);
        jTextFieldTitel.setComponentPopupMenu(handler.getPopupMenu());
    }

    private void notifyBlacklistChanged() {
        daten.getListeBlacklist().filterListe();
        daten.getMessageBus().publishAsync(new BlacklistChangedEvent());
    }

    private void comboThemaLaden() {
        String filterSender = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();

        if (filterSender.isEmpty())
            filterSender = "";

        java.util.List<String> lst = daten.getListeFilmeNachBlackList().getThemen(filterSender);
        DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
        model.addElement("");
        for (String item : lst)
            model.addElement(item);
        jComboBoxThema.setModel(model);

        lst.clear();
    }

    private void tabelleLaden() {
        var model = new DefaultTableModel(daten.getListeBlacklist().getObjectData(), BlacklistRule.COLUMN_NAMES);
        jTableBlacklist.setModel(model);
    }

    private void tableSelect() {
        BlacklistRule bl = null;
        int selectedTableRow = jTableBlacklist.getSelectedRow();
        if (selectedTableRow >= 0) {
            int del = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
            String delNr = jTableBlacklist.getModel().getValueAt(del, BlacklistRule.BLACKLIST_NR).toString();
            bl = daten.getListeBlacklist().getRuleByNr(delNr);
        }
        if (bl != null) {
            jComboBoxSender.setSelectedItem(bl.arr[BlacklistRule.BLACKLIST_SENDER]);
            jComboBoxThema.setSelectedItem(bl.arr[BlacklistRule.BLACKLIST_THEMA]);
            jTextFieldTitel.setText(bl.arr[BlacklistRule.BLACKLIST_TITEL]);
            jTextFieldThemaTitel.setText(bl.arr[BlacklistRule.BLACKLIST_THEMA_TITEL]);
        }
    }

    private void removeTableRow() {
        int selectedTableRow = jTableBlacklist.getSelectedRow();
        if (selectedTableRow >= 0) {
            int del = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
            String delNr = jTableBlacklist.getModel().getValueAt(del, BlacklistRule.BLACKLIST_NR).toString();
            daten.getListeBlacklist().remove(Integer.parseInt(delNr));
            tabelleLaden();
        }
    }

    private class BeobMausTabelle extends MouseAdapter {

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            int nr;
            Point p = evt.getPoint();
            nr = jTableBlacklist.rowAtPoint(p);
            if (nr >= 0) {
                jTableBlacklist.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Zeile löschen");
            item.addActionListener(l -> removeTableRow());
            jPopupMenu.add(item);
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jTabbedPaneBlacklist = new JTabbedPane();
        var jPanel3 = new JPanel();
        jCheckBoxZukunftNichtAnzeigen = new JCheckBox();
        jCheckBoxGeo = new JCheckBox();
        var jPanel6 = new JPanel();
        jSliderMinuten = new JSlider();
        var jLabel1 = new JLabel();
        jTextFieldMinuten = new JTextField();
        var jLabel13 = new JLabel();
        var jPanel1 = new JPanel();
        var jScrollPane1 = new JScrollPane();
        jTableBlacklist = new JTable();
        var jPanel4 = new JPanel();
        var jLabel5 = new JLabel();
        jComboBoxSender = new JComboBox<>();
        var jLabel6 = new JLabel();
        jComboBoxThema = new JComboBox<>();
        jButtonHinzufuegen = new JButton();
        var jLabel7 = new JLabel();
        var jLabel8 = new JLabel();
        jTextFieldTitel = new JTextField();
        var jLabel2 = new JLabel();
        jTextFieldThemaTitel = new JTextField();
        jButtonAendern = new JButton();
        jRadioButtonBlacklist = new JRadioButton();
        jRadioButtonWhitelist = new JRadioButton();
        jButtonHilfe = new JButton();
        var jLabel10 = new JLabel();
        jButtonTabelleLoeschen = new JButton();
        var jPanel8 = new JPanel();
        jCheckBoxStart = new JCheckBox();
        jCheckBoxBlacklistEingeschaltet = new JCheckBox();
        jCheckBoxAbo = new JCheckBox();

        //======== this ========

        //======== jTabbedPaneBlacklist ========
        {

            //======== jPanel3 ========
            {
                jPanel3.setLayout(new VerticalLayout(5));

                //---- jCheckBoxZukunftNichtAnzeigen ----
                jCheckBoxZukunftNichtAnzeigen.setText("Filme mit Datum in der Zukunft nicht anzeigen"); //NON-NLS
                jPanel3.add(jCheckBoxZukunftNichtAnzeigen);

                //---- jCheckBoxGeo ----
                jCheckBoxGeo.setText("Filme, die per Geoblocking gesperrt sind, nicht anzeigen"); //NON-NLS
                jCheckBoxGeo.setToolTipText("<html>Geogeblockte Filme k\u00f6nnen im jeweiligen \"Ausland\" nicht abgerufen werden.<br>Dazu muss die eigene Position in den Einstellungen angegeben werden</html>"); //NON-NLS
                jPanel3.add(jCheckBoxGeo);

                //======== jPanel6 ========
                {
                    jPanel6.setBorder(new EtchedBorder());

                    //---- jSliderMinuten ----
                    jSliderMinuten.setValue(0);

                    //---- jLabel1 ----
                    jLabel1.setText("Nur Filme anzeigen mit einer L\u00e4nge von mehr als [min]:"); //NON-NLS

                    //---- jTextFieldMinuten ----
                    jTextFieldMinuten.setEditable(false);

                    //---- jLabel13 ----
                    jLabel13.setText("Filme, die keine L\u00e4ngenangabe haben, werden immer angezeigt."); //NON-NLS

                    GroupLayout jPanel6Layout = new GroupLayout(jPanel6);
                    jPanel6.setLayout(jPanel6Layout);
                    jPanel6Layout.setHorizontalGroup(
                        jPanel6Layout.createParallelGroup()
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel6Layout.createParallelGroup()
                                    .addComponent(jSliderMinuten, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addGroup(jPanel6Layout.createParallelGroup()
                                            .addGroup(jPanel6Layout.createSequentialGroup()
                                                .addComponent(jLabel1)
                                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldMinuten, GroupLayout.PREFERRED_SIZE, 81, GroupLayout.PREFERRED_SIZE))
                                            .addComponent(jLabel13))
                                        .addGap(0, 405, Short.MAX_VALUE)))
                                .addContainerGap())
                    );
                    jPanel6Layout.setVerticalGroup(
                        jPanel6Layout.createParallelGroup()
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel6Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jLabel1)
                                    .addComponent(jTextFieldMinuten, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jLabel13)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jSliderMinuten, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap())
                    );
                }
                jPanel3.add(jPanel6);
            }
            jTabbedPaneBlacklist.addTab("Blacklist allgemein", jPanel3); //NON-NLS

            //======== jPanel1 ========
            {

                //======== jScrollPane1 ========
                {

                    //---- jTableBlacklist ----
                    jTableBlacklist.setAutoCreateRowSorter(true);
                    jScrollPane1.setViewportView(jTableBlacklist);
                }

                //======== jPanel4 ========
                {
                    jPanel4.setBorder(new EtchedBorder());

                    //---- jLabel5 ----
                    jLabel5.setText("Sender:"); //NON-NLS

                    //---- jComboBoxSender ----
                    jComboBoxSender.setModel(new DefaultComboBoxModel<>(new String[] {

                    }));

                    //---- jLabel6 ----
                    jLabel6.setText("Thema:"); //NON-NLS

                    //---- jComboBoxThema ----
                    jComboBoxThema.setModel(new DefaultComboBoxModel<>(new String[] {

                    }));

                    //---- jButtonHinzufuegen ----
                    jButtonHinzufuegen.setText("Hinzuf\u00fcgen"); //NON-NLS

                    //---- jLabel7 ----
                    jLabel7.setText("Sender, Thema, Titel oder Thema/Titel:"); //NON-NLS

                    //---- jLabel8 ----
                    jLabel8.setText("Titel:"); //NON-NLS

                    //---- jLabel2 ----
                    jLabel2.setText("Thema oder Titel:"); //NON-NLS

                    //---- jButtonAendern ----
                    jButtonAendern.setText("\u00c4ndern"); //NON-NLS

                    GroupLayout jPanel4Layout = new GroupLayout(jPanel4);
                    jPanel4.setLayout(jPanel4Layout);
                    jPanel4Layout.setHorizontalGroup(
                        jPanel4Layout.createParallelGroup()
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel4Layout.createParallelGroup()
                                    .addGroup(jPanel4Layout.createSequentialGroup()
                                        .addComponent(jLabel7)
                                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                    .addGroup(jPanel4Layout.createSequentialGroup()
                                        .addGroup(jPanel4Layout.createParallelGroup()
                                            .addGroup(jPanel4Layout.createSequentialGroup()
                                                .addGap(68, 68, 68)
                                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                                    .addComponent(jLabel6)
                                                    .addComponent(jLabel5))
                                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                .addGroup(jPanel4Layout.createParallelGroup()
                                                    .addComponent(jComboBoxSender)
                                                    .addComponent(jComboBoxThema)))
                                            .addGroup(jPanel4Layout.createSequentialGroup()
                                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                                    .addComponent(jLabel2)
                                                    .addComponent(jLabel8))
                                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                .addGroup(jPanel4Layout.createParallelGroup()
                                                    .addComponent(jTextFieldTitel)
                                                    .addComponent(jTextFieldThemaTitel))))
                                        .addGap(12, 12, 12))))
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jButtonAendern)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonHinzufuegen)
                                .addContainerGap())
                    );
                    jPanel4Layout.linkSize(SwingConstants.HORIZONTAL, new Component[] {jButtonAendern, jButtonHinzufuegen});
                    jPanel4Layout.setVerticalGroup(
                        jPanel4Layout.createParallelGroup()
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addGap(23, 23, 23)
                                .addComponent(jLabel7)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jLabel5)
                                    .addComponent(jComboBoxSender, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jLabel6)
                                    .addComponent(jComboBoxThema, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jLabel8)
                                    .addComponent(jTextFieldTitel, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jLabel2)
                                    .addComponent(jTextFieldThemaTitel, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jButtonHinzufuegen)
                                    .addComponent(jButtonAendern))
                                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    );
                    jPanel4Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jComboBoxSender, jComboBoxThema, jTextFieldThemaTitel, jTextFieldTitel});
                }

                //---- jRadioButtonBlacklist ----
                jRadioButtonBlacklist.setSelected(true);
                jRadioButtonBlacklist.setText("\"Sender / Thema / Titel\" werden nicht angezeigt (Blacklist)"); //NON-NLS

                //---- jRadioButtonWhitelist ----
                jRadioButtonWhitelist.setText("nur diese \"Sender / Thema / Titel\" anzeigen (Whitelist)"); //NON-NLS

                //---- jButtonHilfe ----
                jButtonHilfe.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); //NON-NLS
                jButtonHilfe.setToolTipText("Hilfe anzeigen"); //NON-NLS

                //---- jLabel10 ----
                jLabel10.setText("Alle Eintr\u00e4ge l\u00f6schen:"); //NON-NLS

                //---- jButtonTabelleLoeschen ----
                jButtonTabelleLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); //NON-NLS
                jButtonTabelleLoeschen.setToolTipText("Alle Eintr\u00e4ge l\u00f6schen"); //NON-NLS

                GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
                jPanel1.setLayout(jPanel1Layout);
                jPanel1Layout.setHorizontalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel1Layout.createParallelGroup()
                                .addComponent(jScrollPane1, GroupLayout.Alignment.TRAILING)
                                .addComponent(jPanel4, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGroup(jPanel1Layout.createSequentialGroup()
                                    .addGroup(jPanel1Layout.createParallelGroup()
                                        .addComponent(jRadioButtonWhitelist)
                                        .addComponent(jRadioButtonBlacklist))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 373, Short.MAX_VALUE)
                                    .addComponent(jButtonHilfe))
                                .addGroup(GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                    .addGap(0, 0, Short.MAX_VALUE)
                                    .addComponent(jLabel10)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonTabelleLoeschen)))
                            .addContainerGap())
                );
                jPanel1Layout.setVerticalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel1Layout.createParallelGroup()
                                .addGroup(jPanel1Layout.createSequentialGroup()
                                    .addComponent(jRadioButtonBlacklist)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jRadioButtonWhitelist))
                                .addComponent(jButtonHilfe))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jScrollPane1, GroupLayout.DEFAULT_SIZE, 113, Short.MAX_VALUE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                .addComponent(jLabel10)
                                .addComponent(jButtonTabelleLoeschen))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                            .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addContainerGap())
                );
            }
            jTabbedPaneBlacklist.addTab("Sender-Thema-Titel", jPanel1); //NON-NLS
        }

        //======== jPanel8 ========
        {
            jPanel8.setBorder(new TitledBorder("Allgemeine Einstellungen")); //NON-NLS
            jPanel8.setLayout(new GridLayout(3, 1));

            //---- jCheckBoxStart ----
            jCheckBoxStart.setText("Beim Programmstart einschalten"); //NON-NLS
            jPanel8.add(jCheckBoxStart);

            //---- jCheckBoxBlacklistEingeschaltet ----
            jCheckBoxBlacklistEingeschaltet.setText("Im Tab Filme einschalten"); //NON-NLS
            jPanel8.add(jCheckBoxBlacklistEingeschaltet);

            //---- jCheckBoxAbo ----
            jCheckBoxAbo.setText("Bei der Suche nach Abos ber\u00fccksichtigen"); //NON-NLS
            jCheckBoxAbo.setToolTipText("<html>Die Blacklist beim Suchen nach Abos ber\u00fccksichtigen.<br/>Ansonsten wird die komplette Filmliste durchsucht.</html>"); //NON-NLS
            jPanel8.add(jCheckBoxAbo);
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jPanel8, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTabbedPaneBlacklist))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel8, GroupLayout.PREFERRED_SIZE, 97, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jTabbedPaneBlacklist, GroupLayout.PREFERRED_SIZE, 510, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap())
        );

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonBlacklist);
        buttonGroup1.add(jRadioButtonWhitelist);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox jCheckBoxZukunftNichtAnzeigen;
    private JCheckBox jCheckBoxGeo;
    private JSlider jSliderMinuten;
    private JTextField jTextFieldMinuten;
    private JTable jTableBlacklist;
    private JComboBox<String> jComboBoxSender;
    private JComboBox<String> jComboBoxThema;
    private JButton jButtonHinzufuegen;
    private JTextField jTextFieldTitel;
    private JTextField jTextFieldThemaTitel;
    private JButton jButtonAendern;
    private JRadioButton jRadioButtonBlacklist;
    private JRadioButton jRadioButtonWhitelist;
    private JButton jButtonHilfe;
    private JButton jButtonTabelleLoeschen;
    private JCheckBox jCheckBoxStart;
    private JCheckBox jCheckBoxBlacklistEingeschaltet;
    private JCheckBox jCheckBoxAbo;
    // End of variables declaration//GEN-END:variables
}
