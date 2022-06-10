package mediathek.gui.dialogEinstellungen;

import jiconfont.icons.font_awesome.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.file.GetFile;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.tool.*;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.table.TableStringConverter;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.PatternSyntaxException;

public class PanelBlacklist extends JPanel {
    public boolean ok;
    private final String name;
    private final Daten daten;
    private final JFrame parentComponent;

    public PanelBlacklist(Daten d, JFrame parentComponent, String nname) {
        daten = d;
        this.parentComponent = parentComponent;

        initComponents();
        name = nname;
        jButtonHilfe.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jButtonTabelleLoeschen.setIcon(IconFontSwing.buildIcon(FontAwesome.TRASH_O, 16));

        jButtonAendern.setEnabled(jTableBlacklist.getSelectionModel().getSelectedItemsCount() == 1);

        jTableBlacklist.setModel(tableModel);

        tableModel.addTableModelListener(l -> jButtonTabelleLoeschen.setEnabled(tableModel.getRowCount() != 0));
        jTableBlacklist.getSelectionModel().addListSelectionListener(l -> {
            if (!l.getValueIsAdjusting()) {
                jButtonAendern.setEnabled(jTableBlacklist.getSelectionModel().getSelectedItemsCount() == 1);

                if (jTableBlacklist.getSelectionModel().getSelectedItemsCount() == 0){
                    resetRuleEntryFields();
                }
            }
        });

        jCheckBoxGeo.addActionListener(e -> {
            ApplicationConfiguration.getConfiguration().
                    setProperty(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, jCheckBoxGeo.isSelected());
            notifyBlacklistChanged();
        });

        init_();
        init();

        MessageBus.getMessageBus().subscribe(this);

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

        //Table filtering
        setupTableFilter();

        lblNumEntries.setText(Integer.toString(jTableBlacklist.getRowCount()));
        jTableBlacklist.getModel().addTableModelListener(e -> lblNumEntries.setText(Integer.toString(jTableBlacklist.getRowCount())));
    }

    private static final Logger logger = LogManager.getLogger();

    private void setupTableFilter() {
        final TableRowSorter<BlacklistRuleTableModel> sorter = new TableRowSorter<>(tableModel);
        // make search case-insensitive
        sorter.setStringConverter(new TableStringConverter() {
            @Override
            public String toString(TableModel model, int row, int column) {
                return model.getValueAt(row, column).toString().toLowerCase();
            }
        });
        jTableBlacklist.setRowSorter(sorter);
        btnFilterTable.addActionListener(l -> {
            String text = tfFilter.getText();
            if(text.isEmpty()) {
                sorter.setRowFilter(null);
            } else {
                try {
                    sorter.setRowFilter(RowFilter.regexFilter(text.toLowerCase()));
                } catch(PatternSyntaxException pse) {
                    logger.error("Bad regex pattern", pse);
                }
            }
        });
    }

    private void resetRuleEntryFields() {
        jTextFieldTitel.setText("");
        jTextFieldThemaTitel.setText("");
        jComboBoxThema.setSelectedItem("");
        jComboBoxSender.setSelectedItem("");
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        SwingUtilities.invokeLater(this::init_);
    }

    /**
     * TableModel which syncs table view with ListBlacklist entries.
     */
    private final BlacklistRuleTableModel tableModel = new BlacklistRuleTableModel();

    private void init_() {
        jCheckBoxAbo.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO)));
        jCheckBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
        jCheckBoxBlacklistEingeschaltet.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON)));

        jCheckBoxZukunftNichtAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN)));

        var config = ApplicationConfiguration.getConfiguration();
        jCheckBoxGeo.setSelected(config.getBoolean(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS,false));

        try {
            jSliderMinuten.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)));
        } catch (Exception ex) {
            jSliderMinuten.setValue(0);
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, "0");
        }

        tableModel.fireTableDataChanged();
    }

    private void init() {
        jTableBlacklist.addMouseListener(new BeobMausTabelle());
        jTableBlacklist.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                fillControlsWithRuleData();
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
        jButtonHinzufuegen.addActionListener(e -> onAddBlacklistRule());

        jButtonAendern.addActionListener(e -> onChangeBlacklistRule());

        jButtonHilfe.addActionListener(e -> new DialogHilfe(parentComponent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BLACKLIST)).setVisible(true));
        jButtonTabelleLoeschen.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parentComponent,
                    "<html>Möchten Sie wirklich <b>alle Regeln</b> dauerhaft löschen?</html>",
                    "Blacklist Regeln", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                tableModel.removeAll();
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

        jComboBoxSender.setModel(new SenderListComboBoxModel());

        comboThemaLaden();

        var handler = new TextCopyPasteHandler<>(jTextFieldThemaTitel);
        jTextFieldThemaTitel.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldTitel);
        jTextFieldTitel.setComponentPopupMenu(handler.getPopupMenu());
    }

    /**
     * Apply changes to an existing rule.
     */
    private void onChangeBlacklistRule() {
        String strSender = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();
        String strThema = Objects.requireNonNull(jComboBoxThema.getSelectedItem()).toString();
        String strTitel = jTextFieldTitel.getText().trim();
        String strThemaTitel = jTextFieldThemaTitel.getText().trim();
        if (!strSender.isEmpty() || !strThema.isEmpty() || !strTitel.isEmpty() || !strThemaTitel.isEmpty()) {
            int selectedTableRow = jTableBlacklist.getSelectedRow();
            if (selectedTableRow != -1) {
                int modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
                var bl = tableModel.get(modelIndex);
                bl.setSender(strSender);
                bl.setThema(strThema);
                bl.setTitel(strTitel);
                bl.setThema_titel(strThemaTitel);

                tableModel.fireTableRowsUpdated(modelIndex, modelIndex);

                notifyBlacklistChanged();
            }
        }
    }

    private void notifyBlacklistChanged() {
        daten.getListeBlacklist().filterListe();
        MessageBus.getMessageBus().publishAsync(new BlacklistChangedEvent());
    }

    private void comboThemaLaden() {
        String filterSender = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();

        if (filterSender.isEmpty())
            filterSender = "";

        java.util.List<String> lst = daten.getListeFilme().getThemen(filterSender);
        DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
        model.addElement("");
        for (String item : lst)
            model.addElement(item);
        jComboBoxThema.setModel(model);

        lst.clear();
    }

    private void fillControlsWithRuleData() {
        int selectedTableRow = jTableBlacklist.getSelectedRow();
        if (selectedTableRow != -1) {
            int modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
            var bl = tableModel.get(modelIndex);
            jComboBoxSender.setSelectedItem(bl.getSender());
            jComboBoxThema.setSelectedItem(bl.getThema());
            jTextFieldTitel.setText(bl.getTitel());
            jTextFieldThemaTitel.setText(bl.getThema_titel());
        }
    }

    /**
     * Add a new blacklist rule to the model
     */
    private void onAddBlacklistRule() {
        String strSender = Objects.requireNonNull(jComboBoxSender.getSelectedItem()).toString();
        String strThema = Objects.requireNonNull(jComboBoxThema.getSelectedItem()).toString();
        String strTitel = jTextFieldTitel.getText().trim();
        String strThemaTitel = jTextFieldThemaTitel.getText().trim();

        if (!strSender.isEmpty() || !strThema.isEmpty() || !strTitel.isEmpty() || !strThemaTitel.isEmpty()) {
            var rule = new BlacklistRule(strSender, strThema, strTitel, strThemaTitel);
            if (!tableModel.contains(rule)) {
                tableModel.addRule(rule);
                resetRuleEntryFields();
            }
            else {
                //duplicate rule
                var msg = """
                        Es existiert bereits eine gleichlautende Regel.
                        Es dürfen keine Duplikate in der Liste vorkommen.
                        """;
                MVMessageDialog.showMessageDialog(this, msg, Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
            }
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

        /**
         * Remove one or more selected BlacklistRule objects from model.
         */
        private void onRemoveBlacklistRules() {
            var selectedIndices = jTableBlacklist.getSelectionModel().getSelectedIndices();
            if (selectedIndices.length == 1) {
                int modelIndex = jTableBlacklist.convertRowIndexToModel(selectedIndices[0]);
                tableModel.removeRow(modelIndex);
            }
            else {
                List<BlacklistRule> tempStore = new ArrayList<>();
                for (var selectedRow : selectedIndices) {
                    int modelIndex = jTableBlacklist.convertRowIndexToModel(selectedRow);
                    var rule = tableModel.get(modelIndex);
                    tempStore.add(rule);
                }
                tableModel.removeRules(tempStore);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            String menuText;
            if (jTableBlacklist.getSelectedRowCount() > 1)
                menuText = "Zeilen löschen";
            else
                menuText = "Zeile löschen";
            JMenuItem item = new JMenuItem(menuText);
            item.addActionListener(l -> onRemoveBlacklistRules());
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
        var jPanel1 = new JPanel();
        var jScrollPane1 = new JScrollPane();
        jTableBlacklist = new JTable();
        var jPanel4 = new JPanel();
        var jLabel5 = new JLabel();
        jComboBoxSender = new JComboBox<>();
        var jLabel6 = new JLabel();
        jComboBoxThema = new JComboBox<>();
        jButtonHinzufuegen = new JButton();
        var jLabel8 = new JLabel();
        jTextFieldTitel = new JTextField();
        var jLabel2 = new JLabel();
        jTextFieldThemaTitel = new JTextField();
        jButtonAendern = new JButton();
        var panel2 = new JPanel();
        jRadioButtonBlacklist = new JRadioButton();
        jRadioButtonWhitelist = new JRadioButton();
        jButtonHilfe = new JButton();
        var separator1 = new JSeparator();
        var label1 = new JLabel();
        tfFilter = new JTextField();
        btnFilterTable = new JButton();
        var panel1 = new JPanel();
        var panel3 = new JPanel();
        var label2 = new JLabel();
        lblNumEntries = new JLabel();
        var jLabel10 = new JLabel();
        jButtonTabelleLoeschen = new JButton();
        var separator2 = new JSeparator();
        var jPanel3 = new JPanel();
        jCheckBoxZukunftNichtAnzeigen = new JCheckBox();
        jCheckBoxGeo = new JCheckBox();
        var jPanel6 = new JPanel();
        jSliderMinuten = new JSlider();
        var jLabel1 = new JLabel();
        jTextFieldMinuten = new JTextField();
        var jLabel13 = new JLabel();
        var jPanel8 = new JPanel();
        jCheckBoxStart = new JCheckBox();
        jCheckBoxBlacklistEingeschaltet = new JCheckBox();
        jCheckBoxAbo = new JCheckBox();

        //======== this ========
        setMaximumSize(new Dimension(2147483647, 800));
        setLayout(new BorderLayout());

        //======== jTabbedPaneBlacklist ========
        {

            //======== jPanel1 ========
            {
                jPanel1.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .gap()
                        .gap()
                        .gap()
                        .gap()
                        .grow().fill().gap()
                        .gap()
                        .gap()
                        ));

                //======== jScrollPane1 ========
                {
                    jScrollPane1.setMinimumSize(new Dimension(22, 50));
                    jScrollPane1.setPreferredSize(new Dimension(454, 200));
                    jScrollPane1.setViewportView(jTableBlacklist);
                }
                jPanel1.add(jScrollPane1, new CC().cell(0, 4));

                //======== jPanel4 ========
                {
                    jPanel4.setBorder(new TitledBorder("Sender, Thema, Titel oder Thema/Titel:")); //NON-NLS

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
                                        .addGap(68, 68, 68)
                                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                            .addComponent(jLabel6)
                                            .addComponent(jLabel5))
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addGroup(jPanel4Layout.createParallelGroup()
                                            .addComponent(jComboBoxSender, GroupLayout.DEFAULT_SIZE, 714, Short.MAX_VALUE)
                                            .addComponent(jComboBoxThema)))
                                    .addGroup(jPanel4Layout.createSequentialGroup()
                                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                            .addComponent(jLabel2)
                                            .addComponent(jLabel8))
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addGroup(jPanel4Layout.createParallelGroup()
                                            .addComponent(jTextFieldTitel)
                                            .addComponent(jTextFieldThemaTitel)))
                                    .addGroup(GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                                        .addGap(0, 620, Short.MAX_VALUE)
                                        .addComponent(jButtonAendern)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonHinzufuegen)))
                                .addContainerGap())
                    );
                    jPanel4Layout.linkSize(SwingConstants.HORIZONTAL, new Component[] {jButtonAendern, jButtonHinzufuegen});
                    jPanel4Layout.setVerticalGroup(
                        jPanel4Layout.createParallelGroup()
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
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
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(jButtonHinzufuegen)
                                    .addComponent(jButtonAendern))
                                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    );
                    jPanel4Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jComboBoxSender, jComboBoxThema, jTextFieldThemaTitel, jTextFieldTitel});
                }
                jPanel1.add(jPanel4, new CC().cell(0, 7));

                //======== panel2 ========
                {
                    panel2.setLayout(new MigLayout(
                        new LC().insets("0").hideMode(3).gridGap("5", "5"), //NON-NLS
                        // columns
                        new AC()
                            .fill().gap()
                            .grow().align("right"), //NON-NLS
                        // rows
                        new AC()
                            .fill().gap()
                            .fill()));

                    //---- jRadioButtonBlacklist ----
                    jRadioButtonBlacklist.setSelected(true);
                    jRadioButtonBlacklist.setText("\"Sender / Thema / Titel\" werden nicht angezeigt (Blacklist)"); //NON-NLS
                    panel2.add(jRadioButtonBlacklist, new CC().cell(0, 0));

                    //---- jRadioButtonWhitelist ----
                    jRadioButtonWhitelist.setText("nur diese \"Sender / Thema / Titel\" anzeigen (Whitelist)"); //NON-NLS
                    panel2.add(jRadioButtonWhitelist, new CC().cell(0, 1));

                    //---- jButtonHilfe ----
                    jButtonHilfe.setToolTipText("Hilfe anzeigen"); //NON-NLS
                    panel2.add(jButtonHilfe, new CC().cell(1, 0, 1, 2).alignX("right").alignY("center").grow(0, 0)); //NON-NLS
                }
                jPanel1.add(panel2, new CC().cell(0, 0));
                jPanel1.add(separator1, new CC().cell(0, 2));

                //---- label1 ----
                label1.setText("Suchstring:"); //NON-NLS
                jPanel1.add(label1, new CC().cell(0, 3).alignX("center").growX(0)); //NON-NLS

                //---- tfFilter ----
                tfFilter.setToolTipText("<html>Hier wird der Suchtext als RegExp eingegeben.<br>Zum Zur\u00fccksetzen des Filters den Suchtext <b>l\u00f6schen</b> und erneut <i>\"Filtern\"</i> dr\u00fccken.</html>"); //NON-NLS
                jPanel1.add(tfFilter, new CC().cell(0, 3));

                //---- btnFilterTable ----
                btnFilterTable.setText("Filtern"); //NON-NLS
                jPanel1.add(btnFilterTable, new CC().cell(0, 3).alignX("center").growX(0)); //NON-NLS

                //======== panel1 ========
                {
                    panel1.setLayout(new MigLayout(
                        new LC().insets("0").hideMode(3).gridGap("5", "0"), //NON-NLS
                        // columns
                        new AC()
                            .fill().gap()
                            .grow().fill().gap()
                            .fill(),
                        // rows
                        new AC()
                            .grow().align("center"))); //NON-NLS

                    //======== panel3 ========
                    {
                        panel3.setLayout(new MigLayout(
                            new LC().insets("0").hideMode(3).gridGap("5", "0"), //NON-NLS
                            // columns
                            new AC()
                                .fill().gap()
                                .fill(),
                            // rows
                            new AC()
                                .grow().align("center"))); //NON-NLS

                        //---- label2 ----
                        label2.setText("Eintr\u00e4ge:"); //NON-NLS
                        panel3.add(label2, new CC().cell(0, 0));

                        //---- lblNumEntries ----
                        lblNumEntries.setText("0"); //NON-NLS
                        panel3.add(lblNumEntries, new CC().cell(1, 0));
                    }
                    panel1.add(panel3, new CC().cell(0, 0));

                    //---- jLabel10 ----
                    jLabel10.setText("Alle Eintr\u00e4ge l\u00f6schen:"); //NON-NLS
                    jLabel10.setHorizontalAlignment(SwingConstants.RIGHT);
                    panel1.add(jLabel10, new CC().cell(1, 0));

                    //---- jButtonTabelleLoeschen ----
                    jButtonTabelleLoeschen.setToolTipText("Alle Eintr\u00e4ge l\u00f6schen"); //NON-NLS
                    panel1.add(jButtonTabelleLoeschen, new CC().cell(2, 0));
                }
                jPanel1.add(panel1, new CC().cell(0, 5));
                jPanel1.add(separator2, new CC().cell(0, 6));
            }
            jTabbedPaneBlacklist.addTab("Sender-Thema-Titel", jPanel1); //NON-NLS

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
                                        .addGap(0, 424, Short.MAX_VALUE)))
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
        }
        add(jTabbedPaneBlacklist, BorderLayout.CENTER);

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
        add(jPanel8, BorderLayout.NORTH);

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonBlacklist);
        buttonGroup1.add(jRadioButtonWhitelist);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
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
    private JTextField tfFilter;
    private JButton btnFilterTable;
    private JLabel lblNumEntries;
    private JButton jButtonTabelleLoeschen;
    private JCheckBox jCheckBoxZukunftNichtAnzeigen;
    private JCheckBox jCheckBoxGeo;
    private JSlider jSliderMinuten;
    private JTextField jTextFieldMinuten;
    private JCheckBox jCheckBoxStart;
    private JCheckBox jCheckBoxBlacklistEingeschaltet;
    private JCheckBox jCheckBoxAbo;
    // End of variables declaration//GEN-END:variables
}
