package mediathek.gui.dialog;

import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.FileDialogs;
import mediathek.tool.FilenameUtils;
import org.apache.commons.configuration2.Configuration;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.util.Objects;

@SuppressWarnings("serial")
public class DialogAddMoreDownload extends JDialog {
    public boolean addAll;
    public boolean cancel;
    public boolean info;
    public boolean subtitle;

    private final DatenPset pSet;
    private final String orgPfad;
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    public DialogAddMoreDownload(JFrame parent, DatenPset pSet) {
        super(parent);
        this.pSet = pSet;

        initComponents();

        chkSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE]));
        subtitle = chkSubtitle.isSelected();
        chkSubtitle.addActionListener(l -> subtitle = chkSubtitle.isSelected());

        chkInfo.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI]));
        info = chkInfo.isSelected();
        chkInfo.addActionListener(l -> info = chkInfo.isSelected());

        chkStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        chkStart.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(chkStart.isSelected())));

        jCheckBoxPfadSpeichern.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true));
        jCheckBoxPfadSpeichern.addActionListener(e -> config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, jCheckBoxPfadSpeichern.isSelected()));

        btnChange.addActionListener(l -> beenden());
        btnOk.addActionListener(e -> {
            addAll = true;
            beenden();
        });
        btnCancel.addActionListener(l -> {
            cancel = true;
            beenden();
        });

        jButtonPath.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonPath.addActionListener(l -> {
            var initialDirectory = "";
            var cbItem = Objects.requireNonNull(jComboBoxPath.getSelectedItem()).toString();
            if (!cbItem.isEmpty()) {
                initialDirectory = cbItem;
            }
            var selectedDirectory = FileDialogs.chooseDirectoryLocation(MediathekGui.ui(),"Film speichern",initialDirectory);
            if (selectedDirectory != null) {
                final String absolutePath = selectedDirectory.getAbsolutePath();
                jComboBoxPath.addItem(absolutePath);
                jComboBoxPath.setSelectedItem(absolutePath);
            }
        });


        jButtonDelPath.setIcon(Icons.ICON_BUTTON_DEL);
        jButtonDelPath.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[]{pSet.getZielPfad()}));
        });

        DialogAddDownload.setModelPfad(pSet.getZielPfad(), jComboBoxPath);
        orgPfad = pSet.getZielPfad();
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {

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
                String s = ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText();
                if (!s.equals(FilenameUtils.checkDateiname(s, true /*pfad*/))) {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(Color.WHITE);
                }
            }
        });

        EscapeKeyHandler.installHandler(this, () -> {
            cancel = true;
            beenden();
        });

        pack();
    }

    public String getPath() {
        String path = jComboBoxPath.getModel().getSelectedItem().toString();
        if (path.isEmpty()) {
            path = pSet.getZielPfad();
        }
        return path;
    }

    private void beenden() {
        DialogAddDownload.saveComboPfad(jComboBoxPath, orgPfad);
        dispose();
    }


    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanelExtra = new JPanel();
        var jLabel1 = new JLabel();
        jComboBoxPath = new JComboBox<>();
        jButtonDelPath = new JButton();
        jButtonPath = new JButton();
        var jPanel1 = new JPanel();
        chkInfo = new JCheckBox();
        jCheckBoxPfadSpeichern = new JCheckBox();
        chkSubtitle = new JCheckBox();
        chkStart = new JCheckBox();
        btnChange = new JButton();
        btnCancel = new JButton();
        btnOk = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Alle Downloads starten"); //NON-NLS
        setModal(true);
        setResizable(false);
        var contentPane = getContentPane();

        //======== jPanelExtra ========
        {
            jPanelExtra.setBorder(new EtchedBorder());

            //---- jLabel1 ----
            jLabel1.setText("Speicherpfad:"); //NON-NLS

            //---- jComboBoxPath ----
            jComboBoxPath.setEditable(true);
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[] {

            }));

            //---- jButtonDelPath ----
            jButtonDelPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); //NON-NLS
            jButtonDelPath.setToolTipText("History l\u00f6schen"); //NON-NLS

            //---- jButtonPath ----
            jButtonPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonPath.setToolTipText("Zielpfad ausw\u00e4hlen"); //NON-NLS

            //======== jPanel1 ========
            {
                jPanel1.setLayout(new VerticalLayout(5));

                //---- chkInfo ----
                chkInfo.setText("Infodatei anlegen: \"Filmname.txt\""); //NON-NLS
                jPanel1.add(chkInfo);

                //---- jCheckBoxPfadSpeichern ----
                jCheckBoxPfadSpeichern.setText("Zielpfad speichern"); //NON-NLS
                jPanel1.add(jCheckBoxPfadSpeichern);

                //---- chkSubtitle ----
                chkSubtitle.setText("Untertitel speichern: \"Filmname.xxx\""); //NON-NLS
                jPanel1.add(chkSubtitle);

                //---- chkStart ----
                chkStart.setText("Download sofort starten"); //NON-NLS
                jPanel1.add(chkStart);
            }

            GroupLayout jPanelExtraLayout = new GroupLayout(jPanelExtra);
            jPanelExtra.setLayout(jPanelExtraLayout);
            jPanelExtraLayout.setHorizontalGroup(
                jPanelExtraLayout.createParallelGroup()
                    .addGroup(jPanelExtraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel1)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelExtraLayout.createParallelGroup()
                            .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addComponent(jComboBoxPath, GroupLayout.PREFERRED_SIZE, 450, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonPath)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jButtonDelPath))
                            .addComponent(jPanel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addContainerGap())
            );
            jPanelExtraLayout.setVerticalGroup(
                jPanelExtraLayout.createParallelGroup()
                    .addGroup(jPanelExtraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanelExtraLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel1)
                            .addComponent(jComboBoxPath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonDelPath)
                            .addComponent(jButtonPath))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //---- btnChange ----
        btnChange.setText("Einzeln anlegen"); //NON-NLS
        btnChange.setToolTipText("Die Sammelaktion wird abgebrochen und Sie k\u00f6nnen im Anschlu\u00df die Einstellungen f\u00fcr jeden Download einzeln festlegen."); //NON-NLS

        //---- btnCancel ----
        btnCancel.setText("Abbrechen"); //NON-NLS

        //---- btnOk ----
        btnOk.setText("Speichern"); //NON-NLS

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jPanelExtra, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                            .addComponent(btnOk)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnCancel)
                            .addGap(18, 18, 18)
                            .addComponent(btnChange)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanelExtra, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(btnChange)
                        .addComponent(btnCancel)
                        .addComponent(btnOk))
                    .addContainerGap(2, Short.MAX_VALUE))
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JComboBox<String> jComboBoxPath;
    private JButton jButtonDelPath;
    private JButton jButtonPath;
    private JCheckBox chkInfo;
    private JCheckBox jCheckBoxPfadSpeichern;
    private JCheckBox chkSubtitle;
    private JCheckBox chkStart;
    private JButton btnChange;
    private JButton btnCancel;
    private JButton btnOk;
    // End of variables declaration//GEN-END:variables
}
