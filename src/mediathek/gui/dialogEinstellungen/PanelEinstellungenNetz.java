/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialogEinstellungen;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;

public class PanelEinstellungenNetz extends PanelVorlage {

    public PanelEinstellungenNetz(DDaten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        ddaten = d;
        //proxy
        jCheckBoxProxy.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_HTTP_PROXY_ON_NR]));
        jCheckBoxProxy.addActionListener(new BeobProxy());
        jTextFieldPServer.setText(Daten.system[Konstanten.SYSTEM_HTTP_PROXY_SERVER_NR]);
        jTextFieldPPort.setText(Daten.system[Konstanten.SYSTEM_HTTP_PROXY_PORT_NR]);
        jTextFieldPUser.setText(Daten.system[Konstanten.SYSTEM_HTTP_PROXY_USER_NR]);
        jTextFieldPPwd.setText(Daten.system[Konstanten.SYSTEM_HTTP_PROXY_PWD_NR]);
        setProxy();
        jTextFieldPPort.getDocument().addDocumentListener(new BeobDocProxy());
        jTextFieldPPwd.getDocument().addDocumentListener(new BeobDocProxy());
        jTextFieldPServer.getDocument().addDocumentListener(new BeobDocProxy());
        jTextFieldPUser.getDocument().addDocumentListener(new BeobDocProxy());
        //
    }

    private void setProxy() {
        Daten.system[Konstanten.SYSTEM_HTTP_PROXY_ON_NR] = Boolean.toString(jCheckBoxProxy.isSelected());
        //textfelder setzen
        jTextFieldPPort.setEnabled(jCheckBoxProxy.isSelected());
        jTextFieldPPort.setEditable(jCheckBoxProxy.isSelected());
        jTextFieldPPwd.setEnabled(jCheckBoxProxy.isSelected());
        jTextFieldPPwd.setEditable(jCheckBoxProxy.isSelected());
        jTextFieldPServer.setEnabled(jCheckBoxProxy.isSelected());
        jTextFieldPServer.setEditable(jCheckBoxProxy.isSelected());
        jTextFieldPUser.setEnabled(jCheckBoxProxy.isSelected());
        jTextFieldPUser.setEditable(jCheckBoxProxy.isSelected());
        jLabelPass.setEnabled(jCheckBoxProxy.isSelected());
        jLabelPort.setEnabled(jCheckBoxProxy.isSelected());
        jLabelServer.setEnabled(jCheckBoxProxy.isSelected());
        jLabelUser.setEnabled(jCheckBoxProxy.isSelected());
        //auswerten
        Daten.system[Konstanten.SYSTEM_HTTP_PROXY_SERVER_NR] = jTextFieldPServer.getText();
        Daten.system[Konstanten.SYSTEM_HTTP_PROXY_PORT_NR] = jTextFieldPPort.getText();
        Daten.system[Konstanten.SYSTEM_HTTP_PROXY_USER_NR] = jTextFieldPUser.getText();
        Daten.system[Konstanten.SYSTEM_HTTP_PROXY_PWD_NR] = jTextFieldPPwd.getText();
        //system setzen
        GuiFunktionen.setProxy();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jCheckBoxProxy = new javax.swing.JCheckBox();
        jLabelServer = new javax.swing.JLabel();
        jLabelPort = new javax.swing.JLabel();
        jTextFieldPServer = new javax.swing.JTextField();
        jTextFieldPPort = new javax.swing.JTextField();
        jLabelUser = new javax.swing.JLabel();
        jLabelPass = new javax.swing.JLabel();
        jTextFieldPUser = new javax.swing.JTextField();
        jTextFieldPPwd = new javax.swing.JTextField();

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("HTTP-Proxy"));

        jCheckBoxProxy.setText("Proxy verwenden:");

        jLabelServer.setText("Server:");

        jLabelPort.setText("Port:");

        jLabelUser.setText("Benutzer:");

        jLabelPass.setText("Passwort:");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(19, 19, 19)
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabelPort)
                                    .addComponent(jLabelServer)))
                            .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addComponent(jLabelUser)
                                .addComponent(jLabelPass)))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldPServer, javax.swing.GroupLayout.DEFAULT_SIZE, 165, Short.MAX_VALUE)
                            .addComponent(jTextFieldPPort)
                            .addComponent(jTextFieldPUser)
                            .addComponent(jTextFieldPPwd)))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jCheckBoxProxy)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxProxy)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelServer)
                    .addComponent(jTextFieldPServer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldPPort, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelPort))
                .addGap(18, 18, 18)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldPUser, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelUser))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelPass)
                    .addComponent(jTextFieldPPwd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jTextFieldPPort, jTextFieldPPwd, jTextFieldPServer, jTextFieldPUser});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(53, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxProxy;
    private javax.swing.JLabel jLabelPass;
    private javax.swing.JLabel jLabelPort;
    private javax.swing.JLabel jLabelServer;
    private javax.swing.JLabel jLabelUser;
    private javax.swing.JTextField jTextFieldPPort;
    private javax.swing.JTextField jTextFieldPPwd;
    private javax.swing.JTextField jTextFieldPServer;
    private javax.swing.JTextField jTextFieldPUser;
    // End of variables declaration//GEN-END:variables

    private class BeobDocProxy implements DocumentListener {

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
            setProxy();
        }
    }

    private class BeobProxy implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setProxy();
        }
    }
}
