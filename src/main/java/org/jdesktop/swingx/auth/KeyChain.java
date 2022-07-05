/*
 * $Id: KeyChain.java 3100 2008-10-14 22:33:10Z rah003 $
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
package org.jdesktop.swingx.auth;

import javax.crypto.spec.SecretKeySpec;
import java.io.*;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * <b>KeyChain</b> is a class that implements the "KeyChain" concept.
 * Fundamentally, it allows you to store multiple keys/credentials 
 * in a central password store. Access to this central store is
 * controlled through a master password. This mechanism is used in
 * many popular client applications where you need to store credentials
 * for multiple servers/accounts. The actual store for the KeyStore
 * can be any OutputStream and it can work in the webstart sandbox
 * using Muffins.
 * </p>
 * <p>
 * To contstruct a <b>KeyChain</b>, you need to pass in an InputStream to the
 * store and it will initialize the KeyStore from the InputStream.
 * You can add and remove entries any time once you have an instance of
 * KeyChain. To persist the KeyChain and reflect any changes, you need to
 * call <b>store</b> method with an OutputStream.
 * </p>
 * 
 * @author Bino George
 */
public class KeyChain {
    private static final Logger LOG = Logger
            .getLogger(KeyChain.class.getName());
    
    private KeyStore store;

    private char[] masterPassword;

    /**
     * Creates an instance of KeyChain and initializes the store
     * from the InputStream.
     * 
     * @param masterPassword
     * @param inputStream
     * @throws IOException
     */
    public KeyChain(char[] masterPassword, InputStream inputStream)
            throws IOException {
        this.masterPassword = masterPassword;

        try {
            store = KeyStore.getInstance("JCEKS");
            store.load(inputStream, masterPassword);

        } catch (KeyStoreException ex) {
            LOG.log(Level.WARNING, "", ex);
        } catch (CertificateException ex) {
                        LOG.log(Level.WARNING, "", ex);
        } catch (NoSuchAlgorithmException ex) {
                        LOG.log(Level.WARNING, "", ex);
        } catch (EOFException ex) {
                        LOG.log(Level.WARNING, "", ex);
        }

    }

    /**
     * Fetches the password for a given account/user and server.
     * @param user
     * @param server
     * @return <code>null</code> if no password could be obtained, the password 
     *         otherwise
     */
    public String getPassword(String user, String server) {

        try {

            KeyStore.SecretKeyEntry entry2 = (KeyStore.SecretKeyEntry) store
                    .getEntry(user + "@" + server,
                            new KeyStore.PasswordProtection(masterPassword));
            return new String(entry2.getSecretKey().getEncoded());
        } catch (KeyStoreException ex) {
            LOG.log(Level.WARNING, "", ex);
        } catch (UnrecoverableEntryException ex) {
            LOG.log(Level.WARNING, "", ex);
        } catch (NoSuchAlgorithmException ex) {
            LOG.log(Level.WARNING, "", ex);
        }

        return null;
    }

    /**
     * Adds a password to the KeyChain for a given account/user and server.
     * 
     * @param user
     * @param server
     * @param password
     */
    public void addPassword(String user, String server, char[] password)
            {
        String pass = new String(password);
        SecretKeySpec passwordKey = new SecretKeySpec(pass.getBytes(), "JCEKS");
        KeyStore.SecretKeyEntry entry = new KeyStore.SecretKeyEntry(passwordKey);
        try {
            store.setEntry(user + "@" + server, entry,
                    new KeyStore.PasswordProtection(masterPassword));
        } catch (KeyStoreException e) {
            LOG.log(Level.WARNING, "", e);
        }
    }

    /**
     * Removes a password for a given account/user and server.
     * 
     * @param user
     * @param server
     */
    public void removePassword(String user, String server) {
        try {
            store.deleteEntry(user + "@" + server);
        } catch (KeyStoreException e) {
            LOG.log(Level.WARNING, "", e);
        }
    }

    /**
     * Persists the KeyChain to an OutputStream
     * 
     * @param ostream
     * @throws IOException
     */

    public void store(OutputStream ostream) throws IOException {
        try {
            store.store(ostream, masterPassword);
        } catch (KeyStoreException ex) {
                        LOG.log(Level.WARNING, "", ex);
        } catch (CertificateException ex) {
                        LOG.log(Level.WARNING, "", ex);
        } catch (NoSuchAlgorithmException ex) {
                        LOG.log(Level.WARNING, "", ex);
        }
    }


    public static void main(String[] args) {
        try {
            File file = new File("c:\\test.txt");
            FileInputStream fis;
            if (!file.exists()) {
                file.createNewFile();
                fis = null;
            } else {
                fis = new FileInputStream(file);
            }
            KeyChain kc = new KeyChain("test".toCharArray(), fis);
            kc.addPassword("bino", "sun-ds.sfbay", "test123".toCharArray());
            LOG.fine("pass = "
                    + kc.getPassword("bino", "sun-ds.sfbay"));

            LOG.fine("More testing :");
            for (int i = 0; i < 100; i++) {
                kc.addPassword("" + i, "sun-ds.sfbay", ("" + i).toCharArray());
            }
            for (int i = 0; i < 100; i++) {
                LOG.fine("key =" + i + " pass ="
                        + kc.getPassword("" + i, "sun-ds.sfbay"));
            }
            kc.store(new FileOutputStream(file));
        } catch (Exception e) {
            LOG.log(Level.WARNING, "", e);
        }
    }
    
}
