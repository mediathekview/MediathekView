/*
 * $Id: LoginService.java 3661 2010-04-13 13:19:47Z kleopatra $
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

import org.jdesktop.beans.AbstractBean;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.awt.*;
import java.util.logging.Logger;

/**
 * <b>LoginService</b> is the abstract base class for all classes implementing
 * a login mechanism. It allows you to customize the threading behaviour used to
 * perform the login. Subclasses need to override the <b>authenticate</b>
 * method. Subclasses may implement the getUserRoles() method to return a
 * meaningful value this method will be called once upon a successful login to
 * determine the user roles. It is not defined as abstract to simplify the task
 * of implementing a login service for those who do not require this
 * functionality.
 * 
 * @author Bino George
 * @author Shai Almog
 * @author Karl Schaefer
 */
public abstract class LoginService extends AbstractBean {
    @SuppressWarnings("unused")
    private Logger LOG = Logger.getLogger(LoginService.class.getName());

    private EventListenerList listenerList = new EventListenerList();

    private SwingWorker<Boolean, Void> loginWorker;

    /*
     * Controls the authentication behaviour to be either synchronous or
     * asynchronous
     */
    private boolean synchronous;

    private String server;

    public LoginService() {
    }

    public LoginService(String server) {
        setServer(server);
    }

    /**
     * This method is intended to be implemented by clients wishing to
     * authenticate a user with a given password. Clients should implement the
     * authentication in a manner that the authentication can be cancelled at
     * any time.
     * 
     * @param name
     *            username
     * @param password
     *            password
     * @param server
     *            server (optional)
     * 
     * @return <code>true</code> on authentication success
     * @throws Exception
     */
    public abstract boolean authenticate(String name, char[] password,
            String server) throws Exception;

    /**
     * Called immediately after a successful authentication. This method should
     * return an array of user roles or null if role based permissions are not
     * used.
     * 
     * @return per default <code>null</code>
     */
    public String[] getUserRoles() {
        return null;
    }

    /**
     * Notifies the LoginService that an already running authentication request
     * should be cancelled. This method is intended to be used by clients who
     * want to provide user with control over cancelling a long running
     * authentication request.
     */
    public void cancelAuthentication() {
        if (loginWorker != null) {
            loginWorker.cancel(true);
        }
    }

    /**
     * This method starts the authentication process and is either synchronous
     * or asynchronous based on the synchronous property
     * 
     * @param user
     *            user
     * @param password
     *            password
     * @param server
     *            server
     * @throws Exception
     */
    public void startAuthentication(final String user, final char[] password,
            final String server) throws Exception {
        if (getSynchronous()) {
            try {
                if (authenticate(user, password, server)) {
                    fireLoginSucceeded(new LoginEvent(this));
                } else {
                    fireLoginFailed(new LoginEvent(this));
                }
            } catch (Throwable e) {
                fireLoginFailed(new LoginEvent(this, e));
            }
        } else {
            loginWorker = new SwingWorker<Boolean, Void>() {
                @Override
                protected Boolean doInBackground() throws Exception {
                    try {
                        final boolean result = authenticate(user, password,
                                server);
                        if (isCancelled()) {
                            EventQueue.invokeLater(new Runnable() {
                                public void run() {
                                    fireLoginCanceled(new LoginEvent(this));
                                }
                            });
                            return false;
                        }
                        EventQueue.invokeLater(new Runnable() {
                            public void run() {
                                if (result) {
                                    fireLoginSucceeded(new LoginEvent(
                                            LoginService.this));
                                } else {
                                    fireLoginFailed(new LoginEvent(
                                            LoginService.this));
                                }
                            }
                        });
                        return result;
                    } catch (final Throwable failed) {
                        if (!isCancelled()) {
                            SwingUtilities.invokeLater(new Runnable() {
                                public void run() {
                                    fireLoginFailed(new LoginEvent(
                                            LoginService.this, failed));
                                }
                            });
                        } else {
                            EventQueue.invokeLater(new Runnable() {
                                public void run() {
                                    fireLoginCanceled(new LoginEvent(this));
                                }
                            });
                        }
                        return false;
                    }
                }
            };
            loginWorker.execute();
            fireLoginStarted(new LoginEvent(this));
        }
    }

    /**
     * Get the synchronous property
     * 
     * @return the synchronous property
     */
    public boolean getSynchronous() {
        return synchronous;
    }

    /**
     * Sets the synchronous property
     * 
     * @param synchronous
     *            synchronous property
     */
    public void setSynchronous(boolean synchronous) {
        boolean old = getSynchronous();
        this.synchronous = synchronous;
        firePropertyChange("synchronous", old, getSynchronous());
    }

    /**
     * Adds a <strong>LoginListener</strong> to the list of listeners
     * 
     * @param listener
     *            listener
     */

    public void addLoginListener(LoginListener listener) {
        listenerList.add(LoginListener.class, listener);
    }

    /**
     * Removes a <strong>LoginListener</strong> from the list of listeners
     * 
     * @param listener
     *            listener
     */
    public void removeLoginListener(LoginListener listener) {
        listenerList.remove(LoginListener.class, listener);
    }

    void fireLoginStarted(final LoginEvent source) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i] == LoginListener.class) {
                ((LoginListener) listeners[i+1]).loginStarted(source);
            }
        }
    }

    void fireLoginSucceeded(final LoginEvent source) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i] == LoginListener.class) {
                ((LoginListener) listeners[i+1]).loginSucceeded(source);
            }
        }
    }

    void fireLoginFailed(final LoginEvent source) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i] == LoginListener.class) {
                ((LoginListener) listeners[i+1]).loginFailed(source);
            }
        }
    }

    void fireLoginCanceled(final LoginEvent source) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i] == LoginListener.class) {
                ((LoginListener) listeners[i+1]).loginCanceled(source);
            }
        }
    }

    /**
     * @return Returns the server.
     */
    public String getServer() {
        return server;
    }

    /**
     * @param server
     *            The server to set.
     */
    public void setServer(String server) {
        String old = getServer();
        this.server = server;
        firePropertyChange("server", old, getServer());
    }
}
