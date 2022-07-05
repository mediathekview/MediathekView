/*
 * $Id: JAASLoginService.java 4147 2012-02-01 17:13:24Z kschaefe $
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

import org.jdesktop.beans.JavaBean;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * <b>JAASLoginService</b> implements a <b>LoginService</b>
 * that uses JAAS for authentication. <b>JAASLoginService</b> uses the 
 * server name as name of the configuration for JAAS.
 * 
 * @author Bino George
 */
@JavaBean
public class JAASLoginService extends LoginService {
    private static final Logger LOG = Logger.getLogger(JAASLoginService.class
            .getName());

	protected LoginContext loginContext;

    /**
     * Constructor for <b>JAASLoginService</b>
     * @param server server name that is also used for the JAAS config name
     */
    public JAASLoginService(String server) {
        super(server);
    }
    
        /**
         * Default JavaBeans constructor
         */
        public JAASLoginService() {
            super();
        }
        
    
    /**
     * @inheritDoc
     *      
     */
    @Override
    public boolean authenticate(String name, char[] password, String server) throws Exception {
		// If user has selected a different server, update the login service
		if (server != null) {
			if (!server.equals(getServer())) {
				setServer(server);
			}
		}
		// Clear the login context before attempting authentication
		loginContext = null;
		// Create a login context for the appropriate server and attempt to
		// authenticate the user.
        try {
            loginContext = new LoginContext(getServer(),
                    new JAASCallbackHandler(name, password));
            loginContext.login();
            return true;
        } catch (AccountExpiredException e) {
            // TODO add explanation?
            LOG.log(Level.WARNING, "", e);
            return false;
        } catch (CredentialExpiredException e) {
                        // TODO add explanation?
                        LOG.log(Level.WARNING, "", e);
            return false;
        } catch (FailedLoginException e) {
                        // TODO add explanation?
                        LOG.log(Level.WARNING, "", e);
            return false;
        } catch (LoginException e) {
                        // TODO add explanation?
                        LOG.log(Level.WARNING, "", e);
            return false;
        } catch (Throwable e) {
                        // TODO add explanation?
                        LOG.log(Level.WARNING, "", e);
            return false;
        }
    }

	/**
	 * Returns the <code>LoginContext</code> used during the authentication
	 * process.
	 */
	public LoginContext getLoginContext()
	{
		return loginContext;
	}

	/**
	 * Returns the <code>Subject</code> representing the authenticated 
	 * individual, or <code>null</code> if the user has not yet been 
	 * successfully authenticated.
	 */
	public Subject getSubject()
	{
		if (loginContext == null)
			return null;
		return loginContext.getSubject();
	}

    class JAASCallbackHandler implements CallbackHandler {

        private String name;

        private char[] password;

        public JAASCallbackHandler(String name, char[] passwd) {
            this.name = name;
            this.password = passwd;
        }

        public void handle(Callback[] callbacks) throws java.io.IOException {
            for (int i = 0; i < callbacks.length; i++) {
                if (callbacks[i] instanceof NameCallback) {
                    NameCallback cb = (NameCallback) callbacks[i];
                    cb.setName(name);
                } else if (callbacks[i] instanceof PasswordCallback) {
                    PasswordCallback cb = (PasswordCallback) callbacks[i];
                    cb.setPassword(password);
                }
            }
        }

    }
}
