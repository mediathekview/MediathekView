/*
 * $Id: SimpleLoginService.java 3475 2009-08-28 08:30:47Z kleopatra $
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * An implementation of LoginService that simply matches
 * the username/password against a list of known users and their passwords.
 * This is useful for demos or prototypes where a proper login server is not available.
 *
 * <em>This Implementation is NOT secure. DO NOT USE this in a real application</em>
 * To make this implementation more secure, the passwords should be passed in and
 * stored as the result of a one way hash algorithm. That way an attacker cannot 
 * simply read the password in memory to crack into the system.
 *
 * @author rbair
 */
public final class SimpleLoginService extends LoginService {
    private Map<String,char[]> passwordMap;
    
    /**
     * Creates a new SimpleLoginService based on the given password map.
     */
    public SimpleLoginService(Map<String,char[]> passwordMap) {
        if (passwordMap == null) {
            passwordMap = new HashMap<String,char[]>();
        }
        this.passwordMap = passwordMap;
    }

    /**
     * Attempts to authenticate the given username and password against the password map
     */
    @Override
    public boolean authenticate(String name, char[] password, String server) throws Exception {
        char[] p = passwordMap.get(name);
        return Arrays.equals(password, p);
    }
}
