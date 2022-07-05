/*
 * $Id: Contract.java 4028 2011-06-03 19:32:19Z kschaefe $
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.util;


/**
 * Utility class for checking contracts.
 * 
 * @author Jeanette Winzenburg
 */
public class Contract {

    private Contract() {
        
    }

    /**
     * Tests the input parameter against null. If the input is 
     * an array, checks all of its elements as well. Returns the 
     * unchanged parameter if not null, throws a NullPointerException
     * otherwise. <p>
     * 
     * PENDING: type of exception? there are raging debates, some
     *   favour an IllegalArgument? <p>
     *   
     * PENDING: the implementation uses a unchecked type cast to an array.
     *   can we do better, how?
     *     
     * 
     * @param <T> the type of the input parameter
     * @param input the argument to check against null.
     * @param message the text of the exception if the argument is null
     * @return the input if not null
     * @throws NullPointerException if input is null
     */
    @SuppressWarnings("unchecked")
    public static <T> T asNotNull(T input, String message) {
        if (input == null) 
            throw new NullPointerException(message);
        
        if (input.getClass().isArray()) {
            if (!input.getClass().getComponentType().isPrimitive()) {
                T[] array = (T[]) input;
                for (int i = 0; i < array.length; i++) {
                    asNotNull(array[i], message);
                }
            }
        }
        
        return input;
    }
}
