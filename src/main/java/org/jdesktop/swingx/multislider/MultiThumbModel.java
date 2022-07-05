/*
 * $Id: MultiThumbModel.java 3259 2009-02-17 21:06:19Z kschaefe $
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

package org.jdesktop.swingx.multislider;


import java.util.List;

/**
 *
 * @author joshy
 */
public interface MultiThumbModel<E> extends Iterable<Thumb<E>> {
    
    public float getMinimumValue();
    public void setMinimumValue(float minimumValue);
    public float getMaximumValue();
    public void setMaximumValue(float maximumValue);
    
    public int addThumb(float value, E obj);
    public void insertThumb(float value, E obj, int index);
    public void removeThumb(int index);
    public int getThumbCount();
    public Thumb<E> getThumbAt(int index);
    public int getThumbIndex(Thumb<E> thumb);
    public List<Thumb<E>> getSortedThumbs();
    public void thumbPositionChanged(Thumb<E> thumb);
    public void thumbValueChanged(Thumb<E> thumb);
    
    public void addThumbDataListener(ThumbDataListener listener);
    public void removeThumbDataListener(ThumbDataListener listener);
}
