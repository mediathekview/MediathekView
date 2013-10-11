/**
 * \cond LICENSE
 * ********************************************************************
 * This is a conditional block for preventing the DoxyGen documentation
 * tool to include this license header within the description of each
 * source code file. If you want to include this block, please define
 * the LICENSE parameter into the provided DoxyFile.
 * ********************************************************************
 *
 * JCarrierPigeon - A notification library
 * Copyright (c) 2010, Paulo Roberto Massa Cereda
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. Neither the name of the project's author nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ********************************************************************
 * End of the LICENSE conditional block
 * ********************************************************************
 * \endcond
 *
 * <b>NotificationQueue.java</b>: handles the display of notifications.
 * This class was created to act as a notification manager instead of
 * allowing multiple windows at once. Use this class together with
 * the new <b>net.sf.jcarrierpigeon.Notification</b> class.
 */

package net.sf.jcarrierpigeon;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.Queue;
import javax.swing.Timer;

/**
 * Handles the display of notifications. This class was created to act as a
 * notification manager instead of allowing multiple windows at once. I just
 * realized that too many notifications being displayed at once won't be
 * fully noticed by the user, so a queue system for notifications was a wiser
 * choice. From version 1.3 of JCarrierPigeon, multiple window support is
 * replaced by a notification queue system, where only a single notification is
 * shown per time.
 *
 * Please note that this notification manager handles the calls for methods
 * of the <b>net.sf.jcarrierpigeon.Notification</b> class. After adding a
 * notification to the queue, there is no need of calling any methods of the
 * notification itself. Check the following example:
 * @code
 * JWindow window = new JWindow();
 * Notification note = new Notification(window, WindowPosition.BOTTOMRIGHT, 25, 25, 1000);
 * NotificationQueue queue = new NotificationQueue();
 * queue.add(note);
 * @endcode
 * If there is no notifications in the queue, the notification is displayed
 * right after the call of the #add() method. Otherwise, it will be queued
 * until there is no notifications in display. Please note this is a simple
 * <i>first in first out</i> queue, so no priorities are estabilished when
 * adding notifications.
 * 
 * @author Paulo Roberto Massa Cereda
 * @version 1.3
 * @since 1.3
 */
public class NotificationQueue implements ActionListener {

    // a queue, a timer and a notification
    private Queue queue;
    private Timer timer;
    private Notification current;

    /**
     * Constructor method. Nothing new here, just instantiate
     * the local attributes.
     */
    public NotificationQueue() {
        queue = new LinkedList();
        timer = new Timer(50, this);
        current = null;
    }

    /**
     * Add the current notification to the queue system. If this is the
     * only notification in the queue, it will probably be shown right away.
     * Please check the following example:
     * @code
     * JWindow window = new JWindow();
     * Notification note = new Notification(window, WindowPosition.BOTTOMRIGHT, 25, 25, 1000);
     * NotificationQueue queue = new NotificationQueue();
     * queue.add(note);
     * @endcode
     * @param notification The <b>net.sf.jcarrierpigeon.Notification</b>
     * object.
     */
    public synchronized void add(Notification notification) {
        // check if queue is empty and there is no
        // current notification
        if (queue.isEmpty() && (current == null)) {

            // show notification
            current = notification;
            current.animate();
        }
        else {
            // there are other notifications, so we need to wait
            queue.offer(notification);

            // check if timer is not running
            if (timer.isRunning() == false) {

                // start the timer
                timer.start();
            }
        }
    }

    /**
     * Implements the <code>ActionListener</code> for our timer. It will trigger
     * notifications and process the queue.
     * @param e The event.
     */
    public void actionPerformed(ActionEvent e) {

        // there is a current notification going on
        if (current != null) {

            // check if queue is not empty and there is no
            // notification running
            if ((!queue.isEmpty()) && (!current.isRunning())) {

                // poll a notification from the queue
                current = (Notification) queue.poll();

                // animate
                current.animate();
            }
            else {

                // if the queue is empty
                if (queue.isEmpty()) {

                    // stop the timer
                    timer.stop();
                }
            }
        }
    }

}