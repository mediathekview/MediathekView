/**
 * \cond LICENSE
 * ********************************************************************
 * This is a conditional block for preventing the DoxyGen documentation
 * tool to include this license header within the description of each
 * source code file. If you want to include this block, please define
 * the LICENSE parameter into the provided DoxyFile.
 * ********************************************************************
 *
 * JTelegraph - A message notification library
 * Copyright (c) 2011, Paulo Roberto Massa Cereda
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
 * <b>Telegraph.java</b>: handles the display of message notifications.
 * This class was created to act as a notification manager. Use this class
 * together with the <code>net.sf.jtelegraph.Telegraph</code> class.
 */

package net.sf.jtelegraph;

// all needed imports
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.Queue;
import javax.swing.Timer;

/**
 * Handles the display of message notifications. This class was created to
 * act as a message notification manager. Please note that this message
 * notification manager handles the calls for methods of the
 * <code>net.sf.jtelegraph.Telegraph</code> class. After adding a message
 * notification to the queue, there is no need of calling any methods of the
 * message notification itself. Check the following example:
 * @code
 * Telegraph telegraph = new Telegraph("Connection status", "The DB connection was established successfully", TelegraphType.NOTIFICATION_DONE, WindowPosition.BOTTOMLEFT, 4000);
 * TelegraphQueue queue = new TelegraphQueue();
 * queue.add(telegraph);
 * @endcode
 * If there is no notifications in the queue, the message notification is displayed
 * right after the call of the #add() method. Otherwise, it will be queued
 * until there is no notifications in display. Please note this is a simple
 * <i>first in first out</i> queue, so no priorities are estabilished when
 * adding notifications.
 *
 * @author Paulo Roberto Massa Cereda
 * @version 1.0
 * @since 1.0
 */
@SuppressWarnings("unchecked")
public class TelegraphQueue implements ActionListener {

    // a queue, a timer and a notification
    private Queue queue;
    private Timer timer;
    private Telegraph current;

    /**
     * Constructor method. Nothing new here, just instantiate
     * the local attributes.
     */
    public TelegraphQueue() {
        queue = new LinkedList();
        timer = new Timer(50, this);
        current = null;
    }

    /**
     * Add the current message notification to the queue system. If this is the
     * only notification in the queue, it will probably be shown right away.
     * Please check the following example:
     * @code
     * Telegraph telegraph = new Telegraph("Connection status", "The DB connection was established successfully", TelegraphType.NOTIFICATION_DONE, WindowPosition.BOTTOMLEFT, 4000);
     * TelegraphQueue queue = new TelegraphQueue();
     * queue.add(telegraph);
     * @endcode
     * @param telegraph The telegraph itself. It must be an object of the
     * <code>net.sf.jtelegraph.Telegraph</code> class.
     */
    public synchronized void add(Telegraph telegraph) {

        // check if queue is empty and there is no
        // current notification
        if (queue.isEmpty() && (current == null)) {

            // show notification
            current = telegraph;
            current.animate();
        }
        else {

            // there are other notifications, so we need to wait
            queue.offer(telegraph);

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
                current = (Telegraph) queue.poll();

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