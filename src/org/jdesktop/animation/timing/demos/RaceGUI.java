/**
 * Copyright (c) 2006, Sun Microsystems, Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *   * Neither the name of the TimingFramework project nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.jdesktop.animation.timing.demos;

import java.awt.BorderLayout;
import javax.swing.JFrame;
import javax.swing.UIManager;

/**
 * The GUI used by all of the different race demos.
 * It contains a control panel (for the Go/Stop buttons) and a
 * TrackView (where the race is rendered)
 *
 * @author Chet
 */
public class RaceGUI {

    private TrackView track;
    private RaceControlPanel controlPanel;

    /**
     * Creates a new instance of RaceGUI
     */
    public RaceGUI(String appName) {
        UIManager.put("swing.boldMetal", Boolean.FALSE);
        JFrame f = new JFrame(appName);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setLayout(new BorderLayout());
        
        // Add Track view
        track = new TrackView();
        f.add(track, BorderLayout.CENTER);
        
        // Add control panel
        controlPanel = new RaceControlPanel();
        f.add(controlPanel, BorderLayout.SOUTH);
        
        f.pack();
        f.setVisible(true);
    }
    
    public TrackView getTrack() {
        return track;
    }
    
    public RaceControlPanel getControlPanel() {
        return controlPanel;
    }
}
