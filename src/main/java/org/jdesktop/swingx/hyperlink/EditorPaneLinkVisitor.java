/*
 * $Id: EditorPaneLinkVisitor.java 3927 2011-02-22 16:34:11Z kleopatra $
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
package org.jdesktop.swingx.hyperlink;

import org.jdesktop.swingx.JXEditorPane;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Document;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URL;


/**
 * A ActionListener using a JXEditorPane to "visit" a LinkModel.
 * 
 * adds an internal HyperlinkListener to visit links contained
 * in the document. 
 * 
 * @author Jeanette Winzenburg
 */
public class EditorPaneLinkVisitor implements ActionListener {
    private JXEditorPane editorPane;
    private HyperlinkListener hyperlinkListener;
    private LinkModel internalLink;
    
    public EditorPaneLinkVisitor() {
        this(null);
    }
    
    public EditorPaneLinkVisitor(JXEditorPane pane) {
        if (pane == null) {
            pane = createDefaultEditorPane();
        }
        this.editorPane = pane;
        pane.addHyperlinkListener(getHyperlinkListener());
    }
    

    public JXEditorPane getOutputComponent() {
        return editorPane;
    }
    
    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() instanceof LinkModel) {
            final LinkModel link = (LinkModel) e.getSource();
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    visit(link);

                }
            });
        }
   
    }

    public void visit(LinkModel link) {
        try {
            // make sure to reload
            editorPane.getDocument().putProperty(Document.StreamDescriptionProperty, null);
            // JW: editorPane defaults to asynchronous loading
            // no need to explicitly start a thread - really?
            editorPane.setPage(link.getURL());
            link.setVisited(true);
        } catch (IOException e1) {
            editorPane.setText("<html>Error 404: couldn't show " + link.getURL() + " </html>");
        }
    }

    protected JXEditorPane createDefaultEditorPane() {
        final JXEditorPane editorPane = new JXEditorPane();
        editorPane.setEditable(false);
        editorPane.setContentType("text/html");
        return editorPane;
    }

    protected HyperlinkListener getHyperlinkListener() {
        if (hyperlinkListener == null) {
            hyperlinkListener = createHyperlinkListener();
        }
        return hyperlinkListener;
    }

    protected HyperlinkListener createHyperlinkListener() {
        return new HyperlinkListener() {
            @Override
            public void hyperlinkUpdate(HyperlinkEvent e) {
                if (HyperlinkEvent.EventType.ACTIVATED == e.getEventType()) {
                    visitInternal(e.getURL());
                }
                
            }
            
        };
    }

    protected LinkModel getInternalLink() {
        if (internalLink == null) {
            internalLink = new LinkModel("internal");
        }
        return internalLink;
    }

    protected void visitInternal(URL url) {
        try {
            getInternalLink().setURL(url);
            visit(getInternalLink());
        } catch (Exception e) {
            // todo: error feedback
        }
    }


}
