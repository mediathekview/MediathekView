/**
 * 
 */
package org.jdesktop.swingx.autocomplete;

import javax.swing.event.DocumentEvent;
import javax.swing.text.Document;
import javax.swing.text.Element;

/**
 * @author Karl George Schaefer
 *
 */
final class DelegatingDocumentEvent implements DocumentEvent {
    private final Document resourcedDocument;
    private final DocumentEvent sourceEvent;
    
    public DelegatingDocumentEvent(Document resourcedDocument, DocumentEvent sourceEvent) {
        this.resourcedDocument = resourcedDocument;
        this.sourceEvent = sourceEvent;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public ElementChange getChange(Element elem) {
        return sourceEvent.getChange(elem);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Document getDocument() {
        return resourcedDocument;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLength() {
        return sourceEvent.getLength();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getOffset() {
        return sourceEvent.getOffset();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventType getType() {
        return sourceEvent.getType();
    }

}
