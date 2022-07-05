package org.jdesktop.swingx.plaf;

import javax.swing.*;
import java.beans.PropertyChangeListener;
import java.util.Map;
import java.util.WeakHashMap;

@SuppressWarnings("nls")
public abstract class AbstractUIChangeHandler implements PropertyChangeListener {
	//prevent double installation.
	private final Map<JComponent, Boolean> installed = new WeakHashMap<JComponent, Boolean>();
	
	public void install(JComponent c){
		if(isInstalled(c)){
			return;
		}
		
		c.addPropertyChangeListener("UI", this);
		installed.put(c, Boolean.FALSE);
	}
	
	public boolean isInstalled(JComponent c) {
		return installed.containsKey(c);
	}

    public void uninstall(JComponent c){
		c.removePropertyChangeListener("UI", this);
		installed.remove(c);
	}
}