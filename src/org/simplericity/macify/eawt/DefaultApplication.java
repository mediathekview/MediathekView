package org.simplericity.macify.eawt;

/*
 * Copyright 2007 Eirik Bjorsnos.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Java 6 code modifications:
 * Copyright 2013 CrystalPalace 
 */
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.imageio.ImageIO;

/**
 * Implements Application by calling the Mac OS X API through reflection.
 * If this class is used on a non-OS X platform the operations will have no effect or they will simulate
 * what the Apple API would do for those who manipulate state. ({@link #setEnabledAboutMenu(boolean)} etc.)
 */
@SuppressWarnings("unchecked")
public class DefaultApplication implements Application {

    private Object application;
    private Class applicationListenerClass;
    private Map<ApplicationListener,Object> listenerMap = Collections.synchronizedMap(new HashMap<ApplicationListener, Object>());
    private boolean enabledAboutMenu = true;
    private boolean enabledPreferencesMenu;
    private boolean aboutMenuItemPresent = true;
    private boolean preferencesMenuItemPresent;
   

    public DefaultApplication() {
        try {
            final File file = new File("/System/Library/Java");
            if (file.exists()) {
                ClassLoader scl = ClassLoader.getSystemClassLoader();
                Class clc = scl.getClass();
                if (URLClassLoader.class.isAssignableFrom(clc)) {
                    Method addUrl = URLClassLoader.class.getDeclaredMethod("addURL", new Class[]{URL.class});
                    addUrl.setAccessible(true);
                    addUrl.invoke(scl, file.toURI().toURL());
                }
            }

            Class appClass = Class.forName("com.apple.eawt.Application");
            application = appClass.getMethod("getApplication", new Class[0]).invoke(null);
            applicationListenerClass = Class.forName("com.apple.eawt.ApplicationListener");
        } catch (ClassNotFoundException e) {
            application = null;
        } catch (IllegalAccessException | NoSuchMethodException | InvocationTargetException | MalformedURLException e) {
            throw new RuntimeException(e);
        }

    }

    @Override
    public boolean isMac() {
        return application != null;
    }

    @Override
    public void addAboutMenuItem() {
        if (isMac()) {
            callMethod(application, "addAboutMenuItem");
        } else {
            this.aboutMenuItemPresent = true;
        }
    }

    /**
     * This will set a OS provided badge to the icon, typically a number.
     * @param displayableString The number to display as a string.
     */
    public void setDockIconBadge(String displayableString)
    {
        if (isMac())
        {
            try {
                Method setDockIconBadge = application.getClass().getMethod("setDockIconBadge", String.class);
                setDockIconBadge.invoke(application,displayableString);
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
        }
    }


    @Override
    public void addApplicationListener(ApplicationListener applicationListener) {
        if (!Modifier.isPublic(applicationListener.getClass().getModifiers())) {
            throw new IllegalArgumentException("ApplicationListener must be a public class");
        }
        if (isMac()) {
            Object listener = Proxy.newProxyInstance(getClass().getClassLoader(),
                    new Class[]{applicationListenerClass},
                    new ApplicationListenerInvocationHandler(applicationListener));

            callMethod(application, "addApplicationListener", new Class[]{applicationListenerClass}, new Object[]{listener});
            listenerMap.put(applicationListener, listener);
        } else {
            listenerMap.put(applicationListener, applicationListener);
        }
    }

    @Override
    public void addPreferencesMenuItem() {
        if (isMac()) {
            callMethod("addPreferencesMenuItem");
        } else {
            this.preferencesMenuItemPresent = true;
        }
    }

    @Override
    public boolean getEnabledAboutMenu() {
        if (isMac()) {
            return callMethod("getEnabledAboutMenu").equals(Boolean.TRUE);
        } else {
            return enabledAboutMenu;
        }
    }

    @Override
    public boolean getEnabledPreferencesMenu() {
        if (isMac()) {
            Object result = callMethod("getEnabledPreferencesMenu");
            return result.equals(Boolean.TRUE);
        } else {
            return enabledPreferencesMenu;
        }
    }

    @Override
    public Point getMouseLocationOnScreen() {
        if (isMac()) {
            try {
                Method method = application.getClass().getMethod("getMouseLocationOnScreen", new Class[0]);
              return (Point) method.invoke(null);
            } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        } else {
            return new Point(0, 0);
        }
    }

    @Override
    public boolean isAboutMenuItemPresent() {
        if (isMac()) {
            return callMethod("isAboutMenuItemPresent").equals(Boolean.TRUE);
        } else {
            return aboutMenuItemPresent;
        }
    }

    @Override
    public boolean isPreferencesMenuItemPresent() {
        if (isMac()) {
            return callMethod("isPreferencesMenuItemPresent").equals(Boolean.TRUE);
        } else {
            return this.preferencesMenuItemPresent;
        }
    }

    @Override
    public void removeAboutMenuItem() {
        if (isMac()) {
            callMethod("removeAboutMenuItem");
        } else {
            this.aboutMenuItemPresent = false;
        }
    }

    @Override
    public synchronized void removeApplicationListener(ApplicationListener applicationListener) {
        if (isMac()) {
            Object listener = listenerMap.get(applicationListener);
            callMethod(application, "removeApplicationListener", new Class[]{applicationListenerClass}, new Object[]{listener});

        }
        listenerMap.remove(applicationListener);
    }

    @Override
    public void removePreferencesMenuItem() {
        if (isMac()) {
            callMethod("removeAboutMenuItem");
        } else {
            this.preferencesMenuItemPresent = false;
        }
    }

    @Override
    public void setEnabledAboutMenu(boolean enabled) {
        if (isMac()) {
          callMethod(application, "setEnabledAboutMenu", new Class[]{Boolean.TYPE}, new Object[]{enabled});
        } else {
            this.enabledAboutMenu = enabled;
        }
    }

    @Override
    public void setEnabledPreferencesMenu(boolean enabled) {
        if (isMac()) {
            callMethod(application, "setEnabledPreferencesMenu", new Class[]{Boolean.TYPE}, new Object[]{enabled});
        } else {
            this.enabledPreferencesMenu = enabled;
        }

    }

    @Override
    public void enableSuddenTermination()
    {
      callMethod("enableSuddenTermination");
    }

    @Override
    public void requestUserAttention(final boolean critical)
    {
        if (isMac()) {
            try {
                Class[] paramBoolean = new Class[1];
                paramBoolean[0] = Boolean.TYPE;

                Method method = application.getClass().getDeclaredMethod("requestUserAttention", paramBoolean);
                method.invoke(application, critical);
            } catch (Exception ignored) {
            }
        }
    }

    private Object getNSApplication() throws ClassNotFoundException {
        try {
            Class applicationClass = Class.forName("com.apple.cocoa.application.NSApplication");
            return applicationClass.getMethod("sharedApplication", new Class[0]).invoke(null);
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setApplicationIconImage(BufferedImage image) {
        if (isMac()) {
            try {
                Method setDockIconImage = application.getClass().getMethod("setDockIconImage", Image.class);

                try {
                    setDockIconImage.invoke(application, image);
                } catch (IllegalAccessException | InvocationTargetException ignored) {
                }
            } catch (NoSuchMethodException mnfe) {


                ByteArrayOutputStream stream = new ByteArrayOutputStream();
                try {
                    ImageIO.write(image, "png", stream);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }

                try {
                    Class nsDataClass = Class.forName("com.apple.cocoa.foundation.NSData");
                    Constructor constructor = nsDataClass.getConstructor(new Class[]{byte[].class});

                    Object nsData = constructor.newInstance(new Object[]{stream.toByteArray()});

                    Class nsImageClass = Class.forName("com.apple.cocoa.application.NSImage");
                    Object nsImage = nsImageClass.getConstructor(new Class[]{nsDataClass}).newInstance(nsData);

                    Object application = getNSApplication();

                    application.getClass().getMethod("setApplicationIconImage", new Class[]{nsImageClass}).invoke(application, nsImage);

                } catch (ClassNotFoundException ignored) {
                } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e) {
                    throw new RuntimeException(e);
                }

            }

        }
    }

    @Override
    public BufferedImage getApplicationIconImage() {
        if (isMac()) {

            try {
                Method getDockIconImage = application.getClass().getMethod("getDockIconImage");
                try {
                    return (BufferedImage) getDockIconImage.invoke(application);
                } catch (IllegalAccessException | InvocationTargetException ignored) {
                }
            } catch (NoSuchMethodException nsme) {

                try {
                    Class nsDataClass = Class.forName("com.apple.cocoa.foundation.NSData");
                    Class nsImageClass = Class.forName("com.apple.cocoa.application.NSImage");
                    Object application = getNSApplication();
                    Object nsImage = application.getClass().getMethod("applicationIconImage", new Class[0]).invoke(application, new Object[0]);

                    Object nsData = nsImageClass.getMethod("TIFFRepresentation", new Class[0]).invoke(nsImage);

                    Integer length = (Integer) nsDataClass.getMethod("length", new Class[0]).invoke(nsData);
                    byte[] bytes = (byte[]) nsDataClass.getMethod("bytes", new Class[]{Integer.TYPE, Integer.TYPE}).invoke(nsData, 0, length);

                    return ImageIO.read(new ByteArrayInputStream(bytes));

                } catch (ClassNotFoundException e) {
                    e.printStackTrace();                } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | IOException e) {
                    throw new RuntimeException(e);
                }
            }

        }

        return null;
    }

    private Object callMethod(String methodname) {
        return callMethod(application, methodname, new Class[0], new Object[0]);
    }

    private Object callMethod(Object object, String methodname) {
        return callMethod(object, methodname, new Class[0], new Object[0]);
    }

    private Object callMethod(Object object, String methodname, Class[] classes, Object[] arguments) {
        try {
            if (classes == null) {
                classes = new Class[arguments.length];
                for (int i = 0; i < classes.length; i++) {
                    classes[i] = arguments[i].getClass();

                }
            }
            Method addListnerMethod = object.getClass().getMethod(methodname, classes);
            return addListnerMethod.invoke(object, arguments);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    class ApplicationListenerInvocationHandler implements InvocationHandler {

        private ApplicationListener applicationListener;

        ApplicationListenerInvocationHandler(ApplicationListener applicationListener) {
            this.applicationListener = applicationListener;
        }

        @Override
        public Object invoke(Object object, Method appleMethod, Object[] objects) throws Throwable {

            ApplicationEvent event = createApplicationEvent(objects[0]);
            try {
                Method method = applicationListener.getClass().getMethod(appleMethod.getName(), new Class[]{ApplicationEvent.class});
                return method.invoke(applicationListener, event);
            } catch (NoSuchMethodException e) {
                if (appleMethod.getName().equals("equals") && objects.length == 1) {
                    return object == objects[0];
                }
                return null;
            }
        }
    }

    private ApplicationEvent createApplicationEvent(final Object appleApplicationEvent) {
        return (ApplicationEvent) Proxy.newProxyInstance(getClass().getClassLoader(), new Class[]{ApplicationEvent.class}, new InvocationHandler() {
            @Override
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                return appleApplicationEvent.getClass().getMethod(method.getName(), method.getParameterTypes()).invoke(appleApplicationEvent, objects);
            }
        });
    }
}