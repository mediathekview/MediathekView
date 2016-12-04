package net.sf.jtelegraph;

// imports include color features, window and image settings

import javax.swing.*;
import java.awt.*;
import java.net.URL;

/**
 * Provides a window class used for each message. This class extends
 * <code>javax.swing.JWindow</code> and inherits all methods from the
 * superclass.
 * <code>javax.swing.JWindow</code> was chosen since it
 * has no borders at all and makes no references of active windows in the
 * taskbar. There is no need of using objects of this class, since it has
 * no meaning by itself.
 * <code>net.sf.jtelegraph.TelegraphEnvelope</code>
 * is used by
 * <code>net.sf.jtelegraph.Telegraph</code>, setting title, body and
 * icon of the message. Besides, this class is also indirectly passed to the
 * <code>net.sf.jcarrierpigeon.Notification</code>, which will provide the
 * animation feature.
 *
 * @author Paulo Roberto Massa Cereda
 * @version 1.0
 * @since 1.0
 */
@SuppressWarnings("serial")
public class TelegraphEnvelope extends JWindow {
    // telegraph type
    private TelegraphType teleType;

    /**
     * Constructor method, creates a new window with a fixed size. Other
     * methods of this class will provide further features.
     */
    public TelegraphEnvelope() {

        // call this method to init components
        // NetBeans created this method structure, though I have some
        // restrictions on this procedure, I'll leave this for now, since
        // there is no harm
        initComponents();

    }

    /**
     * This method is called from within the constructor to initialize
     * the form. NetBeans forbides me to alter this code. Meh. By the
     * way, had to suppress some warnings for uncheck operations.
     */
    @SuppressWarnings("unchecked")
    private void initComponents() {

        // create the layout
        ContentPanel = new javax.swing.JPanel();
        telegraphIcon = new javax.swing.JLabel();
        telegraphText = new javax.swing.JLabel();

        // since this is an important message, I opted for marking it
        // as always on top. There's a reason for that: as modal windows
        // are cards off my deck, messages should be noted. So if they
        // appear behind another window, it won't help.
        setAlwaysOnTop(true);

        // layout stuff, panel
        ContentPanel.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2));
        ContentPanel.setAlignmentX(0.0F);
        ContentPanel.setAlignmentY(0.0F);

        // more layout stuff, now about the icon
        telegraphIcon.setText("Icon");
        telegraphIcon.setMaximumSize(new java.awt.Dimension(64, 64));
        telegraphIcon.setMinimumSize(new java.awt.Dimension(64, 64));
        telegraphIcon.setPreferredSize(new java.awt.Dimension(64, 64));

        // layout stuff reloaded, now about the text
        telegraphText.setText("Text");

        // stuff revolution, grouping all
        javax.swing.GroupLayout ContentPanelLayout = new javax.swing.GroupLayout(ContentPanel);
        ContentPanel.setLayout(ContentPanelLayout);
        ContentPanelLayout.setHorizontalGroup(
                ContentPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(ContentPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(telegraphIcon, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(telegraphText, javax.swing.GroupLayout.DEFAULT_SIZE, 252, Short.MAX_VALUE)
                .addContainerGap()));
        ContentPanelLayout.setVerticalGroup(
                ContentPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(ContentPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(ContentPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(telegraphText)
                .addComponent(telegraphIcon, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)));

        // accessibility
        telegraphIcon.getAccessibleContext().setAccessibleName("telegraphIcon");

        // grouping again
        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(ContentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE));
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(ContentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE));

        // now let's pack it!
        pack();
    }
    // window variables
    private javax.swing.JPanel ContentPanel;
    private javax.swing.JLabel telegraphIcon;
    private javax.swing.JLabel telegraphText;

    /**
     * Set the message title and body. This method is used by the
     * <code>net.sf.jtelegraph.Telegraph</code> class in order to define
     * the contents of the message to be displayed.
     *
     * @param title Title of the message.
     * @param message Body of the message. The message itself.
     */
    protected synchronized void setMessage(String title, String message) {

        // since I'd like to provide a word and line wrapping for the
        // <code>javax.swing.JLabel</code> class, I used this trick:
        // the text is inserted in between a HTML formatted code. If
        // you use a native UI for your Java applications, you may even use
        // HTML tags for your message!
        String labelText = String.format("<html><div style=\"width:%dpx;\"><b>%s</b><br/><br/>%s</div><html>", 180, title, message);

        // set the text
        telegraphText.setText(labelText);

    }

    /**
     * In case of a label being bigger than the actual window size, this method
     * will fix the height for us. This method probably will be replaced in
     * future versions, since I don't like it.
     */
    private void fixWindowHeight() {

        // get the window height
        int windowHeight = getHeight();

        // get the label height
        int textHeight = this.telegraphText.getHeight();

        // compare sizes
        if ((windowHeight - (30 * 2)) <= textHeight) {

            // increase the size
            this.setBounds(this.getX(), this.getY(), this.getWidth(), ((30 * 2) + textHeight));
        }
    }

    /**
     * Packs the current window, setting the proper icon and font color, and
     * possibly fixing a bad window.
     */
    protected void packTelegraph() {

        // pack the window
        this.pack();

        // fix the window height
        fixWindowHeight();

        // set the current message icon and font color
        setTelegraphIconAndColor(this.teleType);

    }

    /**
     * Set the message icon and font color for the current telegraph. You may
     * refer to the enumeration values from
     * <code>net.sf.jtelegraph.TelegraphEnvelope</code>.
     *
     * @param iconType The icon reference provided by the <code>net.sf.jtelegraph.TelegraphEnvelope</code>
     * enumeration.
     */
    private void setTelegraphIconAndColor(TelegraphType iconType) {

        // the icon name
        String iconName;

        // the font color
        Color iconColor;
        // check which icon was provided
        switch (iconType) {

            // mediathekView
            case MEDIATHEK_VIEW:
                iconName = "mediathekView.png";
                iconColor = new Color(50, 50, 120);
                break;

            // mediathekView
            case MEDIATHEK_VIEW_ERROR:
                iconName = "mediathekView_fehler.png";
                iconColor = new Color(50, 50, 120);
                break;

            // application
            case APPLICATION:

                // set the name
                iconName = "application.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // application warning
            case APPLICATION_WARNING:

                // set the name
                iconName = "application_warning.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // calculator
            case CALCULATOR:

                // set the name
                iconName = "calculator.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // calendar
            case CALENDAR:

                // set the name
                iconName = "calendar.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // camera
            case CAMERA:

                // set the name
                iconName = "camera.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // clock
            case CLOCK:

                // set the name
                iconName = "clock.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // coffee
            case COFFEE:

                // set the name
                iconName = "coffee.png";

                // set the color
                iconColor = new Color(102, 0, 0);

                break;

            // computer
            case COMPUTER:

                // set the name
                iconName = "computer.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // direction down
            case DIRECTION_DOWN:

                // set the name
                iconName = "direction_down.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // direction left
            case DIRECTION_LEFT:

                // set the name
                iconName = "direction_left.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // direction right
            case DIRECTION_RIGHT:

                // set the name
                iconName = "direction_right.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // direction up
            case DIRECTION_UP:

                // set the name
                iconName = "direction_up.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // disc
            case DISC:

                // set the name
                iconName = "disc.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // diskette
            case DISKETTE:

                // set the name
                iconName = "diskette.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // document
            case DOCUMENT:

                // set the name
                iconName = "document.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // document add
            case DOCUMENT_ADD:

                // set the name
                iconName = "document_add.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // document delete
            case DOCUMENT_DELETE:

                // set the name
                iconName = "document_delete.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // document edit
            case DOCUMENT_EDIT:

                // set the name
                iconName = "document_edit.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // document search
            case DOCUMENT_SEARCH:

                // set the name
                iconName = "document_search.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // document warning
            case DOCUMENT_WARNING:

                // set the name
                iconName = "document_warning.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // file
            case FILE:

                // set the name
                iconName = "file.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // file add
            case FILE_ADD:

                // set the name
                iconName = "file_add.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // file delete
            case FILE_DELETE:

                // set the name
                iconName = "file_delete.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // file edit
            case FILE_EDIT:

                // set the name
                iconName = "file_edit.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // file search
            case FILE_SEARCH:

                // set the name
                iconName = "file_search.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // file warning
            case FILE_WARNING:

                // set the name
                iconName = "file_warning.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // folder
            case FOLDER:

                // set the name
                iconName = "folder.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // folder add
            case FOLDER_ADD:

                // set the name
                iconName = "folder_add.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // folder delete
            case FOLDER_DELETE:

                // set the name
                iconName = "folder_delete.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // folder empty
            case FOLDER_EMPTY:

                // set the name
                iconName = "folder_empty.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // folder search
            case FOLDER_SEARCH:

                // set the name
                iconName = "folder_search.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // folder warning
            case FOLDER_WARNING:

                // set the name
                iconName = "folder_warning.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // home
            case HOME:

                // set the name
                iconName = "home.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // load download
            case LOAD_DOWNLOAD:

                // set the name
                iconName = "load_download.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // load upload
            case LOAD_UPLOAD:

                // set the name
                iconName = "load_upload.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // mail
            case MAIL:

                // set the name
                iconName = "mail.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail delete
            case MAIL_DELETE:

                // set the name
                iconName = "mail_delete.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail receive
            case MAIL_RECEIVE:

                // set the name
                iconName = "mail_receive.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail search
            case MAIL_SEARCH:

                // set the name
                iconName = "mail_search.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail send
            case MAIL_SEND:

                // set the name
                iconName = "mail_send.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail warning
            case MAIL_WARNING:

                // set the name
                iconName = "mail_warning.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // mail write
            case MAIL_WRITE:

                // set the name
                iconName = "mail_write.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // message
            case MESSAGE:

                // set the name
                iconName = "message.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // notification add
            case NOTIFICATION_ADD:

                // set the name
                iconName = "notification_add.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // notification done
            case NOTIFICATION_DONE:

                // set the name
                iconName = "notification_done.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // notification error
            case NOTIFICATION_ERROR:

                // set the name
                iconName = "notification_error.png";

                // set the color
                iconColor = new Color(153, 0, 0);

                break;

            // notification help
            case NOTIFICATION_HELP:

                // set the name
                iconName = "notification_help.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // notification info
            case NOTIFICATION_INFO:

                // set the name
                iconName = "notification_info.png";

                // set the color
                iconColor = new Color(102, 0, 0);

                break;

            // notification remove
            case NOTIFICATION_REMOVE:

                // set the name
                iconName = "notification_remove.png";

                // set the color
                iconColor = new Color(153, 0, 0);

                break;

            // notification warning
            case NOTIFICATION_WARNING:

                // set the name
                iconName = "notification_warning.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // piechart
            case PIECHART:

                // set the name
                iconName = "piechart.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // player fastforward
            case PLAYER_FASTFORWARD:

                // set the name
                iconName = "player_fastforward.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // player play
            case PLAYER_PLAY:

                // set the name
                iconName = "player_play.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // player rewind
            case PLAYER_REWIND:

                // set the name
                iconName = "player_rewind.png";

                // set the color
                iconColor = new Color(51, 102, 0);

                break;

            // player pause
            case PLAYER_PAUSE:

                // set the name
                iconName = "player_pause.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // player record
            case PLAYER_RECORD:

                // set the name
                iconName = "player_record.png";

                // set the color
                iconColor = new Color(153, 0, 0);

                break;

            // player stop
            case PLAYER_STOP:

                // set the name
                iconName = "player_stop.png";

                // set the color
                iconColor = new Color(153, 0, 0);

                break;

            // RSS
            case RSS:

                // set the name
                iconName = "rss.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // search
            case SEARCH:

                // set the name
                iconName = "search.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // security key
            case SECURITY_KEY:

                // set the name
                iconName = "security_key.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // security key and lock
            case SECURITY_KEYANDLOCK:

                // set the name
                iconName = "security_keyandlock.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // security lock
            case SECURITY_LOCK:

                // set the name
                iconName = "security_lock.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // security unlock
            case SECURITY_UNLOCK:

                // set the name
                iconName = "security_unlock.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // shopping cart
            case SHOPPINGCART:

                // set the name
                iconName = "shoppingcart.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // shopping cart add
            case SHOPPINGCART_ADD:

                // set the name
                iconName = "shoppingcart_add.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // shopping cart checkout
            case SHOPPINGCART_CHECKOUT:

                // set the name
                iconName = "shoppingcart_checkout.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // shopping cart remove
            case SHOPPINGCART_REMOVE:

                // set the name
                iconName = "shoppingcart_remove.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // shopping cart warning
            case SHOPPINGCART_WARNING:

                // set the name
                iconName = "shoppingcart_warning.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // star empty
            case STAR_EMPTY:

                // set the name
                iconName = "star_empty.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // star full
            case STAR_FULL:

                // set the name
                iconName = "star_full.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // star half
            case STAR_HALF:

                // set the name
                iconName = "star_half.png";

                // set the color
                iconColor = new Color(153, 153, 0);

                break;

            // user
            case USER:

                // set the name
                iconName = "user.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // user add
            case USER_ADD:

                // set the name
                iconName = "user_add.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // user delete
            case USER_DELETE:

                // set the name
                iconName = "user_delete.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // user manage
            case USER_MANAGE:

                // set the name
                iconName = "user_manage.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // user warning
            case USER_WARNING:

                // set the name
                iconName = "user_warning.png";

                // set the color
                iconColor = new Color(0, 51, 204);

                break;

            // volume
            case VOLUME:

                // set the name
                iconName = "volume.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // volume down
            case VOLUME_DOWN:

                // set the name
                iconName = "volume_down.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // volume mute
            case VOLUME_MUTE:

                // set the name
                iconName = "volume_mute.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // volume up
            case VOLUME_UP:

                // set the name
                iconName = "volume_up.png";

                // set the color
                iconColor = new Color(0, 0, 153);

                break;

            // default value, none of the above
            default:

                // set the name
                iconName = "notification_info.png";

                // set the color
                iconColor = new Color(102, 0, 0);
        }

        // so the image URL will be in the package inside this one
        URL imageURL = this.getClass().getResource("icons/" + iconName);

        // if an image is loaded
        if (imageURL != null) {

            // create image
            ImageIcon icon = new ImageIcon(imageURL);

            // set icon
            this.telegraphIcon.setIcon(icon);

            // create window border
            this.ContentPanel.setBorder(javax.swing.BorderFactory.createLineBorder(iconColor, 2));

            // add color to the text
            this.telegraphText.setForeground(iconColor);
        } else {

            // display a text
            this.telegraphIcon.setText("No icon");
        }
    }

    /**
     * Setter method for the window telegraph type.
     *
     * @param telegraph The telegraph type, that is, which icon and color
     * fonts will be used. This is an object of the <code>net.sf.jtelegraph.TelegraphType</code>
     * class.
     */
    protected void setTelegraphType(TelegraphType telegraph) {

        // set the value
        this.teleType = telegraph;
    }
}
