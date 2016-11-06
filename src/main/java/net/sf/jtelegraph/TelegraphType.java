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
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 *
 * 3. Neither the name of the project's author nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
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
 * <b>TelegraphType.java</b>: provides an enumeration for icon types. All
 * telegraphs require an icon, so you may choose one amongst this set. This
 * enumeration is used to set the parameters referring the message icons. All
 * icons provided here are from the <i>Simplicio IconSet</i> by <i>Neurovit</i>,
 * distributed under the <i>Creative Commons (Attribution-ShareAlike 3.0 Unported)
 * (CC BY-SA 3.0)</i>. Make sure to check both licenses before proceeding. You may
 * also replace the current icons by your own set.
 *
 */
package net.sf.jtelegraph;

/**
 * Provides an enumeration for icon types. It basically references values
 * for setting icons for messages.
 *
 * @author Paulo Roberto Massa Cereda
 * @version 1.0
 * @since 1.0
 */
public enum TelegraphType {

    /**
     * \image html mediathekView.png "Icon preview"
     */
    MEDIATHEK_VIEW,
    /**
     * \image html mediathekView_fehler.png "Icon preview"
     */
    MEDIATHEK_VIEW_ERROR,
    /**
     * \image html application.png "Icon preview"
     */
    APPLICATION,
    /**
     * \image html application_warning.png "Icon preview"
     */
    APPLICATION_WARNING,
    /**
     * \image html calculator.png "Icon preview"
     */
    CALCULATOR,
    /**
     * \image html calendar.png "Icon preview"
     */
    CALENDAR,
    /**
     * \image html camera.png "Icon preview"
     */
    CAMERA,
    /**
     * \image html clock.png "Icon preview"
     */
    CLOCK,
    /**
     * \image html coffee.png "Icon preview"
     */
    COFFEE,
    /**
     * \image html computer.png "Icon preview"
     */
    COMPUTER,
    /**
     * \image html direction_down.png "Icon preview"
     */
    DIRECTION_DOWN,
    /**
     * \image html direction_left.png "Icon preview"
     */
    DIRECTION_LEFT,
    /**
     * \image html direction_right.png "Icon preview"
     */
    DIRECTION_RIGHT,
    /**
     * \image html direction_up.png "Icon preview"
     */
    DIRECTION_UP,
    /**
     * \image html disc.png "Icon preview"
     */
    DISC,
    /**
     * \image html diskette.png "Icon preview"
     */
    DISKETTE,
    /**
     * \image html document.png "Icon preview"
     */
    DOCUMENT,
    /**
     * \image html document_add.png "Icon preview"
     */
    DOCUMENT_ADD,
    /**
     * \image html document_delete.png "Icon preview"
     */
    DOCUMENT_DELETE,
    /**
     * \image html document_edit.png "Icon preview"
     */
    DOCUMENT_EDIT,
    /**
     * \image html document_search.png "Icon preview"
     */
    DOCUMENT_SEARCH,
    /**
     * \image html document_warning.png "Icon preview"
     */
    DOCUMENT_WARNING,
    /**
     * \image html file.png "Icon preview"
     */
    FILE,
    /**
     * \image html file_add.png "Icon preview"
     */
    FILE_ADD,
    /**
     * \image html file_delete.png "Icon preview"
     */
    FILE_DELETE,
    /**
     * \image html file_edit.png "Icon preview"
     */
    FILE_EDIT,
    /**
     * \image html file_search.png "Icon preview"
     */
    FILE_SEARCH,
    /**
     * \image html file_warning.png "Icon preview"
     */
    FILE_WARNING,
    /**
     * \image html folder.png "Icon preview"
     */
    FOLDER,
    /**
     * \image html folder_search.png "Icon preview"
     */
    FOLDER_SEARCH,
    /**
     * \image html folder_warning.png "Icon preview"
     */
    FOLDER_WARNING,
    /**
     * \image html folder_add.png "Icon preview"
     */
    FOLDER_ADD,
    /**
     * \image html folder_delete.png "Icon preview"
     */
    FOLDER_DELETE,
    /**
     * \image html folder_empty.png "Icon preview"
     */
    FOLDER_EMPTY,
    /**
     * \image html home.png "Icon preview"
     */
    HOME,
    /**
     * \image html load_download.png "Icon preview"
     */
    LOAD_DOWNLOAD,
    /**
     * \image html load_upload.png "Icon preview"
     */
    LOAD_UPLOAD,
    /**
     * \image html mail.png "Icon preview"
     */
    MAIL,
    /**
     * \image html mail_delete.png "Icon preview"
     */
    MAIL_DELETE,
    /**
     * \image html mail_receive.png "Icon preview"
     */
    MAIL_RECEIVE,
    /**
     * \image html mail_search.png "Icon preview"
     */
    MAIL_SEARCH,
    /**
     * \image html mail_send.png "Icon preview"
     */
    MAIL_SEND,
    /**
     * \image html mail_warning.png "Icon preview"
     */
    MAIL_WARNING,
    /**
     * \image html mail_write.png "Icon preview"
     */
    MAIL_WRITE,
    /**
     * \image html message.png "Icon preview"
     */
    MESSAGE,
    /**
     * \image html notification_add.png "Icon preview"
     */
    NOTIFICATION_ADD,
    /**
     * \image html notification_done.png "Icon preview"
     */
    NOTIFICATION_DONE,
    /**
     * \image html notification_error.png "Icon preview"
     */
    NOTIFICATION_ERROR,
    /**
     * \image html notification_help.png "Icon preview"
     */
    NOTIFICATION_HELP,
    /**
     * \image html notification_info.png "Icon preview"
     */
    NOTIFICATION_INFO,
    /**
     * \image html notification_remove.png "Icon preview"
     */
    NOTIFICATION_REMOVE,
    /**
     * \image html notification_warning.png "Icon preview"
     */
    NOTIFICATION_WARNING,
    /**
     * \image html piechart.png "Icon preview"
     */
    PIECHART,
    /**
     * \image html player_fastforward.png "Icon preview"
     */
    PLAYER_FASTFORWARD,
    /**
     * \image html player_play.png "Icon preview"
     */
    PLAYER_PLAY,
    /**
     * \image html player_pause.png "Icon preview"
     */
    PLAYER_PAUSE,
    /**
     * \image html player_record.png "Icon preview"
     */
    PLAYER_RECORD,
    /**
     * \image html player_rewind.png "Icon preview"
     */
    PLAYER_REWIND,
    /**
     * \image html player_stop.png "Icon preview"
     */
    PLAYER_STOP,
    /**
     * \image html rss.png "Icon preview"
     */
    RSS,
    /**
     * \image html search.png "Icon preview"
     */
    SEARCH,
    /**
     * \image html security_key.png "Icon preview"
     */
    SECURITY_KEY,
    /**
     * \image html security_keyandlock.png "Icon preview"
     */
    SECURITY_KEYANDLOCK,
    /**
     * \image html security_lock.png "Icon preview"
     */
    SECURITY_LOCK,
    /**
     * \image html security_unlock.png "Icon preview"
     */
    SECURITY_UNLOCK,
    /**
     * \image html shoppingcart.png "Icon preview"
     */
    SHOPPINGCART,
    /**
     * \image html shoppingcart_add.png "Icon preview"
     */
    SHOPPINGCART_ADD,
    /**
     * \image html shoppingcart_checkout.png "Icon preview"
     */
    SHOPPINGCART_CHECKOUT,
    /**
     * \image html shoppingcart_remove.png "Icon preview"
     */
    SHOPPINGCART_REMOVE,
    /**
     * \image html shoppingcart_warning.png "Icon preview"
     */
    SHOPPINGCART_WARNING,
    /**
     * \image html star_empty.png "Icon preview"
     */
    STAR_EMPTY,
    /**
     * \image html star_half.png "Icon preview"
     */
    STAR_HALF,
    /**
     * \image html star_full.png "Icon preview"
     */
    STAR_FULL,
    /**
     * \image html user.png "Icon preview"
     */
    USER,
    /**
     * \image html user_add.png "Icon preview"
     */
    USER_ADD,
    /**
     * \image html user_delete.png "Icon preview"
     */
    USER_DELETE,
    /**
     * \image html user_manage.png "Icon preview"
     */
    USER_MANAGE,
    /**
     * \image html user_warning.png "Icon preview"
     */
    USER_WARNING,
    /**
     * \image html volume.png "Icon preview"
     */
    VOLUME,
    /**
     * \image html volume_down.png "Icon preview"
     */
    VOLUME_DOWN,
    /**
     * \image html volume_up.png "Icon preview"
     */
    VOLUME_UP,
    /**
     * \image html volume_mute.png "Icon preview"
     */
    VOLUME_MUTE
}
