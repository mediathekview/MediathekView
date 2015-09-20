#! /bin/sh
#

# Bei Linux wird das Script ausgeführt
# um den Recher herunter zu fahren

# mögliche Aufrufe:

# systemctl poweroff
# poweroff
# dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
# dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:0 int32:2 int32:2
# sudo shutdown -P now

shutdown -h now

