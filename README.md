
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Build and test](https://github.com/mediathekview/MediathekView/workflows/Build%20and%20test/badge.svg?branch=master)](https://github.com/mediathekview/MediathekView/actions?query=workflow%3A"Build+and+test"+branch%3Amaster)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=mediathekview_MediathekView&metric=alert_status)](https://sonarcloud.io/dashboard?id=mediathekview_MediathekView)

# MediathekView
Das Programm **MediathekView** durchsucht die Mediatheken verschiedener überwiegend deutschsprachiger öffentlich-rechtlicher Sender, lädt Beiträge daraus herunter oder spielt diese ab (mit [VLC Media Player](https://videolan.org/vlc/) oder mit einem Programm eigener Wahl). Es können auch Sendereihen/Serien abonniert werden.

![MediathekView 13.6.0](https://mediathekview.de/images/news/mediathekview-13_6-linux-filter-auswahl-toolbar.png)
## Sender
Derzeit werden die Mediatheken der folgenden Sender unterstützt:

- ARD (Das Erste)
   - alpha 
   - BR (Bayerischer Rundfunk)
   - HR (Hessischer Rundfunk)
   - MDR (Mitteldeutscher Rundfunk)
   - NDR (Norddeutscher Rundfunk)
   - RBB (Rundfunk Berlin-Brandenburg)
   - rbtv (radiobremen)
   - SR (Saarländischer Rundfunk)
   - SWR (Südwestrundfunk)
   - WDR (Westdeutscher Rundfunk)
- ZDF (Zweites Deutsches Fernsehen)
   - ZDFinfo
   - ZDFneo
   - zdf-tivi
- Gemeinschaftsprogramme von ARD und ZDF
   - 3Sat
   - Arte (deutsch, englisch, französisch, spanisch, italienisch und polnisch)
   - Funk
   - Kika (Kinderkanal von ARD und ZDF)
   - Phoenix
- DW TV (Deutsche Welle)
- ORF (Österreichischer Rundfunk)
- SRF (Schweizer Rundfunk) inkl. Podcasts

# Installation
MediathekView wird in mehreren Paketen auf der [Webseite](https://mediathekview.de/download/) angeboten. , darunter Installer für Windows & Linux.
Zusätzlich zu den Installern sind auch Portable Varianten vorhanden z.B. als [Zip](https://download.mediathekview.de/stabil/MediathekView-latest-win.zip) oder [tar.gz](https://download.mediathekview.de/stabil/MediathekView-latest-linux.tar.gz). Diese Pakete beinhalten bereits die benötigte Java Version, Java muss also nicht installiert sein.  
Für RPM-basierte Linux Pakete steht auch ein [GPG Key](https://download.mediathekview.de/stabil/MediathekView-rpm-signature-2021.pub) zur Verfügung, mit dem das RPM-Pakete anhand seiner Signatur verifiziert werden kann. Dieser kann mit diesem Kommando importiert werden:
```bash
sudo rpm --import https://download.mediathekview.de/stabil/MediathekView-rpm-signature-2021.pub
```

**macOS:** Für macOS werden fertige DMG-Images sowohl für Intel als auch Apple Silicon Rechner angeboten.

# Bedienung
Siehe [Anleitung](https://mediathekview.de/anleitung/)

# Support
Bei Fragen, Hilfe, gesuchten Sendungen oder sonstigen bitte das [Forum](https://forum.mediathekview.de/) verwenden.

Für Bugs siehe [Contribution Guide](https://github.com/mediathekview/MediathekView/blob/master/CONTRIBUTING.md#reporting-bugs).

# [Entwicklung / Contributing](https://github.com/mediathekview/MediathekView/blob/master/CONTRIBUTING.md)
See / Siehe [Contribution Guide](https://github.com/mediathekview/MediathekView/blob/master/CONTRIBUTING.md)

# [Lizenz / License]((https://github.com/mediathekview/MediathekView/blob/master/LICENSE.md))
[GPL v3](https://github.com/mediathekview/MediathekView/blob/master/LICENSE.md)

# Links
- [Webseite](https://mediathekview.de)
- [Download](https://mediathekview.de/download/)
- [Wiki](https://github.com/mediathekview/MediathekView/wiki)
- [Forum](https://forum.mediathekview.de/)
- [Anleitung](https://mediathekview.de/anleitung/)
- [FAQ](https://mediathekview.de/faq/)
