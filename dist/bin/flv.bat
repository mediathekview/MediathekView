@echo off
REM %1 ist der flvstreamer
REM %2 ist der vlc-player
REM %3 und die weiteren sind die url des Films

%1 %3 %4 %5 %6 %7 %8 %9 --quiet | %2 -
