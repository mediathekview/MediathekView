#!/usr/bin/make -f
# * Makefile to create MediathekView's upstream source tarball
# *
# * Copyright 2014 Markus Koschany <apo@gambaru.de>
# * This program is free software: you can redistribute it and/or modify
# * it under the terms of the GNU General Public License as published by
# * the Free Software Foundation, either version 3 of the License, or
# * any later version.
# *
# * This program is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# * GNU General Public License for more details.
# *
# * You should have received a copy of the GNU General Public License
# * along with this program. If not, see <http://www.gnu.org/licenses/>.
# */

PKG      := mediathekview
PKG2     := msearch
VER      := 9
UURL     := https://github.com/xaverW/MediathekView.git
UURL2    := https://github.com/xaverW/MSearch.git
COMMIT   := 6216176243971a3c33c83aa23ddc015ae0abeff2
COMMIT2  := 0459c77cb35401afb9a25dff8c6dbfd0a03e9d5b


.PHONY: make-orig-source
make-orig-source: $(PKG)_$(VER).tar.xz
	@

$(PKG)_$(VER).tar.xz:
	@echo "# Cloning upstream git repository..."
	[ -d $(PKG)-$(VER) ] || git clone $(UURL) $(PKG)-$(VER)
	[ -d $(PKG2)-$(VER) ] || git clone $(UURL2) $(PKG2)-$(VER)
	cd $(PKG)-$(VER) \
	&& git checkout -b debiansource $(COMMIT) \
	&& echo "# Setting times..." \
	&& for F in $$(git ls-tree -r --name-only HEAD); \
	do touch --no-dereference -d "$$(git log -1 --format="%ai" -- $$F)" "$$F"; done \
	&& echo "# Cleaning-up..." \
	&& mv -v \
		dist/Info/MediathekView.png \
		dist/Info/MediathekView.svg \
		dist/Info/MediathekView.xpm \
		dist/Info/releases.txt \
		dist/bin/flv.sh \
		dist/Anleitung . \
	&& mv -v \
		releases.txt ChangeLog \
	&& $(RM) -r -v \
		src/com/ \
		src/net/sf/jtelegraph \
		src/org/jdesktop \
		dist/ \
		libs/ \
		res/ \
		launch4j.xml launch4j-winXp.xml post_make.sh \
		.git/
	cd $(PKG2)-$(VER) \
	&& git checkout -b debiansource $(COMMIT2) \
	&& echo "# Setting times..." \
	&& for F in $$(git ls-tree -r --name-only HEAD); \
	do touch --no-dereference -d "$$(git log -1 --format="%ai" -- $$F)" "$$F"; done \
	&& cp -r \
		src/msearch ../$(PKG)-$(VER)/src/
	@echo "# Packing..."
	find -L "$(PKG)-$(VER)" -xdev -type f -print | sort \
	| XZ_OPT="-6v" tar -caf "$(PKG)_$(VER).tar.xz" -T- --owner=root --group=root --mode=a+rX
