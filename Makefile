#
# Makefile
#
# Copyright (C) 2013 James Hogan <james@albanarts.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
# (in the file called COPYING).
#
#
# Main top-level Makefile.
#

include scripts/Makefile.common

inc-y		+= include/

cflags-y	+= -Wall -g -O0

targ-y		+= chron/

include scripts/Makefile.build
