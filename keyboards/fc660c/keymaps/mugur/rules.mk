# -*- buffer-read-only: t -*-
# Copyright 2022 Trey Peacock

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

BOOTMAGIC_ENABLE = yes  # Enable Bootmagic Lite
CONSOLE_ENABLE   = no     # Disable Console for debug
NKRO_ENABLE      = no        # Disable N-Key Rollover

# START-MUGUR-REGION

FORCE_NKRO       = yes
LEADER_ENABLE    = no
RGBLIGHT_ENABLE  = no
TAP_DANCE_ENABLE = yes
COMBO_ENABLE     = no 

# END-MUGUR-REGION   

# Optimize size but this may cause error "relocation truncated to fit"
EXTRALDFLAGS     = -Wl,--relax

AUTO_SHIFT_ENABLE = yes
DYNAMIC_TAPPING_TERM_ENABLE = yes

COMBO_ENABLE     = yes 
VPATH += keyboards/gboards

# Space saving
LTO_ENABLE = yes
MUSIC_ENABLE = no

SRC += features/achordion.c
