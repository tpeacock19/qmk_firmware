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

# USER_NAME := tpeacock19

 # START-MUGUR-REGION

 FORCE_NKRO       = yes
LEADER_ENABLE    = no
RGBLIGHT_ENABLE  = no
TAP_DANCE_ENABLE = yes
COMBO_ENABLE     = yes 

# END-MUGUR-REGION      

# Optimize size but this may cause error "relocation truncated to fit"
EXTRALDFLAGS     = -Wl,--relax

BOOTMAGIC_ENABLE   = yes	# Enable Bootmagic Lite
LTO_ENABLE         = yes	# link time optimization to shrink size
MOUSEKEY_ENABLE    = yes	# Emulate mouse using keyboard
EXTRAKEY_ENABLE    = yes	# Audio and System control
CONSOLE_ENABLE     = no		# Disable Console for debug
COMMAND_ENABLE     = no		# Deprecated magic commands
MAGIC_ENABLE       = no		# Magic commands after init
SPACE_CADET_ENABLE = no		# Space Cadet Features
# NKRO_ENABLE        = no		# Disable N-Key Rollover
AUDIO_ENABLE       = no		# Audio direct from keyboard to speaker
MUSIC_ENABLE       = no		# Maps columns/rows to chromatic/octaves
RGBLIGHT_ENABLE    = no		# RGB LED controls
RGB_MATRIX_ENABLE  = no		# RGB Matrix lighting
OLED_ENABLE        = no		# Oled capability

AUTO_SHIFT_ENABLE = yes
GRAVE_ESC_ENABLE = yes
COMBO_ENABLE       = yes	#

DYNAMIC_TAPPING_TERM_ENABLE = yes

VPATH += keyboards/gboards

SRC += features/achordion.c
