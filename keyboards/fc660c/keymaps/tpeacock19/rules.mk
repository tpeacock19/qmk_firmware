# -*- buffer-read-only: t -*-
# Copyright 2022 Trey Peacock

BOOTMAGIC_ENABLE = yes  # Enable Bootmagic Lite
CONSOLE_ENABLE = no     # Disable Console for debug
NKRO_ENABLE = no        # Disable N-Key Rollover

# Optimize size but this may cause error "relocation truncated to fit"
EXTRALDFLAGS = -Wl,--relax

AUTO_SHIFT_ENABLE = yes
TAP_DANCE_ENABLE = yes
DYNAMIC_TAPPING_TERM_ENABLE = yes

# Combos
COMBO_ENABLE = yes
VPATH += keyboards/gboards

# Space saving
LTO_ENABLE = yes
MUSIC_ENABLE = no
