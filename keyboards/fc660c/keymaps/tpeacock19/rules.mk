# MCU name
MCU = atmega32u4

# Bootloader selection
#   Teensy       halfkay
#   Pro Micro    caterina
#   Atmel DFU    atmel-dfu
#   LUFA DFU     lufa-dfu
#   QMK DFU      qmk-dfu
#   ATmega32A    bootloadHID
#   ATmega328P   USBasp
BOOTLOADER = atmel-dfu

# Build Options
#   comment out to disable the options.
#
BOOTMAGIC_ENABLE = yes	# Virtual DIP switch configuration
MOUSEKEY_ENABLE = yes	# Mouse keys
EXTRAKEY_ENABLE = yes	# Audio control and System control
CONSOLE_ENABLE = no	# Console for debug
COMMAND_ENABLE = yes    # Commands for debug and configuration
# NKRO_ENABLE = yes		# USB Nkey Rollover - not yet supported in LUFA
# TAP_DANCE_ENABLE = yes

# Optimize size but this may cause error "relocation truncated to fit"
#EXTRALDFLAGS = -Wl,--relax

CUSTOM_MATRIX = yes
SRC +=	matrix.c \
		actuation_point.c \
		i2c.c

# WPM_ENABLE = yes
# Holding down keys will result in shifted character
AUTO_SHIFT_ENABLE = yes

# Space saving
LTO_ENABLE = yes
MUSIC_ENABLE = no
