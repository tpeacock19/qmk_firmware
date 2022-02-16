// -*- buffer-read-only: t -*-
// Copyright 2022 Trey Peacock

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

// default but used in macros
#undef TAPPING_TERM
#define TAPPING_TERM 230
#define TAPPING_TERM_PER_KEY

// Prevent normal rollover on alphas from accidentally triggering mods.
#define IGNORE_MOD_TAP_INTERRUPT

// Enable rapid switch from tap to hold, disables double tap hold auto-repeat.
#define TAPPING_FORCE_HOLD_PER_KEY

// Auto Shift
#define NO_AUTO_SHIFT_ALPHA
#define AUTO_SHIFT_TIMEOUT TAPPING_TERM
#define AUTO_SHIFT_NO_SETUP

// Make actuation point more sensitive
#define ACTUATION_DEPTH_ADJUSTMENT -3

// Tapping this number of times holds the key until tapped once again.
#define ONESHOT_TAP_TOGGLE 2 
// Time (in ms) before the one shot code is released
#define ONESHOT_TIMEOUT 5000 

#define NO_MUSIC_MODE
