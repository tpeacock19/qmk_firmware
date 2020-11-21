/* Copyright 2017 Balz Guenat
 *
 * This program is free software: you can redistribute it and/or modify
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

// place overrides here

/*****************************************************************************/
/*                             Auto Shift Timeout                            */
/*****************************************************************************/

/*****************************************************************************/
/*					Temporary Rebind for setting timeout
Key Name 	Description
KC_ASDN 	Lower the Auto Shift timeout variable (down)
KC_ASUP 	Raise the Auto Shift timeout variable (up)
KC_ASRP 	Report your current Auto Shift timeout value
KC_ASON 	Turns on the Auto Shift Function
KC_ASOFF 	Turns off the Auto Shift Function
KC_ASTG 	Toggles the state of the Auto Shift feature 					 */
/*****************************************************************************/

/* #define AUTO_SHIFT_TIMEOUT 165 */

/* Do not Auto Shift special keys, which include -_, =+, [{, ]}, ;:, ‘“, ,<, .>, and /? */
/* #define NO_AUTO_SHIFT_SPECIAL */

/* Do not Auto Shift numeric keys, [0-9] */
/* #define NO_AUTO_SHIFT_NUMERIC */

/* Do not Auto Shift alpha characters, [a-Z] */
/* #define NO_AUTO_SHIFT_ALPHA */

/*****************************************************************************/
/*                             Oneshot Settings                              */
/*****************************************************************************/
#define ONESHOT_TAP_TOGGLE 2  /* Tapping this number of times holds the key until tapped once again. */
#define ONESHOT_TIMEOUT 5000  /* Time (in ms) before the one shot key is released */
