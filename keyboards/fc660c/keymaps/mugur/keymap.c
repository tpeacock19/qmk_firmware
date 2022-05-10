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

#include QMK_KEYBOARD_H
#include "g/keymap_combo.h"
#include "features/achordion.h"

enum key_codes { COLEMAK = SAFE_RANGE, QWERTY, FNM, NUM, MDIA, UTIL };
// Define a type for as many tap dance states as you need
typedef enum { TD_NONE, TD_UNKNOWN, TD_SINGLE_TAP, TD_SINGLE_HOLD, TD_DOUBLE_TAP } td_state_t;

typedef struct {
    bool       is_press_action;
    td_state_t state;
} td_tap_t;

enum { TD_RESET };

// Function associated with all tap dances
td_state_t cur_dance(qk_tap_dance_state_t *state);

// Functions associated with individual tap dances
void ql_finished(qk_tap_dance_state_t *state, void *user_data);
void ql_reset(qk_tap_dance_state_t *state, void *user_data);
void safe_reset(qk_tap_dance_state_t *state, void *user_data);

// START-MUGUR-REGION

/* Macros */


/* Tap Dances */

enum {DANCE_1, DANCE_2, DANCE_3}; 

qk_tap_dance_action_t tap_dance_actions[] = {
	[DANCE_1] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, ql_finished, ql_reset), 
	[DANCE_2] = ACTION_TAP_DANCE_DOUBLE(KC_SPACE, KC_ENTER), 
	[DANCE_3] = ACTION_TAP_DANCE_FN(safe_reset)
};

/* Leader Keys */


/* Combos */


/* Layer Codes and Matrix */
enum layer_codes {_COLEMAK, _QWERTY, _FNM, _NUM, _MDIA, _UTIL}; 

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
	[_COLEMAK] = LAYOUT(KC_GESC, KC_1, KC_2, KC_3, KC_4, KC_5, KC_6, KC_7, KC_8, KC_9, KC_0, KC_MINUS, KC_EQUAL, KC_BSPACE, KC_GRAVE, KC_TAB, KC_Q, KC_W, KC_F, KC_P, KC_B, KC_J, KC_L, KC_U, KC_Y, KC_SCOLON, KC_LBRACKET, KC_RBRACKET, KC_BSLASH, KC_DELETE, MT(MOD_LCTL, KC_ESCAPE), MT(MOD_LGUI, KC_A), MT(MOD_LALT, KC_R), MT(MOD_LCTL, KC_S), MT(MOD_LSFT, KC_T), KC_G, KC_M, MT(MOD_LSFT, KC_N), MT(MOD_LCTL, KC_E), MT(MOD_LALT, KC_I), MT(MOD_LGUI, KC_O), TD(DANCE_1), KC_ENTER, OSM(MOD_LSFT), KC_X, KC_C, KC_D, LT(_MDIA, KC_V), KC_Z, LT(_UTIL, KC_K), KC_H, KC_COMMA, KC_DOT, KC_SLASH, OSM(MOD_LSFT), KC_UP, KC_CAPSLOCK, KC_LGUI, KC_LALT, TD(DANCE_2), KC_RALT, KC_RCTRL, MO(_FNM), KC_LEFT, KC_DOWN, KC_RIGHT),

	[_QWERTY] = LAYOUT(KC_GESC, KC_1, KC_2, KC_3, KC_4, KC_5, KC_6, KC_7, KC_8, KC_9, KC_0, KC_MINUS, KC_EQUAL, KC_BSPACE, KC_GRAVE, KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P, KC_LBRACKET, KC_RBRACKET, KC_BSLASH, KC_DELETE, MT(MOD_LCTL, KC_ESCAPE), KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L, KC_SCOLON, TD(DANCE_1), KC_ENTER, OSM(MOD_LSFT), KC_Z, KC_X, KC_C, LT(_MDIA, KC_V), KC_B, LT(_UTIL, KC_N), KC_M, KC_COMMA, KC_DOT, KC_SLASH, OSM(MOD_LSFT), KC_UP, KC_CAPSLOCK, KC_LGUI, KC_LALT, TD(DANCE_2), KC_RALT, KC_RCTRL, MO(_FNM), KC_LEFT, KC_DOWN, KC_RIGHT),

	[_FNM] = LAYOUT(KC_GRAVE, KC_F1, KC_F2, KC_F3, KC_F4, KC_F5, KC_F6, KC_F7, KC_F8, KC_F9, KC_F10, KC_F11, KC_F12, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_PSCREEN, KC_SCROLLLOCK, KC_TRNS, KC_TRNS, KC_TRNS, KC_F20, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, QWERTY, COLEMAK, KC_TRNS, KC_TRNS, KC_TRNS, KC_HOME, KC_PGUP, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_COPY, KC_TRNS, KC_PASTE, KC_TRNS, KC_TRNS, KC_TRNS, KC_END, KC_PGDOWN, KC_TRNS, KC_MS_BTN1, KC_MS_UP, DEBUG, TD(DANCE_3), KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_LEFT, KC_MS_DOWN, KC_MS_RIGHT),

	[_NUM] = LAYOUT(KC_GRAVE, KC_F1, KC_F2, KC_F3, KC_F4, KC_F5, KC_F6, KC_F7, KC_F8, KC_F9, KC_F10, KC_F11, KC_F12, DT_PRNT, DT_UP, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_7, KC_8, KC_9, KC_MINUS, KC_TRNS, KC_TRNS, KC_TRNS, DT_DOWN, TG(_NUM), KC_TRNS, KC_TRNS, QWERTY, COLEMAK, KC_TRNS, KC_TRNS, KC_4, KC_5, KC_6, KC_PLUS, KC_KP_ASTERISK, KC_TRNS, KC_DOT, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_1, KC_2, KC_3, KC_SLASH, KC_DOT, KC_MS_UP, DEBUG, TD(DANCE_3), KC_TRNS, KC_0, KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_LEFT, KC_MS_DOWN, KC_MS_RIGHT),

	[_MDIA] = LAYOUT(KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_ASRP, KC_ASUP, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_AUDIO_MUTE, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_ASDN, TG(_MDIA), KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_MEDIA_PREV_TRACK, KC_AUDIO_VOL_DOWN, KC_AUDIO_VOL_UP, KC_MEDIA_NEXT_TRACK, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_UP, KC_TRNS, KC_TRNS, KC_TRNS, KC_MEDIA_PLAY_PAUSE, KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_LEFT, KC_MS_DOWN, KC_MS_RIGHT),

	[_UTIL] = LAYOUT(KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, TG(_UTIL), KC_TRNS, KC_BRIGHTNESS_DOWN, KC_BRIGHTNESS_UP, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_COPY, KC_TRNS, KC_PASTE, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS)
};

/* Per Key Tapping Terms */
uint16_t get_tapping_term(uint16_t keycode, keyrecord_t *record) {
	switch (keycode) {
		case LT(_UTIL, KC_K):
			return TAPPING_TERM + 150;
		case LT(_UTIL, KC_N):
			return TAPPING_TERM + 150;
		case LT(_MDIA, KC_V):
			return TAPPING_TERM + 150;
		case TD(DANCE_1):
			return TAPPING_TERM;
		default:
			return TAPPING_TERM;
	}
}; 

// END-MUGUR-REGION

// Determine the current tap dance state
td_state_t cur_dance(qk_tap_dance_state_t *state) {
    if (state->count == 1) {
        if (!state->pressed)
            return TD_SINGLE_TAP;
        else
            return TD_SINGLE_HOLD;
    } else if (state->count == 2)
        return TD_DOUBLE_TAP;
    else
        return TD_UNKNOWN;
}

// Initialize tap structure associated with example tap dance key
static td_tap_t ql_tap_state = {.is_press_action = true, .state = TD_NONE};

void ql_finished(qk_tap_dance_state_t *state, void *user_data) {
    ql_tap_state.state = cur_dance(state);
    switch (ql_tap_state.state) {
        case TD_SINGLE_TAP:
            tap_code(KC_QUOT);
            break;
        case TD_SINGLE_HOLD:
            layer_on(_FNM);
            break;
        case TD_DOUBLE_TAP:
            // Check to see if the layer is already set
            if (layer_state_is(_NUM)) {
                // If already set, then switch it off
                layer_off(_NUM);
            } else {
                // If not already set, then switch the layer on
                layer_on(_NUM);
            }
            break;
        default:
            break;
    }
};

void ql_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    if (ql_tap_state.state == TD_SINGLE_HOLD) {
        layer_off(_FNM);
    }
    ql_tap_state.state = TD_NONE;
};

void safe_reset(qk_tap_dance_state_t *state, void *user_data) {
    if (state->count >= 3) {
        // Reset the keyboard if you tap the key more than three times
        reset_keyboard();
        reset_tap_dance(state);
    }
}

bool get_tapping_force_hold(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case LT(1, KC_BSPC):
            return true;
        default:
            if (record->event.key.row == 4) {
                return true;
            }
            return false;
    }
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (!process_achordion(keycode, record)) {
        return false;
    }
    switch (keycode) {
        case QWERTY:
            if (record->event.pressed) {
                set_single_persistent_default_layer(_QWERTY);
            }
            return false;
        case COLEMAK:
            if (record->event.pressed) {
                set_single_persistent_default_layer(_COLEMAK);
            }
            return false;
        case LT(0, KC_NO):
            if (record->tap.count && record->event.pressed) {
                tap_code16(C(KC_C)); // Intercept tap function to send Ctrl-C
            } else if (record->event.pressed) {
                tap_code16(C(KC_V)); // Intercept hold function to send Ctrl-V
            }
            return false;
    }
    return true;
};

static bool fc660c_left_hand(keypos_t pos) {
    if (pos.col == 5) return false;
    switch (pos.row) {
        case 3:
            return pos.col < 8;
        case 2:
            return pos.col < 4;
        default:
            return pos.col < 7;
    }
}

bool fc660c_achordion_opposite_hands(const keyrecord_t *tap_hold_record, const keyrecord_t *other_record) {
    return fc660c_left_hand(tap_hold_record->event.key) != fc660c_left_hand(other_record->event.key);
}

bool achordion_chord(uint16_t tap_hold_keycode, keyrecord_t *tap_hold_record, uint16_t other_keycode, keyrecord_t *other_record) {
    return fc660c_achordion_opposite_hands(tap_hold_record, other_record);
}

void matrix_scan_user(void) {
    achordion_task();
}
