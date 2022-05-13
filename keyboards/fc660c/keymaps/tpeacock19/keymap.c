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

enum tpeacock19_layers { _COLEMAK, _QWERTY, _FNM, _NUM, _MDIA, _UTIL };

enum tpeacock19_keycodes { COLEMAK = SAFE_RANGE, QWERTY, FNM, NUM, MDIA, UTIL };

// Define a type for as many tap dance states as you need
typedef enum { TD_NONE, TD_UNKNOWN, TD_SINGLE_TAP, TD_SINGLE_HOLD, TD_DOUBLE_TAP } td_state_t;

typedef struct {
    bool       is_press_action;
    td_state_t state;
} td_tap_t;

enum { QUOT_LAYR, TD_RESET, TD_SPC };

// Function associated with all tap dances
td_state_t cur_dance(qk_tap_dance_state_t *state);

// Functions associated with individual tap dances
void ql_finished(qk_tap_dance_state_t *state, void *user_data);
void ql_reset(qk_tap_dance_state_t *state, void *user_data);
void safe_reset(qk_tap_dance_state_t *state, void *user_data);

// START-MUGUR-REGION

/* Macros */

/* Tap Dances */

// Associate our tap dance key with its functionality
qk_tap_dance_action_t tap_dance_actions[] = {[QUOT_LAYR] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, ql_finished, ql_reset), [TD_SPC] = ACTION_TAP_DANCE_DOUBLE(KC_SPC, KC_ENT), [TD_RESET] = ACTION_TAP_DANCE_FN(safe_reset)};

/* Leader Keys */

/* Combos */

/* Layer Codes and Matrix */

#define ASTRSK KC_KP_ASTERISK
#define C_ESC CTL_T(KC_ESC)
#define CA_CAPS LCA_T(KC_CAPS)
#define FNM MO(_FNM)
#define NUM TG(_NUM)
#define MDIA TG(_MDIA)
#define UTIL TG(_UTIL)
#define GUI_A LGUI_T(KC_A)
#define ALT_R LALT_T(KC_R)
#define CTL_S LCTL_T(KC_S)
#define SHFT_T LSFT_T(KC_T)
#define SFT_N RSFT_T(KC_N)
#define CTL_E RCTL_T(KC_E)
#define ALT_I LALT_T(KC_I)
#define GUI_O RGUI_T(KC_O)
#define MDIA_V LT(_MDIA, KC_V)
#define UTIL_K LT(_UTIL, KC_K)
#define UTIL_N LT(_UTIL, KC_N)
#define MLSFT OSM(MOD_LSFT)
#define MRSFT OSM(MOD_RSFT)
#define QUOT_TD TD(QUOT_LAYR)
#define SPC_TD TD(TD_SPC)
#define TD_RES TD(TD_RESET)
#define GRAVE_MODS (MOD_BIT(KC_LSHIFT) | MOD_BIT(KC_RSHIFT) | MOD_BIT(KC_LGUI) | MOD_BIT(KC_RGUI) | MOD_BIT(KC_LALT) | MOD_BIT(KC_RALT))

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {[_COLEMAK] = LAYOUT(KC_GESC, KC_1, KC_2, KC_3, KC_4, KC_5, KC_6, KC_7, KC_8, KC_9, KC_0, KC_MINS, KC_EQL, KC_BSPC, KC_GRV, KC_TAB, KC_Q, KC_W, KC_F, KC_P, KC_B, KC_J, KC_L, KC_U, KC_Y, KC_SCLN, KC_LBRC, KC_RBRC, KC_BSLS, KC_DEL, C_ESC, GUI_A, ALT_R, CTL_S, SHFT_T, KC_G, KC_M, SFT_N, CTL_E, ALT_I, GUI_O, QUOT_TD, KC_ENT, MLSFT, KC_X, KC_C, KC_D, MDIA_V, KC_Z, UTIL_K, KC_H, KC_COMM, KC_DOT, KC_SLSH, MRSFT, KC_UP, KC_CAPS, KC_LGUI, KC_LALT, SPC_TD, KC_RALT, KC_RCTL, FNM, KC_LEFT, KC_DOWN, KC_RIGHT),

                                                              [_QWERTY] = LAYOUT(KC_GESC, KC_1, KC_2, KC_3, KC_4, KC_5, KC_6, KC_7, KC_8, KC_9, KC_0, KC_MINS, KC_EQL, KC_BSPC, KC_GRV, KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P, KC_LBRC, KC_RBRC, KC_BSLS, KC_DEL, C_ESC, KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L, KC_SCLN, QUOT_TD, KC_ENT, MLSFT, KC_Z, KC_X, KC_C, MDIA_V, KC_B, UTIL_N, KC_M, KC_COMM, KC_DOT, KC_SLSH, MRSFT, KC_UP, KC_CAPS, KC_LGUI, KC_LALT, SPC_TD, KC_RALT, KC_RCTL, FNM, KC_LEFT, KC_DOWN, KC_RIGHT),

                                                              [_FNM] = LAYOUT(KC_GRV, KC_F1, KC_F2, KC_F3, KC_F4, KC_F5, KC_F6, KC_F7, KC_F8, KC_F9, KC_F10, KC_F11, KC_F12, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, KC_PSCR, KC_SLCK, _______, _______, _______, KC_F20, _______, _______, _______, _______, QWERTY, COLEMAK, _______, _______, _______, KC_HOME, KC_PGUP, _______, _______, _______, _______, _______, KC_COPY, _______, KC_PASTE, _______, _______, _______, KC_END, KC_PGDN, _______, KC_BTN1, KC_MS_U, DEBUG, RESET, _______, _______, _______, _______, _______, KC_MS_L, KC_MS_D, KC_MS_R),

                                                              /* Per Key Tapping Terms */
                                                              uint16_t get_tapping_term(uint16_t keycode, keyrecord_t *record){switch (keycode){case LT(5, KC_K) : return TAPPING_TERM + 150;
case LT(4, KC_N):
    return TAPPING_TERM + 150;
case LT(4, KC_V):
    return TAPPING_TERM + 150;
case TD(QUOT_LAYR):
    return TAPPING_TERM;
default:
    return TAPPING_TERM;
    }
    }
    ;

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
