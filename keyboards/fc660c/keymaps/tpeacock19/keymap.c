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
#define GRAVE_MODS  (MOD_BIT(KC_LSHIFT)|MOD_BIT(KC_RSHIFT)|MOD_BIT(KC_LGUI)|MOD_BIT(KC_RGUI)|MOD_BIT(KC_LALT)|MOD_BIT(KC_RALT))

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [_COLEMAK] = LAYOUT(
                      KC_GESC, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS, KC_EQL,  KC_BSPC,          KC_GRV,
                      KC_TAB,  KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_Y,    KC_SCLN, KC_LBRC, KC_RBRC, KC_BSLS,          KC_DEL,
                      C_ESC,   GUI_A,   ALT_R,   CTL_S,   SHFT_T,  KC_G,    KC_M,    SFT_N,   CTL_E,   ALT_I,   GUI_O,   QUOT_TD,          KC_ENT,
                      MLSFT,   KC_X,    KC_C,    KC_D,    MDIA_V,  KC_Z,    UTIL_K,  KC_H,    KC_COMM, KC_DOT,  KC_SLSH,                   MRSFT,   KC_UP,
                      KC_CAPS, KC_LGUI, KC_LALT,                            SPC_TD,                             KC_RALT, KC_RCTL, FNM,     KC_LEFT, KC_DOWN, KC_RIGHT
                      ),
  
  [_QWERTY] = LAYOUT(
                      KC_GESC, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS, KC_EQL,  KC_BSPC,          KC_GRV,
                      KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC, KC_RBRC, KC_BSLS,          KC_DEL,
                      C_ESC,   KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, QUOT_TD,          KC_ENT,
                      MLSFT,   KC_Z,    KC_X,    KC_C,    MDIA_V,  KC_B,    UTIL_N,  KC_M,    KC_COMM, KC_DOT,  KC_SLSH,                   MRSFT,   KC_UP,
                      KC_CAPS, KC_LGUI, KC_LALT,                            SPC_TD,                             KC_RALT, KC_RCTL, FNM,     KC_LEFT, KC_DOWN, KC_RIGHT
                      ),

  [_FNM] = LAYOUT(
                      KC_GRV,  KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  _______,          _______,
                      _______, _______, _______, _______, _______, _______, _______, _______, KC_PSCR, KC_SLCK, _______, _______, _______, KC_F20,           _______,
                      _______, _______, _______, QWERTY,  COLEMAK, _______, _______, _______, KC_HOME, KC_PGUP, _______, _______,          _______,
                      _______, _______, KC_COPY, _______, KC_PASTE, _______, _______, _______, KC_END,  KC_PGDN, _______,                   KC_BTN1, KC_MS_U,
                      DEBUG,   RESET,   _______,                            _______,                            _______, _______, _______, KC_MS_L, KC_MS_D, KC_MS_R
                      ),

  [_NUM] = LAYOUT(
                      KC_GRV,  KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  DT_PRNT,          DT_UP,
                      _______, _______, _______, _______, _______, _______, _______, KC_7,    KC_8,    KC_9,    KC_MINS, _______, _______, _______,          DT_DOWN,
                      NUM,     _______, _______, QWERTY,  COLEMAK, _______, _______, KC_4,    KC_5,    KC_6,    KC_PLUS, ASTRSK,           _______,
                      KC_DOT,  _______, _______, _______, _______, _______, _______, KC_1,    KC_2,    KC_3,    KC_SLSH,                   KC_DOT,  KC_MS_U,
                      DEBUG,   RESET,   _______,                            KC_0,                               _______, _______, _______, KC_MS_L, KC_MS_D, KC_MS_R
                      ),
  [_MDIA] = LAYOUT(
                      _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, KC_ASRP,          KC_ASUP,
                      _______, _______, _______, _______, _______, _______, _______, _______, KC_MUTE, _______, _______, _______, _______, _______,          KC_ASDN,
                      MDIA,    _______, _______, _______, _______, _______, _______, KC_MPRV, KC_VOLD, KC_VOLU, KC_MNXT, _______,          _______,
                      _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,                   _______, KC_MS_U,
                      _______, _______, _______,                            KC_MPLY,                            _______, _______, _______, KC_MS_L, KC_MS_D, KC_MS_R
                      ),
  [_UTIL] = LAYOUT(
                      _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,          _______,
                      _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,          _______,
                      UTIL,    _______, KC_BRID, KC_BRIU, _______, _______, _______, _______, _______, _______, _______, _______,          _______,
                      _______, _______, KC_COPY, _______, KC_PASTE, _______, _______, _______, _______, _______, _______,                   _______, _______,
                      _______, _______, _______,                            _______,                            _______, _______, _______, _______, _______, _______
                      )
};

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

// Associate our tap dance key with its functionality
qk_tap_dance_action_t tap_dance_actions[] = {[QUOT_LAYR] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, ql_finished, ql_reset), [TD_SPC] = ACTION_TAP_DANCE_DOUBLE(KC_SPC, KC_ENT)};

uint16_t get_tapping_term(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case LT(5, KC_K):
            return TAPPING_TERM + 150;
        case LT(4, KC_V):
            return TAPPING_TERM + 150;
        case TD(QUOT_LAYR):
            return TAPPING_TERM + 150;
        case GUI_T(KC_SPC):
            return TAPPING_TERM + 1250;
        default:
            return TAPPING_TERM;
    }
};

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
                tap_code16(C(KC_C));  // Intercept tap function to send Ctrl-C
            } else if (record->event.pressed) {
                tap_code16(C(KC_V));  // Intercept hold function to send Ctrl-V
            }
            return false;
            /* case RCTL_T(KC_E): */
            /*     /\* */
            /*       This piece of code nullifies the effect of Right Shift when tapping */
            /*       the RCTL_T(KC_E) key. */
            /*       This helps rolling over RSFT_T(KC_N) and RCTL_T(KC_E) */
            /*       to obtain the intended "en" instead of "N". */
            /*       Consequently, capital N can only be obtained by tapping RCTL_T(KC_E) */
            /*       and holding LSFT_T(KC_T) (which is the left Shift mod tap). */
            /*     *\/ */

            /*     /\* */
            /*       Detect the tap. */
            /*       We're only interested in overriding the tap behaviour */
            /*       in a certain cicumstance. The hold behaviour can stay the same. */
            /*     *\/ */
            /*     if (record->event.pressed && record->tap.count > 0) { */
            /*         // Detect right Shift */
            /*         if (get_mods() & MOD_BIT(KC_RSHIFT)) { */
            /*             // temporarily disable right Shift */
            /*             // so that we can send KC_N and KC_E */
            /*             // without Shift on. */
            /*             unregister_mods(MOD_BIT(KC_RSHIFT)); */
            /*             tap_code(KC_N); */
            /*             tap_code(KC_E); */
            /*             // restore the mod state */
            /*             add_mods(MOD_BIT(KC_RSHIFT)); */
            /*             // to prevent QMK from processing RCTL_T(KC_E) as usual in our special case */
            /*             return false; */
            /*         } */
            /*     } */
            /*     /\*else process RCTL_T(KC_E) as usual.*\/ */
            /*     return true; */
            /* case LCTL_T(KC_S): */
            /*     /\* */
            /*       This piece of code nullifies the effect of Left Shift when */
            /*       tapping the LCTL_T(KC_S) key. */
            /*       This helps rolling over LSFT_T(KC_T) and LCTL_T(KC_S) */
            /*       to obtain the intended "st" instead of "T". */
            /*       Consequently, capital T can only be obtained by tapping LCTL_T(KC_S) */
            /*       and holding RSFT_T(KC_N) (which is the right Shift mod tap). */
            /*     *\/ */

            /*     if (record->event.pressed && record->tap.count > 0) { */
            /*         if (get_mods() & MOD_BIT(KC_LSHIFT)) { */
            /*             unregister_mods(MOD_BIT(KC_LSHIFT)); */
            /*             tap_code(KC_T); */
            /*             tap_code(KC_S); */
            /*             add_mods(MOD_BIT(KC_LSHIFT)); */
            /*             return false; */
            /*         } */
            /*     } */
            /*     /\*else process LCTL_T(KC_S) as usual.*\/ */
            /*     return true; */
    }
    return true;
};
