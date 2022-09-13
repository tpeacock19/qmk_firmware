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

#include "action.h"
#include "action_code.h"
#include "action_layer.h"
#include "action_tapping.h"
#include "action_util.h"
#include "config.h"
#include "definitions/mods.h"
#include "keycode.h"
#include "quantum.h"
#include "quantum_keycodes.h"
#include "timer.h"
#include "tpeacock19.h"

// Define a type for as many tap dance states as you need
typedef enum { TD_NONE, TD_UNKNOWN, SINGLE_INT, SINGLE_TAP, SINGLE_HOLD, DOUBLE_HOLD, DOUBLE_TAP, DOUBLE_SINGLE_TAP, MORE_TAPS } td_state_t;

// Use LTOSL in your keymap.

typedef struct {
    bool       is_press_action;
    td_state_t state;
} td_tap_t;

uint16_t key_timer;
bool     one_shot_layer;

// Functions associated with individual tap dances
void safe_reset(qk_tap_dance_state_t *state, void *user_data);
void comma_minus_finished(qk_tap_dance_state_t *state, void *user_data);
void comma_minus_reset(qk_tap_dance_state_t *state, void *user_data);
void comma_paren_finished(qk_tap_dance_state_t *state, void *user_data);
void comma_paren_reset(qk_tap_dance_state_t *state, void *user_data);
void dot_minus_finished(qk_tap_dance_state_t *state, void *user_data);
void dot_minus_reset(qk_tap_dance_state_t *state, void *user_data);
void dot_exclaim_finished(qk_tap_dance_state_t *state, void *user_data);
void dot_exclaim_reset(qk_tap_dance_state_t *state, void *user_data);
void slsh_exclaim_finished(qk_tap_dance_state_t *state, void *user_data);
void slsh_exclaim_reset(qk_tap_dance_state_t *state, void *user_data);
void slsh_paren_finished(qk_tap_dance_state_t *state, void *user_data);
void slsh_paren_reset(qk_tap_dance_state_t *state, void *user_data);
void l1_tt_finished(qk_tap_dance_state_t *state, void *user_data);
void l1_tt_reset(qk_tap_dance_state_t *state, void *user_data);
void l2_tt_finished(qk_tap_dance_state_t *state, void *user_data);
void l2_tt_reset(qk_tap_dance_state_t *state, void *user_data);
void l3_tt_finished(qk_tap_dance_state_t *state, void *user_data);
void l3_tt_reset(qk_tap_dance_state_t *state, void *user_data);
void l4_tt_finished(qk_tap_dance_state_t *state, void *user_data);
void l4_tt_reset(qk_tap_dance_state_t *state, void *user_data);

 // START-MUGUR-REGION

 #include QMK_KEYBOARD_H
#include "version.h"


/* Macros */


/* Tap Dances */

enum {DANCE_1, DANCE_2, DANCE_3, DANCE_4, DANCE_5, DANCE_6};

qk_tap_dance_action_t tap_dance_actions[] = {
	[DANCE_1] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, comma_paren_finished, comma_paren_reset),
	[DANCE_2] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, dot_minus_finished, dot_minus_reset),
	[DANCE_3] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, slsh_exclaim_finished, slsh_exclaim_reset),
	[DANCE_4] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, l4_tt_finished, l4_tt_reset),
	[DANCE_5] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, l1_tt_finished, l1_tt_reset),
	[DANCE_6] = ACTION_TAP_DANCE_FN(safe_reset)
};

/* Leader Keys */


/* Combos */
enum {KC_T__OSM_MOD_LSFT, OSL_L2__OSL_L3, TO_L1__TD_DANCE_5, TD_DANCE_6__TO_L4, KC_N__KC_SPACE, KC_I__KC_SPACE, KC_M__KC_SPACE, KC_C__KC_SPACE, KC_V__KC_SPACE, REPEAT__KC_SPACE, KC_D__KC_V, KC_T__KC_G, KC_M__KC_N, KC_G__KC_M, KC_K__KC_H}; 

const uint16_t PROGMEM kc_t__osm_mod_lsft[] = {KC_T, OSM(MOD_LSFT), COMBO_END};
const uint16_t PROGMEM osl_l2__osl_l3[] = {OSL(L2), OSL(L3), COMBO_END};
const uint16_t PROGMEM to_l1__td_dance_5[] = {TO(L1), TD(DANCE_5), COMBO_END};
const uint16_t PROGMEM td_dance_6__to_l4[] = {TD(DANCE_6), TO(L4), COMBO_END};
const uint16_t PROGMEM kc_n__kc_space[] = {KC_N, KC_SPACE, COMBO_END};
const uint16_t PROGMEM kc_i__kc_space[] = {KC_I, KC_SPACE, COMBO_END};
const uint16_t PROGMEM kc_m__kc_space[] = {KC_M, KC_SPACE, COMBO_END};
const uint16_t PROGMEM kc_c__kc_space[] = {KC_C, KC_SPACE, COMBO_END};
const uint16_t PROGMEM kc_v__kc_space[] = {KC_V, KC_SPACE, COMBO_END};
const uint16_t PROGMEM repeat__kc_space[] = {REPEAT, KC_SPACE, COMBO_END};
const uint16_t PROGMEM kc_d__kc_v[] = {KC_D, KC_V, COMBO_END};
const uint16_t PROGMEM kc_t__kc_g[] = {KC_T, KC_G, COMBO_END};
const uint16_t PROGMEM kc_m__kc_n[] = {KC_M, KC_N, COMBO_END};
const uint16_t PROGMEM kc_g__kc_m[] = {KC_G, KC_M, COMBO_END};
const uint16_t PROGMEM kc_k__kc_h[] = {KC_K, KC_H, COMBO_END};

combo_t key_combos[15] = {
[KC_T__OSM_MOD_LSFT] = COMBO(kc_t__osm_mod_lsft, KC_ESCAPE),
[OSL_L2__OSL_L3] = COMBO(osl_l2__osl_l3, TO(L4)),
[TO_L1__TD_DANCE_5] = COMBO(to_l1__td_dance_5, TO(L5)),
[TD_DANCE_6__TO_L4] = COMBO(td_dance_6__to_l4, TO(L5)),
[KC_N__KC_SPACE] = COMBO(kc_n__kc_space, KC_ENTER),
[KC_I__KC_SPACE] = COMBO(kc_i__kc_space, LCTL(KC_TAB)),
[KC_M__KC_SPACE] = COMBO(kc_m__kc_space, LCTL(LSFT(KC_TAB))),
[KC_C__KC_SPACE] = COMBO(kc_c__kc_space, KC_COPY),
[KC_V__KC_SPACE] = COMBO(kc_v__kc_space, KC_PASTE),
[REPEAT__KC_SPACE] = COMBO(repeat__kc_space, OSM(MOD_LSFT)),
[KC_D__KC_V] = COMBO(kc_d__kc_v, KC_ESCAPE),
[KC_T__KC_G] = COMBO(kc_t__kc_g, KC_TAB),
[KC_M__KC_N] = COMBO(kc_m__kc_n, KC_ENTER),
[KC_G__KC_M] = COMBO(kc_g__kc_m, CAPSWRD),
[KC_K__KC_H] = COMBO(kc_k__kc_h, KC_BSPACE)
};

/* Layer Codes and Matrix */
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
	[L1] = LAYOUT(KC_Q, KC_W, KC_F, KC_P, KC_B, KC_J, KC_L, KC_U, KC_Y, REPEAT, KC_A, KC_R, KC_S, KC_T, KC_G, KC_M, KC_N, KC_E, KC_I, KC_O, KC_Z, KC_X, KC_C, KC_D, KC_V, KC_K, KC_H, TD(DANCE_1), TD(DANCE_2), TD(DANCE_3), OSL(L3), OSM(MOD_LSFT), KC_SPACE, OSL(L2)),

	[L2] = LAYOUT(KC_BACKSLASH, KC_GRAVE, KC_HASH, KC_COLON, KC_SEMICOLON, KC_EQUAL, KC_7, KC_8, KC_9, KC_PLUS, KC_ESCAPE, KC_QUOTE, KC_LEFT_CURLY_BRACE, KC_LEFT_PAREN, KC_LEFT_BRACKET, KC_ASTERISK, KC_4, KC_5, KC_6, KC_MINUS, OSM(MOD_LSFT), KC_DOT, KC_RIGHT_CURLY_BRACE, KC_RIGHT_PAREN, KC_RIGHT_BRACKET, KC_0, KC_1, KC_2, KC_3, KC_TRNS, TO(L1), KC_ENTER, KC_BSPACE, TD(DANCE_4)),

	[L3] = LAYOUT(KC_NO, KC_WWW_BACK, KC_UP, LCTL(LSFT(KC_TAB)), KC_NO, KC_NO, LCTL(KC_TAB), KC_NO, KC_WWW_FORWARD, LSFT(KC_TAB), KC_ESCAPE, KC_LEFT, KC_DOWN, KC_RIGHT, KC_LGUI, OSM(MOD_LSFT), OSM(MOD_LCTL), OSM(MOD_LGUI), OSM(MOD_LALT), KC_TAB, KC_UNDO, KC_CUT, KC_COPY, KC_PASTE, KC_NO, KC_NO, KC_BSPACE, KC_DELETE, LGUI(KC_DOT), TO(EMACS), TD(DANCE_5), KC_TAB, KC_ENTER, TO(L4)),

	[L4] = LAYOUT(KC_NO, KC_MS_WH_DOWN, KC_MS_UP, KC_NO, KC_BRIGHTNESS_DOWN, KC_BRIGHTNESS_UP, KC_NO, KC_NO, KC_MS_WH_UP, KC_NO, KC_ESCAPE, KC_MS_LEFT, KC_MS_DOWN, KC_MS_RIGHT, KC_NO, KC_LEFT, KC_MEDIA_PREV_TRACK, KC_MEDIA_PLAY_PAUSE, KC_MEDIA_NEXT_TRACK, KC_AUDIO_MUTE, KC_NO, KC_MS_WH_LEFT, KC_NO, KC_NO, KC_AUDIO_VOL_DOWN, KC_AUDIO_VOL_UP, KC_MS_BTN1, KC_MS_BTN2, KC_MS_WH_RIGHT, KC_F20, TO(L1), KC_TRNS, KC_ENTER, TO(L5)),

	[L5] = LAYOUT(KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_F10, KC_F7, KC_F8, KC_F9, KC_NO, KC_NO, OSM(MOD_LALT), OSM(MOD_LGUI), OSM(MOD_LCTL), KC_NO, KC_F11, KC_F4, KC_F5, KC_F6, KC_DELETE, TD(DANCE_6), KC_NO, KC_NO, KC_NO, KC_NO, KC_F12, KC_F1, KC_F2, KC_F3, KC_NO, TO(L1), KC_NO, KC_NO, TO(L2)),

	[EMACS] = LAYOUT(LCTL(KC_G), KC_NO, KC_NO, KC_TRNS, KC_NO, KC_TRNS, KC_NO, KC_TRNS, KC_TRNS, REPEAT, KC_TAB, KC_NO, LCTL(LALT(KC_R)), LCTL(LALT(KC_T)), KC_TRNS, LCTL(LALT(KC_SPACE)), LCTL(LALT(KC_F)), LCTL(LALT(KC_B)), LCTL(LALT(LGUI(KC_U))), KC_ENTER, LCTL(LALT(KC_A)), KC_TRNS, LALT(KC_MINUS), LCTL(LALT(KC_K)), KC_TRNS, LCTL(LALT(KC_U)), LCTL(LALT(KC_D)), LCTL(KC_LEFT_ANGLE_BRACKET), LCTL(KC_RIGHT_ANGLE_BRACKET), LCTL(LALT(KC_E)), TO(L1), KC_TRNS, KC_TRNS, TO(L2))
};

/* Per Key Tapping Terms */
 

// END-MUGUR-REGION 

/***********************************************************************/
/*                        Tap Dance Functions                          */
/***********************************************************************/

td_state_t cur_dance(qk_tap_dance_state_t *state) {
    if (state->count == 1) {
        if (state->interrupted) {
            return SINGLE_TAP;
        } else {
            if (!state->pressed)
                return SINGLE_TAP;
            else
                return SINGLE_HOLD;
        }
    } else if (state->count == 2) {
        if (state->interrupted)
            return DOUBLE_SINGLE_TAP;
        else if (state->pressed)
            return DOUBLE_HOLD;
        else
            return DOUBLE_TAP;
    } else
        return MORE_TAPS;
}

// This works well if you want this key to work as a "fast modifier". It
// favors being held over being tapped.
int hold_cur_dance(qk_tap_dance_state_t *state) {
    if (state->count == 1) {
        if (state->interrupted) {
            if (!state->pressed)
                return SINGLE_TAP;
            else
                return SINGLE_HOLD;
        } else {
            if (!state->pressed)
                return SINGLE_TAP;
            else
                return SINGLE_HOLD;
        }
    }
    // If count = 2, and it has been interrupted - assume that user is
    // trying to type the letter associated with single tap.
    else if (state->count == 2) {
        if (state->pressed)
            return DOUBLE_HOLD;
        else
            return DOUBLE_TAP;
    } else
        return MORE_TAPS;
}

// Initialize tap structure associated with example tap dance key
static td_tap_t l2_tt_tap_state = {.is_press_action = true, .state = TD_NONE};

void l2_tt_finished(qk_tap_dance_state_t *state, void *user_data) {
    l2_tt_tap_state.state = cur_dance(state);
    /* static uint32_t tap_deadline = 0; */
    switch (l2_tt_tap_state.state) {
        /* tap_deadline = timer_read32() + 200; // Set 200 ms tap deadline. */
        case SINGLE_TAP:
        case SINGLE_INT:
            set_oneshot_layer(L2, ONESHOT_START);
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            break;
        case SINGLE_HOLD:
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            layer_on(L2);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
        case DOUBLE_HOLD:
            layer_move(L2);
            break;
        default:
            break;
    }
};
void l2_tt_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (l2_tt_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            /* clear_oneshot_layer_state(ONESHOT_PRESSED); */
            break;
        case SINGLE_HOLD:
            layer_off(L2);
            break;
        default:
            break;
    }
    l2_tt_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t l3_tt_tap_state = {.is_press_action = true, .state = TD_NONE};

void l3_tt_finished(qk_tap_dance_state_t *state, void *user_data) {
    l3_tt_tap_state.state = cur_dance(state);
    switch (l3_tt_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            set_oneshot_layer(L3, ONESHOT_START);
            break;
        case SINGLE_HOLD:
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            layer_on(L3);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
        case DOUBLE_HOLD:
            layer_move(L3);
            break;
        default:
            break;
    }
};

void l3_tt_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (l3_tt_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            break;
        case SINGLE_HOLD:
            layer_off(L3);
            break;
        default:
            break;
    }
    l3_tt_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t l1_tt_tap_state = {.is_press_action = true, .state = TD_NONE};

void l1_tt_finished(qk_tap_dance_state_t *state, void *user_data) {
    l1_tt_tap_state.state = cur_dance(state);
    switch (l1_tt_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
        case SINGLE_HOLD:
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            layer_move(L3);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
        case DOUBLE_HOLD:
            layer_off(L3);
            layer_move(L1);
            break;
        default:
            break;
    }
};

void l1_tt_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (l1_tt_tap_state.state) {
        default:
            break;
    }
    l1_tt_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t l4_tt_tap_state = {.is_press_action = true, .state = TD_NONE};

void l4_tt_finished(qk_tap_dance_state_t *state, void *user_data) {
    l4_tt_tap_state.state = cur_dance(state);
    switch (l4_tt_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
        case SINGLE_HOLD:
            clear_oneshot_layer_state(ONESHOT_PRESSED);
            layer_move(L2);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
        case DOUBLE_HOLD:
            layer_off(L2);
            layer_move(L4);
            break;
        default:
            break;
    }
};

void l4_tt_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (l4_tt_tap_state.state) {
        default:
            break;
    }
    l4_tt_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t comma_minus_tap_state = {.is_press_action = true, .state = TD_NONE};

void comma_minus_finished(qk_tap_dance_state_t *state, void *user_data) {
    comma_minus_tap_state.state = hold_cur_dance(state);
    switch (comma_minus_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_COMM);
            break;
        case SINGLE_HOLD:
            tap_code16(KC_MINS);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_COMM);
            register_code(KC_COMM);
            break;
        default:
            break;
    }
};

void comma_minus_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (comma_minus_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            last_keycode = KC_COMM;
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_COMM;
            unregister_code(KC_COMM);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_MINS;
            break;
        default:
            break;
    }
    comma_minus_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t dot_minus_tap_state = {.is_press_action = true, .state = TD_NONE};

void dot_minus_finished(qk_tap_dance_state_t *state, void *user_data) {
    dot_minus_tap_state.state = cur_dance(state);
    switch (dot_minus_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_DOT);
            break;
        case SINGLE_HOLD:
            tap_code16(KC_MINS);
            break;
        case DOUBLE_TAP:
            // Double tapping TD_DOT produces
            // ". <one-shot-shift>" i.e. dot, space and capitalize next letter.
            // This helps to quickly end a sentence and begin another one
            // without having to hit shift.
            /* Check that Shift is inactive */
            if (!(get_mods() & MOD_MASK_SHIFT)) {
                tap_code(KC_DOT);
                tap_code(KC_SPC);
                /* Internal code of OSM(MOD_LSFT) */
                add_oneshot_mods(MOD_BIT(KC_LSHIFT));
            } else {
                // send ">" (KC_DOT + shift → ">")
                tap_code(KC_DOT);
                tap_code(KC_DOT);
            }
            break;
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_DOT);
            register_code16(KC_DOT);
        case MORE_TAPS:
            // Since `sentence_end` is called on each tap
            // and not at the end of the tapping term,
            // the third tap needs to cancel the effects
            // of the double tap in order to get the expected
            // three dots ellipsis.
            // remove the added space of the double tap case
            tap_code(KC_BSPC);
            // replace the space with a second dot
            tap_code(KC_DOT);
            // tap the third dot
            tap_code(KC_DOT);
            break;
        default:
            break;
    }
};

void dot_minus_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (dot_minus_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
        case DOUBLE_TAP:
        case MORE_TAPS:
            last_keycode = KC_DOT;
            break;
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_DOT;
            unregister_code16(KC_DOT);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_MINS;
            break;
        default:
            break;
    }
    dot_minus_tap_state.state = TD_NONE;
};

// Initialize tap structure associated with example tap dance key
static td_tap_t dot_exclaim_tap_state = {.is_press_action = true, .state = TD_NONE};

void dot_exclaim_finished(qk_tap_dance_state_t *state, void *user_data) {
    dot_exclaim_tap_state.state = cur_dance(state);
    switch (dot_exclaim_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_DOT);
            break;
        case SINGLE_HOLD:
            tap_code16(KC_EXCLAIM);
            break;
        case DOUBLE_TAP:
            // Double tapping TD_DOT produces
            // ". <one-shot-shift>" i.e. dot, space and capitalize next letter.
            // This helps to quickly end a sentence and begin another one
            // without having to hit shift.
            /* Check that Shift is inactive */
            if (!(get_mods() & MOD_MASK_SHIFT)) {
                tap_code(KC_DOT);
                tap_code(KC_SPC);
                /* Internal code of OSM(MOD_LSFT) */
                add_oneshot_mods(MOD_BIT(KC_LSHIFT));
            } else {
                // send ">" (KC_DOT + shift → ">")
                tap_code(KC_DOT);
                tap_code(KC_DOT);
            }
            break;
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_DOT);
            register_code16(KC_DOT);
        case MORE_TAPS:
            // Since `sentence_end` is called on each tap
            // and not at the end of the tapping term,
            // the third tap needs to cancel the effects
            // of the double tap in order to get the expected
            // three dots ellipsis.
            // remove the added space of the double tap case
            tap_code(KC_BSPC);
            // replace the space with a second dot
            tap_code(KC_DOT);
            // tap the third dot
            tap_code(KC_DOT);
            break;
        default:
            break;
    }
};

void dot_exclaim_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (dot_exclaim_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
        case DOUBLE_TAP:
        case MORE_TAPS:
            last_keycode = KC_DOT;
            break;
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_DOT;
            unregister_code16(KC_DOT);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_EXCLAIM;
            break;
        default:
            break;
    }
    dot_exclaim_tap_state.state = TD_NONE;
};

static td_tap_t comma_paren_tap_state = {.is_press_action = true, .state = TD_NONE};

void comma_paren_finished(qk_tap_dance_state_t *state, void *user_data) {
    comma_paren_tap_state.state = cur_dance(state);
    switch (comma_paren_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_COMM);
            break;
        case SINGLE_HOLD:
            register_code16(KC_LEFT_PAREN);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_COMM);
            register_code16(KC_SLASH);
            break;
        default:
            break;
    }
};

void comma_paren_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (comma_paren_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            last_keycode = KC_COMM;
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_COMM;
            unregister_code16(KC_COMM);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_LEFT_PAREN;
            unregister_code16(KC_LEFT_PAREN);
            break;
        default:
            break;
    }
    comma_paren_tap_state.state = TD_NONE;
};

static td_tap_t slsh_exclaim_tap_state = {.is_press_action = true, .state = TD_NONE};

void slsh_exclaim_finished(qk_tap_dance_state_t *state, void *user_data) {
    slsh_exclaim_tap_state.state = cur_dance(state);
    switch (slsh_exclaim_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_SLASH);
            break;
        case SINGLE_HOLD:
            register_code16(KC_EXCLAIM);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_SLASH);
            register_code16(KC_SLASH);
            break;
        default:
            break;
    }
};

void slsh_exclaim_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (slsh_exclaim_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            last_keycode = KC_SLASH;
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_SLASH;
            unregister_code16(KC_SLASH);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_EXCLAIM;
            unregister_code16(KC_EXCLAIM);
            break;
        default:
            break;
    }
    slsh_exclaim_tap_state.state = TD_NONE;
};
static td_tap_t slsh_paren_tap_state = {.is_press_action = true, .state = TD_NONE};

void slsh_paren_finished(qk_tap_dance_state_t *state, void *user_data) {
    slsh_paren_tap_state.state = cur_dance(state);
    switch (slsh_paren_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            tap_code(KC_SLASH);
            break;
        case SINGLE_HOLD:
            register_code16(KC_LEFT_PAREN);
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            tap_code(KC_SLASH);
            register_code16(KC_SLASH);
            break;
        default:
            break;
    }
};

void slsh_paren_reset(qk_tap_dance_state_t *state, void *user_data) {
    // If the key was held down and now is released then switch off the layer
    switch (slsh_paren_tap_state.state) {
        case SINGLE_TAP:
        case SINGLE_INT:
            last_keycode = KC_SLASH;
            break;
        case DOUBLE_TAP:
        case DOUBLE_SINGLE_TAP:
            last_keycode = KC_SLASH;
            unregister_code16(KC_SLASH);
            break;
        case SINGLE_HOLD:
            last_keycode = KC_LEFT_PAREN;
            unregister_code16(KC_LEFT_PAREN);
            break;
        default:
            break;
    }
    slsh_paren_tap_state.state = TD_NONE;
};

void safe_reset(qk_tap_dance_state_t *state, void *user_data) {
    if (state->count >= 3) {
        // Reset the keyboard if you tap the key more than three times
        reset_keyboard();
        reset_tap_dance(state);
    }
}

void oneshot_layer_changed_user(uint8_t layer) {
    if (layer == 1) {
        one_shot_layer = true;
    }
    if (!layer) {
        one_shot_layer = false;
    }
}

bool caps_word_press_user(uint16_t keycode) {
    switch (keycode) {
        // Keycodes that continue Caps Word, with shift applied.
        case KC_A ... KC_Z:
        case KC_MINS:
        case TD(DANCE_1):
        case TD(DANCE_2):
        case TD(DANCE_3):
            add_weak_mods(MOD_BIT(KC_LSFT)); // Apply shift to next key.
            return true;

        // Keycodes that continue Caps Word, without shifting.
        case KC_1 ... KC_0:
        case KC_BSPC:
        case KC_DEL:
        case KC_UNDS:
        case REPEAT:
            return true;

        default:
            return false; // Deactivate Caps Word.
    }
}

uint16_t get_tapping_term(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
            /* case TD(DANCE_2): */
            /* case TD(DANCE_3): */
            /* return TAPPING_TERM - 50; */
            /* return TAPPING_TERM - 100; */
        case SFT_T(KC_SPC):
            return TAPPING_TERM + 1250;
        case LT(1, KC_GRV):
            return 130;
        default:
            return TAPPING_TERM;
    }
}

bool get_tapping_force_hold(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case LT(1, KC_BSPC):
            return true;
        default:
            return false;
    }
};

// Used to extract the basic tapping keycode from a dual-role key.
// Example: GET_TAP_KC(MT(MOD_RSFT, KC_E)) == KC_E
#define GET_TAP_KC(dual_role_key) dual_role_key & 0xFF
uint16_t last_keycode  = KC_NO;
uint8_t  last_modifier = 0;
bool     is_caps_word_on(void); /**< Gets whether currently active. */

void process_repeat_key(uint16_t keycode, const keyrecord_t *record) {
    if (keycode != REPEAT) {
        // Early return when holding down a pure layer key
        // to retain modifiers
        switch (keycode) {
            case QK_DEF_LAYER ... QK_DEF_LAYER_MAX:
            case QK_MOMENTARY ... QK_MOMENTARY_MAX:
            case QK_LAYER_MOD ... QK_LAYER_MOD_MAX:
            case QK_ONE_SHOT_LAYER ... QK_ONE_SHOT_LAYER_MAX:
            case QK_TOGGLE_LAYER ... QK_TOGGLE_LAYER_MAX:
            case QK_TO ... QK_TO_MAX:
            case QK_LAYER_TAP_TOGGLE ... QK_LAYER_TAP_TOGGLE_MAX:
                return;
        }
        last_modifier = oneshot_mod_state > mod_state ? oneshot_mod_state : mod_state;
        if (is_caps_word_on()) {
            last_modifier = MOD_BIT(KC_LSFT);
        }
        switch (keycode) {
            case QK_LAYER_TAP ... QK_LAYER_TAP_MAX:
            case QK_MOD_TAP ... QK_MOD_TAP_MAX:
                if (record->event.pressed) {
                    last_keycode = GET_TAP_KC(keycode);
                }
                break;
            default:
                if (record->event.pressed) {
                    last_keycode = keycode;
                }
                break;
        }
    } else { // keycode == REPEAT
        if (record->event.pressed) {
            register_mods(last_modifier);
            register_code16(last_keycode);
        } else {
            unregister_code16(last_keycode);
            unregister_mods(last_modifier);
        }
    }
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    process_repeat_key(keycode, record);
    // It's important to update the mod variables *after* calling process_repeat_key, or else
    // only a single modifier from the previous key is repeated (e.g. Ctrl+Shift+T then Repeat produces Shift+T)
    mod_state         = get_mods();
    oneshot_mod_state = get_oneshot_mods();

    if (!process_achordion(keycode, record)) {
        return false;
    }
    if ((keycode >= QK_MOD_TAP && keycode <= QK_MOD_TAP_MAX) || (keycode >= QK_LAYER_TAP && keycode <= QK_LAYER_TAP_MAX) || (keycode >= QK_MODS && keycode <= QK_MODS_MAX)) {
        keycode = keycode & 0xFF;
    }
    if (keycode == KC_ESC && record->event.pressed) {
        bool    rc   = true;
        uint8_t mods = 0;
        if ((mods = get_oneshot_mods()) && !has_oneshot_mods_timed_out()) {
            clear_oneshot_mods();
            unregister_mods(mods);
            rc = false;
        }
        if ((mods = get_oneshot_locked_mods())) {
            clear_oneshot_locked_mods();
            unregister_mods(mods);
            rc = false;
        }
        if (is_oneshot_layer_active()) {
            layer_clear();
            rc = false;
        }
        tap_code(KC_ESCAPE);
        return rc;
    }
    switch (keycode) {
        case TG(L1):
            if (record->event.pressed) {
                set_single_persistent_default_layer(L1);
            }
    }
    return true;
};

static bool key_left_hand(keypos_t pos) {
    return pos.row < MATRIX_ROWS / 2;
}

bool achordion_split_opposite_hands(const keyrecord_t *tap_hold_record, const keyrecord_t *other_record) {
    return key_left_hand(tap_hold_record->event.key) != key_left_hand(other_record->event.key);
}

bool achordion_chord(uint16_t tap_hold_keycode, keyrecord_t *tap_hold_record, uint16_t other_keycode, keyrecord_t *other_record) {
    if (tap_hold_keycode == MT(MOD_LCTL | MOD_LALT, KC_SPACE)) {
        return true;
    }
    if (other_keycode < KC_Z) {
        return true;
    }
    return true;
    /* return achordion_split_opposite_hands(tap_hold_record, other_record); */
}

void matrix_scan_user(void) {
    achordion_task();
}
