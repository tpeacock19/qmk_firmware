/*
Copyright 2019 Khader Syed

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#include QMK_KEYBOARD_H
#include "g/keymap_combo.h"

enum tpeacock19_layers { _COLEMAK, _QWERTY, _FNM, _NUM, _MDIA};

enum tpeacock19_keycodes { COLEMAK = SAFE_RANGE, QWERTY };


/***********************************************************************/
/*                            Tap Dancing                             */
/***********************************************************************/

// Define a type for as many tap dance states as you need
typedef enum {
  TD_NONE,
  TD_UNKNOWN,
  TD_SINGLE_TAP,
  TD_SINGLE_HOLD,
  TD_DOUBLE_TAP
} td_state_t;

typedef struct {
  bool is_press_action;
  td_state_t state;
} td_tap_t;

enum {QUOT_LAYR, TD_RESET, TD_SPC};

// Function associated with all tap dances
td_state_t cur_dance(qk_tap_dance_state_t *state);

// Functions associated with individual tap dances
void ql_finished(qk_tap_dance_state_t *state, void *user_data);
void ql_reset(qk_tap_dance_state_t *state, void *user_data);

/***********************************************************************/
/*                              Aliases                               */
/***********************************************************************/
#define DOLLAR KC_DOLLAR
#define LSQUIGLY KC_LBRACKET
#define RSQUIGLY KC_RBRACKET
#define NUMLOCK KC_NUMLOCK
#define CAPLOCK KC_CAPSLOCK
#define BK_SLASH KC_BSLASH
#define ASTRSK KC_KP_ASTERISK

/*  One Shot Modifiers */
#define CTRL_ESC CTL_T(KC_ESC)
#define CTRL_F CTL_T(KC_F)
#define CTRL_J CTL_T(KC_J)
#define CTRL_Z CTL_T(KC_Z)
#define CA_CAPS LCA_T(KC_CAPS)
#define ALT_V ALT_T(KC_V)
#define ALT_M ALT_T(KC_M)
#define WIN_G GUI_T(KC_G)
#define WIN_H GUI_T(KC_H)
#define HYPER_X ALL_T(KC_X)
#define HYPE_DOT ALL_T(KC_DOT)
#define MEH_S MEH_T(KC_S)
#define MEH_L MEH_T(KC_L)
#define ALT_HOME ALT_T(KC_HOME)

#define GRAVE_MODS  (MOD_BIT(KC_LSHIFT)|MOD_BIT(KC_RSHIFT)|MOD_BIT(KC_LGUI)|MOD_BIT(KC_RGUI)|MOD_BIT(KC_LALT)|MOD_BIT(KC_RALT))
#define FNM MO(_FNM)
#define NUM TG(_NUM)
#define MDIA TG(_MDIA)

/***********************************************************************/
/*                           Home Row Mods                           */
/***********************************************************************/
// Left-hand home row mods
#define GUI_A LGUI_T(KC_A)
#define ALT_R LALT_T(KC_R)
#define CTL_S LCTL_T(KC_S)
#define SHFT_T LSFT_T(KC_T)

// Right-hand home row mods
#define SFT_N RSFT_T(KC_N)
#define CTL_E RCTL_T(KC_E)
#define ALT_I LALT_T(KC_I)
#define GUI_O RGUI_T(KC_O)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  /* Default Colemak Layer (0)
   * ,--------------------------------------------------------------------------------------------------.
   * | Esc |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  0  |  -  |  =  | Backspace |  | `   |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * | Tab    |  Q  |  W  |  F  |  P  |  B  |  J  |  L  |  U  |  Y  |  ;  |  [  |  ]  |  Bksl  |  | Del |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |Ctrl(Esc)|  A  |  R  |  S  |  T  |  G  |  M  |  N  |  E  |  I  |  O  |  '  |    Enter    |
   * |--------------------------------------------------------------------------------------------+
   * | Shift(Stk) |  X  |  C  |  D  |  V  |  Z  |  K  |  H  |  ,  |  .  |  /  | Shift(Stk)  | Up  |
   * +--------------------------------------------------------------------------------------------+-----+
   * | Caps  | Gui   | Alt  |               Space                | Alt | Ctrl | Fn    | Left| Down|Right|
   * `--------------------------------------------------------------------------------------------------´
   */
  [_COLEMAK] = LAYOUT(
                      KC_GESC      , KC_1 , KC_2 , KC_3 , KC_4       , KC_5, KC_6, KC_7 , KC_8   , KC_9  , KC_0   , KC_MINS      , KC_EQL , KC_BSPC, KC_GRV,
                      KC_TAB       , KC_Q , KC_W , KC_F , KC_P       , KC_B, KC_J, KC_L , KC_U   , KC_Y  , KC_SCLN, KC_LBRC      , KC_RBRC, KC_BSLS, KC_DEL,
                      CTRL_ESC     , GUI_A, ALT_R, CTL_S, SHFT_T     , KC_G, KC_M, SFT_N, CTL_E  , ALT_I , GUI_O  , TD(QUOT_LAYR), KC_ENT ,
                      OSM(MOD_LSFT), KC_X , KC_C , KC_D , LT(4, KC_V), KC_Z, KC_K, KC_H , KC_COMM, KC_DOT, KC_SLSH, OSM(MOD_RSFT), KC_UP  ,
                      CA_CAPS      , KC_LGUI, KC_LALT,                   TD(TD_SPC),            KC_RALT , KC_RCTL , FNM,  KC_LEFT, KC_DOWN, KC_RGHT
                      ),

  /* QWERTY Layer (1)
   * ,--------------------------------------------------------------------------------------------------.
   * | Esc |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  0  |  -  |  =  | Backspace |  | `   |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * | Tab    |  Q  |  W  |  E  |  R  |  T  |  Y  |  U  |  I  |  O  |  P  |  [  |  ]  |  Bksl  |  | Del |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * | Caps    |  A  |  S  |  D  |  F  |  G  |  H  |  J  |  K  |  L  |  ;  |  '  |    Enter    |
   * |--------------------------------------------------------------------------------------------+
   * | Shift(Stk) |  Z  |  X  |  C  |  V  |  B  |  N  |  M  |  ,  |  .  |  /  | Shift(Stk)  | Up  |
   * +--------------------------------------------------------------------------------------------+-----+
   * | Ctrl  | Gui   | Alt  |               Space                | Alt | Ctrl |  Fn   | Left| Down|Right|
   * `--------------------------------------------------------------------------------------------------´
   */
  [_QWERTY] = LAYOUT(
                     KC_GESC, KC_1,   KC_2,   KC_3,  KC_4,  KC_5,  KC_6,  KC_7,  KC_8,    KC_9,   KC_0,    KC_MINS, KC_EQL,  KC_BSPC,  KC_GRV,
                     KC_TAB,  KC_Q,   KC_W,   KC_E,  KC_R,  KC_T,  KC_Y,  KC_U,  KC_I,    KC_O,   KC_P,    KC_LBRC, KC_RBRC, KC_BSLS,  KC_DEL,
                     KC_CAPS, KC_A,   KC_S,   KC_D,  KC_F,  KC_G,  KC_H,  KC_J,  KC_K,    KC_L,   KC_SCLN, TD(QUOT_LAYR),          KC_ENT,
                     OSM(MOD_LSFT), KC_Z,   KC_X,   KC_C,  LT(4, KC_V),  KC_B,  KC_N,  KC_M,  KC_COMM, KC_DOT, KC_SLSH, OSM(MOD_RSFT),          KC_UP,
                     KC_LCTL, KC_LGUI, KC_LALT,             TD(TD_SPC),                       KC_RALT,KC_RCTL,     FNM, KC_LEFT, KC_DOWN, KC_RGHT
                     ),
  /* FN layer (2)
   * ,--------------------------------------------------------------------------------------------------.
   * | `   | F1  | F2  | F3  | F4  | F5  | F6  | F7  | F8  | F9  | F10 | F11 | F12 |       Mute|  | Vol-|
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |        |     | Prev | TGL | Next |     |     |     |PrtSc| Slck|     |     |     |      |  | Vol+|
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |         |     |     |QWRTY|COLMK|     |     |     | Home| PgUp|ScrUp|     |             |
   * |--------------------------------------------------------------------------------------------+
   * |            |     |     |     |     |     |     |     | End | PgDn|ScrDn| Mouse Btn 1 | MsU |
   * +--------------------------------------------------------------------------------------------+-----+
   * |       | Reset |      |                 NUM                |      |      |      | MsL | MsD | MsR |
   * `--------------------------------------------------------------------------------------------------´
   */
  [_FNM] = LAYOUT(
                  KC_GRV, KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_F6,  KC_F7,  KC_F8,  KC_F9,  KC_F10, KC_F11, KC_F12, KC_MUTE,     KC_VOLU,
                  _______,_______,KC_MPRV,KC_MPLY,KC_MNXT,_______,_______,_______,KC_PSCR,KC_SLCK,_______,_______,_______,KC_F20,      KC_VOLD,
                  _______,_______,_______, QWERTY,COLEMAK,_______,_______,_______,KC_HOME,KC_PGUP,KC_BRIU,_______,_______,
                  _______,_______,_______,_______,_______,_______,_______,_______,KC_END, KC_PGDN,KC_BRID,KC_BTN1,     KC_MS_U,
                  _______,  RESET,_______,                NUM,                    _______,_______,_______,     KC_MS_L,KC_MS_D,KC_MS_R
                  ),
  /* NUM layer (3)
   * ,--------------------------------------------------------------------------------------------------.
   * | `   | F1  | F2  | F3  | F4  | F5  | F6  | F7  | F8  | F9  | F10 | F11 | F12 |           |  | DT+ |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |        |     |     |     |     |     |     |  7  |  8  |  9  |  -  |     |     |        |  | DT- |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |   NUM   |     |     |QWRTY|COLMK|     |     |  4  |  5  |  6  |  +  |  *  |             |
   * |--------------------------------------------------------------------------------------------+
   * |     .      |     |     |     |     |     |     |  1  |  2  |  3  |  /  |      .   | MsU |
   * +--------------------------------------------------------------------------------------------+-----+
   * |       | Reset |      |                0                   |      |      |      | MsL | MsD | MsR |
   * `--------------------------------------------------------------------------------------------------´
   */
  [_NUM] = LAYOUT(
                  KC_GRV, KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_F6,  KC_F7,  KC_F8,  KC_F9,  KC_F10, KC_F11, KC_F12,_______,     DT_UP,
                  _______,_______,_______,_______,DT_PRNT,_______,_______,KC_7,KC_8,KC_9,KC_KP_MINUS,_______,_______,_______, DT_DOWN,
                  NUM,_______,_______, QWERTY,COLEMAK,_______,KC_0,KC_4,KC_5,KC_6,KC_KP_PLUS,ASTRSK,_______,
                  KC_DOT,_______,_______,_______,_______,_______,_______,KC_1,KC_2,KC_3,KC_KP_SLASH,KC_DOT,     KC_MS_U,
                  _______,TD(TD_RESET),_______,            KC_0,             _______,_______,_______,          KC_MS_L,KC_MS_D,KC_MS_R
                  ),
  /* MEDIA layer (4)
   * ,--------------------------------------------------------------------------------------------------.
   * | `   | F1  | F2  | F3  | F4  | F5  | F6  | F7  | F8  | F9  | F10 | F11 | F12 | AShftPrnt |  | AS- |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |        |     | Prev| TGL | Next|     |     |     |PrtSc| Slck|     |     |     |        |  | AS+ |
   * |-----------------------------------------------------------------------------------------+  +-----+
   * |         |     |     |QWRTY|COLMK|     |     |     | Prev|  Vol+| Vol-|     |            |
   * |--------------------------------------------------------------------------------------------+
   * |            |     |     |     |     |     |     |     | End | PgDn|ScrDn| Mouse Btn 1 | MsU |
   * +--------------------------------------------------------------------------------------------+-----+
   * |       | Reset |      |                 NUM                |      |      |      | MsL | MsD | MsR |
   * `--------------------------------------------------------------------------------------------------´
   */
  [_MDIA] = LAYOUT(
                   KC_GRV ,  KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_F6,  KC_F7,  KC_F8,  KC_F9,  KC_F10, KC_F11, KC_F12,KC_ASRP,     KC_ASUP,
                   _______,_______,_______,_______,_______,_______,_______,_______,KC_MUTE,_______,_______,_______,_______,_______,     KC_ASDN,
                   MDIA,_______,_______,_______,_______,_______,_______,KC_MPRV,KC_VOLD,KC_VOLU,KC_MNXT,_______,_______,
                   _______,_______,_______,_______,_______,_______,_______,_______,_______,_______,_______,_______,        _______,
                   _______,_______,_______,                KC_MPLY,                    _______,_______,_______,    _______,_______,_______
                   )
};

/* This is a list of user defined functions. F(N) corresponds to item N
   of this list.
*/

// Determine the current tap dance state
td_state_t cur_dance(qk_tap_dance_state_t *state) {
  if (state->count == 1) {
    if (!state->pressed) return TD_SINGLE_TAP;
    else return TD_SINGLE_HOLD;
  } else if (state->count == 2) return TD_DOUBLE_TAP;
  else return TD_UNKNOWN;
}

// Initialize tap structure associated with example tap dance key
static td_tap_t ql_tap_state = {
  .is_press_action = true,
  .state = TD_NONE
};

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
}

void ql_reset(qk_tap_dance_state_t *state, void *user_data) {
  // If the key was held down and now is released then switch off the layer
  if (ql_tap_state.state == TD_SINGLE_HOLD) {
    layer_off(_FNM);
  }
  ql_tap_state.state = TD_NONE;
}

// Associate our tap dance key with its functionality
qk_tap_dance_action_t tap_dance_actions[] = {
  [QUOT_LAYR] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, ql_finished, ql_reset),
  [TD_RESET] = ACTION_TAP_DANCE_DOUBLE(KC_LGUI, RESET),
  [TD_SPC] = ACTION_TAP_DANCE_DOUBLE(KC_SPC, KC_ENT)
};

uint16_t get_tapping_term(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
  case LT(4, KC_V):
    return TAPPING_TERM + 150;
  case TD(QUOT_LAYR):
    return TAPPING_TERM + 150;
  case GUI_T(KC_SPC):
    return TAPPING_TERM + 1250;
  default:
    return TAPPING_TERM;
  }
}

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
  case LT(0,KC_NO):
    if (record->tap.count && record->event.pressed) {
      tap_code16(C(KC_C)); // Intercept tap function to send Ctrl-C
    } else if (record->event.pressed) {
      tap_code16(C(KC_V)); // Intercept hold function to send Ctrl-V
    }
    return false;
  case RCTL_T(KC_E):
    /*
      This piece of code nullifies the effect of Right Shift when tapping
      the RCTL_T(KC_E) key.
      This helps rolling over RSFT_T(KC_N) and RCTL_T(KC_E)
      to obtain the intended "en" instead of "N".
      Consequently, capital N can only be obtained by tapping RCTL_T(KC_E)
      and holding LSFT_T(KC_T) (which is the left Shift mod tap).
    */

    /*
      Detect the tap.
      We're only interested in overriding the tap behaviour
      in a certain cicumstance. The hold behaviour can stay the same.
    */
    if (record->event.pressed && record->tap.count > 0) {
      // Detect right Shift
      if (get_mods() & MOD_BIT(KC_RSHIFT)) {
        // temporarily disable right Shift
        // so that we can send KC_N and KC_E
        // without Shift on.
        unregister_mods(MOD_BIT(KC_RSHIFT));
        tap_code(KC_N);
        tap_code(KC_E);
        // restore the mod state
        add_mods(MOD_BIT(KC_RSHIFT));
        // to prevent QMK from processing RCTL_T(KC_E) as usual in our special case
        return false;
      }
    }
    /*else process RCTL_T(KC_E) as usual.*/
    return true;

  case LCTL_T(KC_S):
    /*
      This piece of code nullifies the effect of Left Shift when
      tapping the LCTL_T(KC_S) key.
      This helps rolling over LSFT_T(KC_T) and LCTL_T(KC_S)
      to obtain the intended "st" instead of "T".
      Consequently, capital T can only be obtained by tapping LCTL_T(KC_S)
      and holding RSFT_T(KC_N) (which is the right Shift mod tap).
    */

    if (record->event.pressed && record->tap.count > 0) {
      if (get_mods() & MOD_BIT(KC_LSHIFT)) {
        unregister_mods(MOD_BIT(KC_LSHIFT));
        tap_code(KC_T);
        tap_code(KC_S);
        add_mods(MOD_BIT(KC_LSHIFT));
        return false;
      }
    }
    /*else process LCTL_T(KC_S) as usual.*/
    return true;
  }
  return true;
};
