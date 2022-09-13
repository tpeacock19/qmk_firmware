;;; keymap.el --- tpeacock19 fc660c keymap -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Trey Peacock
;;
;; Author: Trey Peacock <https://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: May 06, 2022
;; Modified: May 06, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/keymap
;; Package-Requires: ((emacs 29.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; tpeacock19 fc660c keymap
;;
;;; Code:

(setq mugur-qmk-path "~/qmk_firmware")
(setq mugur-combo-keys '(((G_a M_r) tab)
                         ((G_a S_n) esc)))
;; (setq mugur-combo-keys nil)

(setq mugur-tapping-term-keys '(
                                ;; (G_a 100)
                                ;; (M_r 50)
                                ;; (C_s 50)
                                ;; (S_t 25)
                                ;; (S_n 25)
                                ;; (C_e 50)
                                ;; (M_i 50)
                                ;; (G_o 100)
                                (u_k 150)
                                (u_n 150)
                                (m_v 150)
                                (qt_td nil)))

(setq mugur-user-defined-keys '((fnm (MO _fnm))
                                (num (TG _num))
                                (mdia (TG _mdia))
                                (util (TG _util))
                                (C_esc (C esc))
                                (G_a (G a))
                                (M_r (M r))
                                (C_s (C s))
                                (S_t (S t))
                                (S_n (S n))
                                (C_e (C e))
                                (M_i (M i))
                                (G_o (G o))
                                (m_v (LT _mdia v))
                                (u_k (LT _util k))
                                (u_n (LT _util n))
                                (mlsft (OSM S))
                                (mrsft (OSM S))
                                (b_b (RAW "KC_WFWD"))
                                (b_f (RAW "KC_WBAK"))
                                (tap_pr (RAW "DT_PRNT"))
                                (tap_up (RAW "DT_UP"))
                                (tap_dn (RAW "DT_DOWN"))
                                (asrp (RAW "KC_ASRP"))
                                (asup (RAW "KC_ASUP"))
                                (asdn (RAW "KC_ASDN"))
                                (qt_td (DANCE-FN nil
                                                 (RAW "ql_finished")
                                                 (RAW "ql_reset")))
                                (spc_td (DANCE spc ent))
                                (qwty (RAW "QWERTY"))
                                (clmk (RAW "COLEMAK"))
                                (td_r (DANCE-FN (RAW "safe_reset") ))))

(let ((mugur-keyboard-name "fc660c")
      (mugur-layout-name "LAYOUT")
      (mugur-keymap-name "mugur")
      (mugur-tapping-term 200)
      (mugur-combo-term 50)
      (mugur-leader-timeout 200)
      (mugur-leader-per-key-timing nil))
  (mugur-mugur
   '(
     ("_colemak"
      gesc   1      2      3      4      5      6      7      8      9      0      ?\-    ?\=    bspc          grv
      tab    q      w      f      p      b      j      l      u      y      ?\;    ?\[    ?\]    bsls          del
      C_esc  G_a    M_r    C_s    S_t    g      m      S_n    C_e    M_i    G_o    qt_td         ent
      mlsft  x      c      d      m_v    z      u_k    h      ?\,    ?\.    ?\/                  mrsft  up
      caps   G      M                           spc_td                      ralt   rctl   fnm    left   down   right
      )
     ("_qwerty"
      gesc   1      2      3      4      5      6      7      8      9      0      ?\-    ?\=    bspc          grv
      tab    q      w      e      r      t      y      u      i      o      p      ?\[    ?\]    bsls          del
      C_esc  a      s      d      f      g      h      j      k      l      ?\;    qt_td         ent
      mlsft  z      x      c      m_v    b      u_n    m      ?\,    ?\.    ?\/                  mrsft  up
      caps   G      M                           spc_td                      ralt   rctl   fnm    left   down   right
      )
     ("_fnm"
      grv    f1     f2     f3     f4     f5     f6     f7     f8     f9     f10    f11    f12    ---           ---
      b_b    b_f    ---    ---    ---    ---    ---    ---    pscr   slck   ---    ---    ---    f20           ---
      ---    ---    ---    qwty   clmk   ---    ---    ---    home   pgup   ---    ---           ---
      ---    ---    copy   ---    paste  ---    ---    ---    end    pgdn   ---                  btn1   ms_u
      debug  td_r   ---                         ---                         ---    ---    ---    ms_l   ms_d   ms_r
      )
     ("_num"
      grv    f1     f2     f3     f4     f5     f6     f7     f8     f9     f10    f11    f12    tap_pr        tap_up
      ---    ---    ---    ---    ---    ---    ---    7      8      9      mins   ---    ---    ---           tap_dn
      num    ---    ---    qwty   clmk   ---    ---    4      5      6      plus   past          ---
      ?\.    ---    ---    ---    ---    ---    ---    1      2      3      ?\/                  ?\.    ms_u
      debug  td_r   ---                         0                           ---    ---    ---    ms_l   ms_d   ms_r
      )
     ("_mdia"
      ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    asrp          asup
      ---    ---    ---    ---    ---    ---    ---    ---    mute   ---    ---    ---    ---    ---           asdn
      mdia   ---    ---    ---    ---    ---    ---    mprv   vold   volu   mnxt   ---           ---
      ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---                  ---    ms_u
      ---    ---    ---                         mply                        ---    ---    ---    ms_l   ms_d   ms_r
      )
     ("_util"
      ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---           ---
      ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---    ---           ---
      util   ---    brid   briu   ---    ---    ---    ---    ---    ---    ---    ---           ---
      ---    ---    copy   ---    paste  ---    ---    ---    ---    ---    ---                  ---    ---
      ---    ---    ---                         ---                         ---    ---    ---    ---    ---    ---
      ))))

(provide 'keymap)
;;; keymap.el
