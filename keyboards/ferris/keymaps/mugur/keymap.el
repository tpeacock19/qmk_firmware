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
(setq mugur-combo-keys nil)
(setq mugur-tapping-term-keys '(
                                ;; (G_a 100)
                                ;; (M_r 50)
                                ;; (C_s 50)
                                ;; (S_t 25)
                                ;; (S_n 25)
                                ;; (C_e 50)
                                ;; (M_i 50)
                                ;; (G_o 100)
                                ;; (u_k 150)
                                ;; (u_n 150)
                                ;; (m_v 150)
                                ))
(setq mugur-user-defined-keys '((to_1 (TG "l1"))
                                (to_2 (TG "l2"))
                                (to_3 (TG "l3"))
                                (to_4 (TG "l4"))
                                (C_esc (C e))
                                (G_s (G s))
                                (M_t (M t))
                                (M_n (M n))
                                (G_e (G e))
                                (S_vu (S volu))
                                (G_vd (G vold))
                                (G_S (G lsft))
                                (M_up (M up))
                                (G_dn (M down))
                                (n_tab C-tab)
                                (p_tab C-S-tab)
                                (spcG (G spc))
                                (mlsft (OSM S))
                                (mrsft (OSM S))
                                (mrctl (OSM C))
                                (tap_pr (RAW "DT_PRNT"))
                                (tap_up (RAW "DT_UP"))
                                (tap_dn (RAW "DT_DOWN"))
                                (asrp (RAW "KC_ASRP"))
                                (asup (RAW "KC_ASUP"))
                                (asdn (RAW "KC_ASDN"))
                                (qt_td (DANCE-FN nil
                                                 (RAW "ql_finished")
                                                 (RAW "ql_reset")))
                                (td_r (DANCE-FN (RAW "safe_reset")))))

(let ((mugur-keyboard-name "ferris")
      (mugur-layout-name "LAYOUT")
      (mugur-keymap-name "mugur")
      (mugur-tapping-term 200)
      (mugur-combo-term 50)
      (mugur-leader-timeout 200)
      (mugur-leader-per-key-timing nil))
  (mugur-mugur
   '(
     ("l1"
      q      w      f      p      b             j      l      u      y      bspc
      a      r      G_s    M_t    g             m      M_n    G_e    i      o
      x      c      d      v      z             k      h      ?\,    ?\.    ?\/
      to_2   mlsft                                                   mrctl  spcG
      )
     ("l2"
      gesc   ?\@    ?\#    ?\$    ?\%           ?\^    ?\&    ?\*    ?\;    bspc
      tab    ?\=    ?\]    ?\}    ?\'           ?\\    ?\{    ?\}    ?\|    ent
      ?\~    -x-    -x-    -x-    ?\"           ?\[    ?\(    ?\)    ?\]    to_4
      to_3   lalt                                                    ralt   to_1)
     ("l3"
      ---    mply   mprv   mnxt   ?\_           ?\-    7      8      9      ---
      G_S    mute   G vd   G vu   ?\+           ?\=    4      5      6      ---
      td_r   past   brid   briu   ?\.           0      1      2      3      ---
      to_4   lgui                                                    ---    to_1)
     ("l4"
      q      wh_l   ms_u   wh_r   t             wbak   p_tab  n_tab  wfwd   ---
      a      ms_l   ms_d   ms_r   g             left   G_dn   M_up   right  ---
      z      btn2   wh_u   wh_d   ---           btn1   acl0   acl1   acl2   -x-
      to_2   ---                                                     ---    to_1)
     )))

(provide 'keymap)
;;; keymap.el
