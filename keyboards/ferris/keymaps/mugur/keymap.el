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
(setq mugur-combo-keys '(((a n) esc)
                         ((a M_r) tab)
                         ((c d) ent)))
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
(setq mugur-user-defined-keys '((to_1 (TO "l1"))
                                (to_2 (TO "l2"))
                                (to_3 (TO "l3"))
                                (to_4 (TO "l4"))
                                (to_5 (TO "l5"))
                                (M_r (M r))
                                (C_s (C s))
                                (G_t (G t))
                                (G_n (G n))
                                (C_e (C e))
                                (M_i (M i))
                                (G_\' (G ?\'))
                                (G_\+ (G ?\+))
                                (S_\\ (S ?\\))
                                (M_vu (M volu))
                                (M_mute (M mute))
                                (G_vd (G vold))
                                (G_S (G lsft))
                                (M_up (M up))
                                (G_dn (M down))
                                (n_tab C-tab)
                                (p_tab C-S-tab)
                                (mlsft (OSM S))
                                (mrsft (OSM S))
                                (malt (OSM M))
                                (mrctl (OSM C))
                                (mgui (OSM G))
                                (CMspc (C M spc))
                                (Gto1 (DANCE-FN nil
                                                (RAW "layer1_gui_finished")
                                                (RAW "layer1_gui_reset")))
                                (Gto2 (DANCE-FN nil
                                                (RAW "layer2_gui_finished")
                                                (RAW "layer2_gui_reset")))
                                (CMsft (C M lsft))
                                (Gspc (DANCE-FN nil
                                                (RAW "spc_gui_finished")
                                                (RAW "spc_gui_reset")))
                                (co_mi (DANCE-FN nil
                                                 (RAW "comma_minus_finished")
                                                 (RAW "comma_minus_reset")))
                                (dot_ex (DANCE-FN nil
                                                  (RAW "dot_exclaim_finished")
                                                  (RAW "dot_exclaim_reset")))
                                (sl_pn (DANCE-FN nil
                                                 (RAW "slsh_paren_finished")
                                                 (RAW "slsh_paren_reset")))
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
   '(("l1"
      q      w      f      p      b             j      l      u      y      bspc
      a      M_r    C_s    t      g             m      n      C_e    M_i    o
      z      x      c      d      v             k      h      co_mi  dot_ex sl_pn
      CMspc  mgui                                                    Gto2   mlsft)
     ("l2"
      gesc   ?\@    ?\#    ?\$    ?\%           ?\^    ?\&    ?\*    ?\=    ---
      tab    ?\"    ?\[    ?\(    ?\_           ?\-    ?\)    ?\]    ?\'    ent
      ?\`    ?\|    ?\{    ?\:    ?\<           ?\>    ?\;    ?\}    ?\\    to_4
      ---    Gto1                                                    to_3   malt)
     ("l3"
      gesc   mply   mprv   mnxt   ?\_           ?\-    7      8      9      ---
      tab    M_mute vold   volu   ?\+           ?\=    4      5      6      ent
      td_r   past   brid   briu   ?\.           0      1      2      3      mlsft
      ---    Gto1                                                    to_4   malt)
     ("l4"
      esc    wh_l   ms_u   wh_r   -x-           wbak   p_tab  n_tab  wfwd   ---
      tab    ms_l   ms_d   ms_r   -x-           left   G_dn   M_up   right  ent
      -x-    btn2   wh_u   wh_d   -x-           btn1   acl0   acl1   acl2   -x-
      ---    to_1                                                    to_5    ---)
     ("l5"
      f1     f2     f3     f4     f5            f6     f7     f8     f9     f10
      -x-    -x-    -x-    -x-    f11           f12    -x-    -x-    -x-    -x-
      -x-    -x-    -x-    -x-    -x-           -x-    -x-    -x-    -x-    -x-
      ---    to_1                                                    to_2    ---))))

(provide 'keymap)
;;; keymap.el
