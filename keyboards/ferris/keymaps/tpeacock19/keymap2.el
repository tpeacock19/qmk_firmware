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
;; tpeacock19 ferris sweep keymap
;;
;;; Code:

(setq mugur-enum-layers nil)
(setq mugur-qmk-path "~/qmk_firmware")
;; (setq mugur-combo-keys '(((t mlsft) esc)
;;                          ((n G_spc) ent)
;;                          ((i G_spc) n_tab)
;;                          ((m G_spc) p_tab)
;;                          ((G_spc mlsft) to_4)
;;                          ((G_spc malt) to_2)
;;                          ((mctl malt) to_1)
;;                          ((d v) esc)
;;                          ((t g) tab)
;;                          ((m n) ent)
;;                          ((k h) bspc)
;;                          ((0 1) bspc)))
(setq mugur-combo-keys '(((t mlsft) esc)
                         ((os2 os3) to_4)
                         ((to_1 (RAW "TD(DANCE_5)")) to_5)
                         (((RAW "TD(DANCE_6)") to_4) to_5)
                         ((n spc) ent)
                         ((i spc) n_tab)
                         ((m spc) p_tab)
                         ((c spc) copy)
                         ((v spc) paste)
                         ((rpt spc) (OSM S))
                         ((d v) esc)
                         ((t g) tab)
                         ((m n) ent)
                         ((g m) (RAW "CAPSWRD"))
                         ((k h) bspc)))
;; (setq mugur-tapping-term-keys '(
;;                                 (Gspc -50)
;;                                 ;; (M_r 50)
;;                                 ;; (C_s 50)
;;                                 ;; (S_t 25)
;;                                 ;; (S_n 25)
;;                                 ;; (C_e 50)
;;                                 ;; (M_i 50)
;;                                 ;; (G_o 100)
;;                                 ;; (u_k 150)
;;                                 ;; (u_n 150)
;;                                 ;; (m_v 150)
;;                                 ))

(setq mugur-user-defined-keys '((to_1 (TO "l1"))
                                (to_2 (TO "l2"))
                                (to_3 (TO "l3"))
                                (to_4 (TO "l4"))
                                (to_5 (TO "l5"))
                                (emc (TO "EMACS"))
                                (tt_2 (RAW "TT(L2)"))
                                (os2 (RAW "OSL(L2)"))
                                (os3 (RAW "OSL(L3)"))
                                (tt_3 (RAW "TT(L3)"))
                                (ltos2 (RAW "LTOSM2"))
                                (ltos3 (RAW "LTOSM3"))
                                (tt_3 (RAW "TT(L3)"))
                                (M_r (M r))

                                (CM_?\% C-M-?\%)
                                (CM_?\, C-M-?\,)
                                (CM_?\- C-M-?\-)
                                (M_mi M-mins)
                                (CM_?\. C-M-?\.)
                                (G_dot G-dot)
                                (S_tab S-tab)
                                (CM_?\/ C-M-?\/)
                                (CM_c C-M-c)
                                (CM_?\\ C-M-?\\)
                                (CM_?\_ C-M-?\_)
                                (CM_a C-M-a)
                                (CM_b C-M-b)
                                (CM_d C-M-d)
                                (CM_D C-M-S-d)
                                (CM_e C-M-e)
                                (CM_f C-M-f)
                                (CM_F C-M-S-f)
                                (C_g C-g)
                                (CM_h C-M-h)
                                (CM_j C-M-j)
                                (CM_k C-M-k)
                                (CM_l C-M-l)
                                (CM_n C-M-n)
                                (CM_o C-M-o)
                                (CM_p C-M-p)
                                (CM_r C-M-r)
                                (CM_t C-M-t)
                                (CM_u C-M-u)
                                (CMG_u C-M-G-u)
                                (CM_v C-M-v)
                                (CM_w C-M-w)
                                (CM_lt C-left_angle)
                                (CM_gt C-right_angle)

                                (CM_spc (C-M-spc))
                                (C_s (C s))
                                (G_t (G t))
                                (G_n (G n))
                                (C_e (C e))
                                (C_5 (C 5))
                                (M_6 (M 6))
                                (M_i (M i))
                                (G_\' (G ?\'))
                                (G_\+ (G ?\+))
                                (S_\\ (S ?\\))
                                (C_lbr (C ?\[))
                                (C_lcl (C ?\{))
                                (C_rbr (C ?\]))
                                (M_ap (M ?\'))
                                (M_spc (M spc))
                                (G_spc (G spc))
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
                                (mctl (OSM C))
                                (mgui (OSM G))
                                (CMspc (C M spc))
                                (GL1 (RAW "LM(L1, MOD_LGUI)"))
                                (rpt (RAW "REPEAT"))
                                (rrpt (RAW "REV_RPT"))
                                (to1 (DANCE-FN nil
                                               (RAW "l1_tt_finished")
                                               (RAW "l1_tt_reset")))
                                (tt2 (DANCE-FN nil
                                               (RAW "l2_tt_finished")
                                               (RAW "l2_tt_reset")))
                                (tt3 (DANCE-FN nil
                                               (RAW "l3_tt_finished")
                                               (RAW "l3_tt_reset")))
                                (to4 (DANCE-FN nil
                                               (RAW "l4_tt_finished")
                                               (RAW "l4_tt_reset")))
                                (Mt1 (DANCE-FN nil
                                               (RAW "meta1_finished")
                                               (RAW "meta1_reset")))
                                (Gt1 (DANCE-FN nil
                                               (RAW "gui1_finished")
                                               (RAW "gui1_reset")))
                                (Mt2 (DANCE-FN nil
                                               (RAW "meta2_finished")
                                               (RAW "meta2_reset")))
                                (Gt2 (DANCE-FN nil
                                               (RAW "gui2_finished")
                                               (RAW "gui2_reset")))
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
                                (M_qt (DANCE-FN nil
                                                (RAW "meta_dquote_finished")
                                                (RAW "meta_dquote_reset")))
                                (M_pip (DANCE-FN nil
                                                 (RAW "meta_pipe_finished")
                                                 (RAW "meta_pipe_reset")))
                                (dot_mi (DANCE-FN nil
                                                  (RAW "dot_minus_finished")
                                                  (RAW "dot_minus_reset")))
                                (co_mi (DANCE-FN nil
                                                 (RAW "comma_minus_finished")
                                                 (RAW "comma_minus_reset")))
                                (co_pn (DANCE-FN nil
                                                 (RAW "comma_paren_finished")
                                                 (RAW "comma_paren_reset")))
                                (dot_ex (DANCE-FN nil
                                                  (RAW "dot_exclaim_finished")
                                                  (RAW "dot_exclaim_reset")))
                                (sl_pn (DANCE-FN nil
                                                 (RAW "slsh_paren_finished")
                                                 (RAW "slsh_paren_reset")))
                                (sl_ex (DANCE-FN nil
                                                 (RAW "slsh_exclaim_finished")
                                                 (RAW "slsh_exclaim_reset")))
                                (qt_td (DANCE-FN nil
                                                 (RAW "ql_finished")
                                                 (RAW "ql_reset")))
                                (td_r (DANCE-FN (RAW "safe_reset")))))

(let ((mugur-keyboard-name "ferris")
      (mugur-layout-name "LAYOUT")
      (mugur-keymap-name "tpeacock19")
      (mugur-tapping-term 180)
      (mugur-combo-term 50)
      (mugur-leader-timeout 200)
      (mugur-leader-per-key-timing nil))
  (mugur-mugur
   '(( "l1"
       q      w      f      p      b             j      l      u      y      rpt
       a      r      s      t      g             m      n      e      i      o
       z      x      c      d      v             k      h      co_pn  dot_mi sl_ex
       os3    mlsft                                                   spc    os2)
     ("l2"
      ?\\    ?\`    ?\#    ?\:    ?\;           ?\=    7      8      9      ?\+
      esc    ?\'    ?\{    ?\(    ?\[           ?\*    4      5      6      ?\-
      mlsft  ?\.    ?\}    ?\)    ?\]           0      1      2      3      ---
      to_1   ent                                                     bspc   to4)
     ("l3"
      -x-    wbak   up     p_tab  -x-           -x-    n_tab  -x-    wfwd   S_tab
      esc    left   down   right  lgui          mlsft  mctl   mgui   malt   tab
      undo   cut    copy   paste  -x-           -x-    bspc   del    G_dot  emc
      to1    tab                                                     ent    to_4)
     ("l4"
      -x-    wh_d   ms_u   -x-    brid          briu   -x-    -x-    wh_u   -x-
      esc    ms_l   ms_d   ms_r   -x-           left   mprv   mply   mnxt   mute
      -x-    wh_l   -x-    -x-    vold          volu   btn1   btn2   wh_r   f20
      to_1   ---                                                     ent    to_5)
     ("l5"
      -x-    -x-    -x-    -x-    -x-           f10    f7     f8     f9     -x-
      -x-    malt   mgui   mctl   -x-           f11    f4     f5     f6     del
      td_r   -x-    -x-    -x-    -x-           f12    f1     f2     f3     -x-
      to_1   -x-                                                     -x-    to_2)
     ("emacs"
      C_g    -x-    -x-    ---    -x-           ---    -x-    ---    ---    rpt
      tab    -x-    CM_r   CM_t   ---           CM_spc CM_f   CM_b   CMG_u  ent
      CM_a   ---    M_mi   CM_k   ---           CM_u   CM_d   CM_lt  CM_gt  CM_e
      to_1   ---                                                     ---    to_2))))

(provide 'keymap)
;;; keymap.el
