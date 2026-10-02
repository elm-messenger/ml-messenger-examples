(* Small UI pieces shared by the scenes: keyboard-hint chips and buttons, all
   set in Space Mono so text widths can be computed (0.604 em per glyph).

   Sizes here are em sizes. A textbox's own size is the font's line height
   (the atlas's lineHeight, 71 units for a 48-unit em in Space Mono), so it is
   scaled before drawing. *)

open Ml_regl_core
open Regl_builtin_programs

let mono = "mono"
let mono_bold = "mono_bold"
let display = "display"

let utf8_length s =
  let n = ref 0 in
  String.iter (fun ch -> if Char.code ch land 0xC0 <> 0x80 then incr n) s;
  !n

let line_per_em = 71. /. 48.
let mono_width em text = 0.604 *. em *. float (utf8_length text)

(* Text whose capitals are centred on [cy]. *)
let mono_text ?(font = mono) (x, cy) em text color =
  textbox (x, cy -. (em *. 0.78)) (em *. line_per_em) text font color

let mono_centered ?(font = mono) (cx, cy) size text color =
  mono_text ~font (cx -. (mono_width size text /. 2.), cy) size text color

type chip = { key : string; label : string; x : float; y : float; w : float; h : float }

let key_size = 13.
let label_size = 15.
let pad = 10.

let chip_width key label =
  pad +. mono_width key_size key +. 14. +. 8. +. mono_width label_size label +. pad

(* Chips laid out right to left, ending at [right]. *)
let chips_from_right ~right ~y ~h items =
  let _, chips =
    List.fold_left
      (fun (r, acc) (key, label) ->
        let w = chip_width key label in
        (r -. w -. 10., { key; label; x = r -. w; y; w; h } :: acc))
      (right, []) (List.rev items)
  in
  chips

let inside (mx, my) c = mx >= c.x && mx <= c.x +. c.w && my >= c.y && my <= c.y +. c.h

let draw_chip ?(hover = false) ?(accent = Pal.gold) c =
  let cy = c.y +. (c.h /. 2.) in
  let kw = mono_width key_size c.key +. 14. in
  Regl_common.group []
    [
      rounded_rect
        (c.x +. (c.w /. 2.), cy)
        (c.w, c.h) 8.
        (if hover then Pal.panel_edge else Pal.with_alpha Pal.panel_edge 0.55);
      rounded_rect (c.x +. pad +. (kw /. 2.), cy) (kw, c.h -. 12.) 5. accent;
      mono_text ~font:mono_bold (c.x +. pad +. 7., cy) key_size c.key Pal.ink;
      mono_text (c.x +. pad +. kw +. 8., cy) label_size c.label
        (if hover then Pal.cream else Pal.mix Pal.cream Pal.cream_dim 0.4);
    ]

(* A big button with a centred label and an optional key hint. *)
type button = { text : string; hint : string; bx : float; by : float; bw : float; bh : float }

let button_inside (mx, my) b =
  mx >= b.bx && mx <= b.bx +. b.bw && my >= b.by && my <= b.by +. b.bh

let draw_button ?(focus = false) ?(accent = Pal.gold) b =
  let cx = b.bx +. (b.bw /. 2.) and cy = b.by +. (b.bh /. 2.) in
  let size = 19. in
  let tw = mono_width size b.text in
  let hw = if b.hint = "" then 0. else mono_width 12. b.hint +. 22. in
  let x0 = cx -. ((tw +. hw) /. 2.) in
  Regl_common.group []
    [
      (if focus then rounded_rect (cx, cy) (b.bw +. 6., b.bh +. 6.) 13. accent else empty);
      rounded_rect (cx, cy) (b.bw, b.bh) 11. (if focus then Pal.panel_edge else Pal.panel);
      mono_text ~font:mono_bold (x0, cy) size b.text (if focus then Pal.cream else Pal.cream_dim);
      (if b.hint = "" then empty
       else
         Regl_common.group []
           [
             rounded_rect
               (x0 +. tw +. 12. +. ((hw -. 12.) /. 2.), cy)
               (hw -. 12., 22.) 5.
               (Pal.with_alpha accent (if focus then 1. else 0.5));
             mono_text ~font:mono_bold (x0 +. tw +. 17., cy) 12. b.hint Pal.ink;
           ]);
    ]
