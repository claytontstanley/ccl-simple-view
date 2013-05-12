#-:clozure (error "This file only works with Clozure Common Lisp and not RMCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

; ----------------------------------------------------------------------
; Defining color-symbol->system-color and system-color->symbol for CCL.
;
; These functions may have been in the base CCL distribution, but I couldn't find them.
; So I searched online for a table of color names -> RGB mappings, threw that
; data into a bash shell, cleaned up the text, and then pasted it here. A few lisp
; parentheses were wrapped around that, which turned the data into a lexical closure.
; ----------------------------------------------------------------------

(let ((rgb-list
        (list
          'Grey (list 84 84 84)
          'grey (list 190 190 190)
          'gray (list 190 190 190)
          'LightGray (list 211 211 211)
          'Light-Gray (list 211 211 211)
          'LightSlateGrey (list 119 136 153)
          'dark-gray (list 169 169 169)
          'SlateGray (list 112 128 144)
          'black (list 0 0 0)
          'AliceBlue (list 240 248 255)
          'BlueViolet (list 138 43 226)
          'CadetBlue (list 95 158 160)
          'CadetBlue (list 95 158 160)
          'CornflowerBlue (list 100 149 237)
          'DarkSlateBlue (list 72 61 139)
          'DarkTurquoise (list 0 206 209)
          'DeepSkyBlue (list 0 191 255)
          'DodgerBlue (list 30 144 255)
          'LightBlue (list 173 216 230)
          'Light-Blue (list 173 216 230)
          'LightCyan (list 224 255 255)
          'LightSkyBlue (list 135 206 250)
          'LightSlateBlue (list 132 112 255)
          'LightSteelBlue (list 176 196 222)
          'Aquamarine (list 112 219 147)
          'MediumBlue (list 0 0 205)
          'MediumSlateBlue (list 123 104 238)
          'MediumTurquoise (list 72 209 204)
          'MidnightBlue (list 25 25 112)
          'NavyBlue (list 0 0 128)
          'PaleTurquoise (list 175 238 238)
          'PowderBlue (list 176 224 230)
          'RoyalBlue (list 65 105 225)
          'SkyBlue (list 135 206 235)
          'SlateBlue (list 106 90 205)
          'SteelBlue (list 70 130 180)
          'aquamarine (list 127 255 212)
          'azure (list 240 255 255)
          'blue (list 0 0 255)
          'aqua (list 0 255 255)
          'cyan (list 0 255 255)
          'navy (list 0 0 128)
          'teal (list 0 128 128)
          'turquoise (list 64 224 208)
          'DarkSlateGray (list 47 79 79)
          'Iris (list 3 180 200)
          'RosyBrown (list 188 143 143)
          'SaddleBrown (list 139 69 19)
          'SandyBrown (list 244 164 96)
          'beige (list 245 245 220)
          'brown (list 165 42 42)
          'brown (list 166 42 42)
          'burlywood (list 222 184 135)
          'chocolate (list 210 105 30)
          'peru (list 205 133 63)
          'tan (list 210 180 140)
          'Sienna (list 142 107 35)
          'Tan (list 219 147 112)
          'DarkGreen (list 0 100 0)
          'dark-green (list 0 100 0)
          'DarkKhaki (list 189 183 107)
          'DarkOliveGreen (list 85 107 47)
          'olive (list 128 128 0)
          'DarkSeaGreen (list 143 188 143)
          'ForestGreen (list 34 139 34)
          'GreenYellow (list 173 255 47)
          'LawnGreen (list 124 252 0)
          'LightSeaGreen (list 32 178 170)
          'LimeGreen (list 50 205 50)
          'MediumSeaGreen (list 60 179 113)
          'MediumSpringGreen (list 0 250 154)
          'MintCream (list 245 255 250)
          'OliveDrab (list 107 142 35)
          'PaleGreen (list 152 251 152)
          'SpringGreen (list 0 255 127)
          'YellowGreen (list 154 205 50)
          'chartreuse (list 127 255 0)
          'green (list 0 255 0)
          'green (list 0 128 0)
          'lime (list 0 255 0)
          'khaki (list 240 230 140)
          'DarkOrange (list 255 140 0)
          'DarkSalmon (list 233 150 122)
          'LightCoral (list 240 128 128)
          'LightSalmon (list 255 160 122)
          'PeachPuff (list 255 218 185)
          'bisque (list 255 228 196)
          'coral (list 255 127 0)
          'coral (list 255 127 80)
          'honeydew (list 240 255 240)
          'orange (list 255 165 0)
          'salmon (list 250 128 114)
          'sienna (list 160 82 45)
          'Orange (list 255 127 0)
          'DeepPink (list 255 20 147)
          'HotPink (list 255 105 180)
          'IndianRed (list 205 92 92)
          'LightPink (list 255 182 193)
          'MediumVioletRed (list 199 21 133)
          'MistyRose (list 255 228 225)
          'OrangeRed (list 255 69 0)
          'PaleVioletRed (list 219 112 147)
          'VioletRed (list 208 32 144)
          'firebrick (list 178 34 34)
          'pink (list 255 192 203)
          'Flesh (list 245 204 176)
          'Feldspar (list 209 146 117)
          'red (list 255 0 0)
          'tomato (list 255 99 71)
          'Firebrick (list 142 35 35)
          'Pink (list 188 143 143)
          'Salmon (list 111 66 66)
          'Scarlet (list 140 23 23)
          'DarkOrchid (list 153 50 204)
          'DarkViolet (list 148 0 211)
          'LavenderBlush (list 255 240 245)
          'MediumOrchid (list 186 85 211)
          'MediumPurple (list 147 112 219)
          'lavender (list 230 230 250)
          'magenta (list 255 0 255)
          'fuchsia (list 255 0 255)
          'maroon (list 176 48 96)
          'orchid (list 218 112 214)
          'Orchid (list 219 112 219)
          'plum (list 221 160 221)
          'purple (list 160 32 240)
          'purple (list 128 0 128)
          'thistle (list 216 191 216)
          'violet (list 238 130 238)
          'Maroon (list 128 0 0)
          'Plum (list 234 173 234)
          'Thistle (list 216 191 216)
          'Turquoise (list 173 234 234)
          'Violet (list 79 47 79)
          'AntiqueWhite (list 250 235 215)
          'FloralWhite (list 255 250 240)
          'GhostWhite (list 248 248 255)
          'NavajoWhite (list 255 222 173)
          'OldLace (list 253 245 230)
          'WhiteSmoke (list 245 245 245)
          'gainsboro (list 220 220 220)
          'ivory (list 255 255 240)
          'linen (list 250 240 230)
          'seashell (list 255 245 238)
          'snow (list 255 250 250)
          'wheat (list 245 222 179)
          'white (list 255 255 255)
          'Quartz (list 217 217 243)
          'Wheat (list 216 216 191)
          'BlanchedAlmond (list 255 235 205)
          'DarkGoldenrod (list 184 134 11)
          'LemonChiffon (list 255 250 205)
          'LightGoldenrod (list 238 221 130)
          'LightGoldenrodYellow (list 250 250 210)
          'LightYellow (list 255 255 224)
          'PaleGoldenrod (list 238 232 170)
          'PapayaWhip (list 255 239 213)
          'cornsilk (list 255 248 220)
          'goldenrod (list 218 165 32)
          'moccasin (list 255 228 181)
          'yellow (list 255 255 0)
          'gold (list 255 215 0)
          'Goldenrod (list 219 219 112)
          'copper (list 184 115 51)
          'brass (list 181 166 66)
          'bronze (list 140 120 83)
          'CSS (list 204 153 0)
          'gold (list 205 127 50)
          'silver (list 230 232 250))))
  (defun color-symbol->rgb (symb)
    (getf rgb-list symb))
  (defun rgb->color-symbol (rgb)
    (loop for item on rgb-list by #'cddr
          do (destructuring-bind (cur-symb cur-rgb) (list (first item) (second item))
               (when (equal cur-rgb rgb)
                 (return-from rgb->color-symbol cur-symb))))))

(defun color-symbol->system-color (symb)
  (destructuring-bind (red green blue) (color-symbol->rgb symb)
    (make-color red green blue)))

(defun system-color->symbol (color)
  (let ((red (easygui:rgb-red color))
        (green (easygui:rgb-green color))
        (blue (easygui:rgb-blue color)))
    (rgb->color-symbol (list red green blue))))

; Converting MCL colors (specified as a huge (technical term) integer) to 'system' colors

; FIXME: Copied this code from CCL's src; couldn't figure out how to load it with a require;
; fix this. This code matches what's in MCL's src, so this is the actual MCL code to do the conversion

(defun make-mcl-color (red green blue)
  "given red, green, and blue, returns an encoded RGB value"
  (flet ((check-color (color)
           (unless (and (fixnump color)
                        (<= 0 (the fixnum color))
                        (<= (the fixnum color) 65535))
             (error "Illegal color component: ~s" color))))
    (declare (inline check-color))
    (check-color red)
    (check-color green)
    (check-color blue))
  (locally (declare (fixnum red green blue))
           (let* ((r (logand red #xff00))
                  (g (logand green #xff00))
                  (b (logand blue #xff00)))
             (declare (fixnum r g b))
             (logior (the fixnum (ash  r 8))
                     (the fixnum g)
                     (the fixnum (ash b -8))))))

(defun make-color (red green blue &optional (opacity 1.0))
  (easygui:make-rgb :red red :green green :blue blue :opacity opacity))

(defun color-red (color)
  (easygui:rgb-red color))

(defun color-green (color)
  (easygui:rgb-green color))

(defun color-blue (color)
  (easygui:rgb-blue color))

(defun color-opacity (color)
  (easygui:rgb-opacity color))

(defun mcl-color-red (color &optional (component (logand (the fixnum (lsh color -16)) #xff)))
  "Returns the red portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-green (color &optional (component (logand (the fixnum (lsh color -8)) #xff)))
  "Returns the green portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-blue (color &optional (component (logand color #xff)))
  "Returns the blue portion of the color"
  (declare (fixnum component))
  (the fixnum (+ (the fixnum (ash component 8)) component)))

(defun mcl-color-values (color)
  "Given an encoded color, returns the red, green, and blue components"
  (values
    (ceiling (* (/ (float (mcl-color-red color)) (float 65535)) 255))
    (ceiling (* (/ (float (mcl-color-green color)) (float 65535)) 255))
    (ceiling (* (/ (float (mcl-color-blue color)) (float 65535)) 255))))

(defun mcl-color->system-color (color)
  "Converts an MCL color to a CCL system color"
  (etypecase color
    (integer (multiple-value-bind (r g b) (mcl-color-values color)
               (make-color r g b)))
    (ns:ns-color color)))

(defparameter *black-color* (color-symbol->system-color 'black))
(defparameter *red-color* (color-symbol->system-color 'red))
(defparameter *light-gray-pattern* (color-symbol->system-color 'gray))
(defparameter *green-color* (color-symbol->system-color 'green))
(defparameter *blue-color* (color-symbol->system-color 'blue))
(defparameter *dark-green-color* (color-symbol->system-color 'DarkGreen))
(defparameter *white-color* (color-symbol->system-color 'white))
(defparameter *gray-color* (mcl-color->system-color 8421504))
(defparameter *yellow-color* (color-symbol->system-color 'yellow))
(defparameter *orange-color* (mcl-color->system-color 16737282))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :mcl-ccl-colors))
