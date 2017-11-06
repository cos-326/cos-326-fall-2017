type markup = Ital | Bold | Font of string

type elt =
    Words of string list
  | Formatted of markup * elt

type doc = elt list

let d = [ Formatted (Bold, Formatted (Bold, Formatted (Font "Arial", Words ["Chapter";"One"]))); Words ["It";"was";"a";"dark";"&";"stormy";"night.";"A"]; Formatted (Ital, Words ["shot"]); Words ["rang";"out."]]

let mapmarkup (m:string -> string) up =
  match up with
      Font f -> Font (m f)
    | Ital | Bold -> up

let maparialtocourier (font:string) =
  match font with
      "Arial" -> "Courier"
    | _ -> font

let chmarkup (m:markup) : markup = mapmarkup maparialtocourier m

let rec chfont (el:elt) : elt =
  match el with
      Formatted (m,e) -> Formatted (chmarkup m,chfont e)
    | Words _ -> el

let rec mapdoc (m:elt -> elt) (elts:doc) : doc =
  match elts with
      [] -> []
    | hd :: tl -> (m hd) :: (mapdoc m tl)

let rec chfonts (elts:doc) : doc = mapdoc chfont elts

let rec simplifyElt (el:elt) : elt =
  match el with
      Formatted (m1, Formatted (m2, e)) -> if m1 == m2 then Formatted (m2,e) else el
    | Formatted _ | Words _ -> el

let rec simplifyElts (elts:doc) : doc = mapdoc simplifyElt elts

type publication = 
    Journal of string * int * int
  | Book of string * string
  | Paper

type bibliography_entry = Entry of string * string * publication
