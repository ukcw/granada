export const BoolUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

library BoolUtils

let andb : Bool -> Bool -> Bool = 
  fun (b : Bool) =>
  fun (c : Bool) =>
    match b with 
    | False => False
    | True  => c
    end

let orb : Bool -> Bool -> Bool = 
  fun (b : Bool) => fun (c : Bool) =>
    match b with 
    | True  => True
    | False => c
    end

let negb : Bool -> Bool = 
  fun (b : Bool) => 
    match b with
    | True => False
    | False => True
    end

let bool_to_string : Bool -> String = 
  fun (flag: Bool) =>
    match flag with
    | True => "True"
    | False => "False"
    end

let orb_3 : Bool -> Bool -> Bool -> Bool =
  fun (a : Bool) => fun (b : Bool) => fun (c : Bool) =>
    let t = orb a b in
    orb c t

let and_3 : Bool -> Bool -> Bool -> Bool =
  fun (a : Bool) => fun (b : Bool) => fun (c : Bool) =>
    let t = andb a b in
    andb c t

let orb_4 : Bool -> Bool -> Bool -> Bool -> Bool =
  fun (a : Bool) => fun (b : Bool) => fun (c : Bool) => fun (d : Bool) =>
    let t1 = orb a b in
    let t2 = orb t1 c in
    orb t2 d

let and_4 : Bool -> Bool -> Bool -> Bool -> Bool =
  fun (a : Bool) => fun (b : Bool) => fun (c : Bool) => fun (d : Bool) =>
    let t1 = andb a b in
    let t2 = andb t1 c in
    andb t2 d
`;

export const Conversions = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

import IntUtils

library Conversions

type IntegerEncoding =
  | LittleEndian
  | BigEndian

(* does not throw exceptions *)
let substr_safe : ByStr -> Uint32 -> Uint32 -> Option ByStr =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
  fun (len : Uint32) =>
    let length = builtin strlen bs in
    let shorter = uint32_le len length in
    match shorter with
    | True =>
      let delta = builtin sub length len in
      let safe_pos = uint32_le pos delta in
      match safe_pos with
      | True =>
        let substr = builtin substr bs pos len in
        Some {ByStr} substr
      | False => None {ByStr}
      end
    | False => None {ByStr}
    end

(* Extract out a Scilla type from a ByStr starting at pos. Returns next position. *)
(* Use the type specific helpers below rather than this function. *)
let extract_scillaval : forall 'A. forall 'B. (ByStr -> Option 'B) -> ('B -> 'A) -> ByStr -> Uint32 -> Uint32 -> Option (Pair 'A Uint32) =
  tfun 'A =>
  tfun 'B =>
  fun (to_bystrx : ByStr -> Option 'B) =>
  fun (to_uint : 'B -> 'A) =>
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
  fun (len : Uint32) =>         (* Byte size of the Uint value. *)
    let subbs_opt =  substr_safe bs pos len in
    match subbs_opt with
    | Some subbs =>
      let subbs_x_opt = to_bystrx subbs in
      match subbs_x_opt with
      | Some subbs_x =>
        let ui = to_uint subbs_x in
        let next_pos = builtin add pos len in
        let res = Pair {'A Uint32} ui next_pos in
        Some {(Pair 'A Uint32)} res
      | None =>
        None {(Pair 'A Uint32)}
      end
    | None =>
      None {(Pair 'A Uint32)}
    end

(* Extracts Uint32 in bs from position pos and returns next position. *)
let extract_uint32 : IntegerEncoding -> ByStr -> Uint32 -> Option (Pair Uint32 Uint32) =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr4 a in
    let to_uint =
      fun (a : ByStr4) =>
        let b = match endian with | LittleEndian => builtin strrev a | BigEndian => a end in
        builtin to_uint32 b
    in
    let extractor = @extract_scillaval Uint32 ByStr4 in
    let uint32_bsize = Uint32 4 in
    extractor to_bystrx to_uint bs pos uint32_bsize

(* Extracts Uint64 in bs from position pos and returns next position. *)
let extract_uint64 : IntegerEncoding -> ByStr -> Uint32 -> Option (Pair Uint64 Uint32) =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr8 a in
    let to_uint =
      fun (a : ByStr8) =>
        let b = match endian with | LittleEndian => builtin strrev a | BigEndian => a end in
        builtin to_uint64 b
    in
    let extractor = @extract_scillaval Uint64 ByStr8 in
    let uint64_bsize = Uint32 8 in
    extractor to_bystrx to_uint bs pos uint64_bsize

(* Extracts Uint128 in bs from position pos and returns next position. *)
let extract_uint128 : IntegerEncoding -> ByStr -> Uint32 -> Option (Pair Uint128 Uint32) =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr16 a in
    let to_uint =
      fun (a : ByStr16) =>
        let b = match endian with | LittleEndian => builtin strrev a | BigEndian => a end in
        builtin to_uint128 b
    in
    let extractor = @extract_scillaval Uint128 ByStr16 in
    let uint128_bsize = Uint32 16 in
    extractor to_bystrx to_uint bs pos uint128_bsize

(* Extracts Uint256 in bs from position pos and returns next position. *)
let extract_uint256 : IntegerEncoding -> ByStr -> Uint32 -> Option (Pair Uint256 Uint32) =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr32 a in
    let to_uint =
      fun (a : ByStr32) =>
        let b = match endian with | LittleEndian => builtin strrev a | BigEndian => a end in
        builtin to_uint256 b
    in
    let extractor = @extract_scillaval Uint256 ByStr32 in
    let uint256_bsize = Uint32 32 in
    extractor to_bystrx to_uint bs pos uint256_bsize

(* Extracts ByStr1 in bs from position pos and returns next position *)
let extract_bystr1 : ByStr -> Uint32 -> Option (Pair ByStr1 Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr1 a in
    let unit = fun (a : ByStr1) => a in
    let extractor = @extract_scillaval ByStr1 ByStr1 in
    let bystr1_bsize = Uint32 1 in
    extractor to_bystrx unit bs pos bystr1_bsize

(* Extracts ByStr2 in bs from position pos and returns next position *)
let extract_bystr2 : ByStr -> Uint32 -> Option (Pair ByStr2 Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr2 a in
    let unit = fun (a : ByStr2) => a in
    let extractor = @extract_scillaval ByStr2 ByStr2 in
    let bystr2_bsize = Uint32 2 in
    extractor to_bystrx unit bs pos bystr2_bsize

(* Extracts ByStr20 in bs from position pos and returns next position *)
let extract_bystr20 : ByStr -> Uint32 -> Option (Pair ByStr20 Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr20 a in
    let unit = fun (a : ByStr20) => a in
    let extractor = @extract_scillaval ByStr20 ByStr20 in
    let bystr20_bsize = Uint32 20 in
    extractor to_bystrx unit bs pos bystr20_bsize

(* Extracts ByStr32 in bs from position pos and returns next position *)
let extract_bystr32 : ByStr -> Uint32 -> Option (Pair ByStr32 Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let to_bystrx = fun (a : ByStr) => builtin to_bystr32 a in
    let unit = fun (a : ByStr32) => a in
    let extractor = @extract_scillaval ByStr32 ByStr32 in
    let bystr32_bsize = Uint32 32 in
    extractor to_bystrx unit bs pos bystr32_bsize

(* Append serialized Uint32 value to given byte string *)
let append_uint32 : IntegerEncoding -> ByStr -> Uint32 -> ByStr =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (ui : Uint32) =>
    let uibx = builtin to_bystr4 ui in
    let uib = builtin to_bystr uibx in
    let uib_endian =
      match endian with | LittleEndian => builtin strrev uib | BigEndian => uib end
    in
    builtin concat bs uib_endian

(* Append serialized Uint64 value to given byte string *)
let append_uint64 : IntegerEncoding -> ByStr -> Uint64 -> ByStr =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (ui : Uint64) =>
    let uibx = builtin to_bystr8 ui in
    let uib = builtin to_bystr uibx in
    let uib_endian =
      match endian with | LittleEndian => builtin strrev uib | BigEndian => uib end
    in
    builtin concat bs uib_endian

(* Append serialized Uint128 value to given byte string *)
let append_uint128 : IntegerEncoding -> ByStr -> Uint128 -> ByStr =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (ui : Uint128) =>
    let uibx = builtin to_bystr16 ui in
    let uib = builtin to_bystr uibx in
    let uib_endian =
      match endian with | LittleEndian => builtin strrev uib | BigEndian => uib end
    in
    builtin concat bs uib_endian

(* Append serialized Uint256 value to given byte string *)
let append_uint256 : IntegerEncoding -> ByStr -> Uint256 -> ByStr =
  fun (endian : IntegerEncoding) =>
  fun (bs : ByStr) =>
  fun (ui : Uint256) =>
    let uibx = builtin to_bystr32 ui in
    let uib = builtin to_bystr uibx in
    let uib_endian =
      match endian with | LittleEndian => builtin strrev uib | BigEndian => uib end
    in
    builtin concat bs uib_endian

`;

export const CryptoUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2021 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

import BoolUtils

library CryptoUtils

(* group order for the bn128_G1 curve *)
let alt_bn128_G1_q = Uint256 21888242871839275222246405745257275088548364400416034343698204186575808495617

(* generator for the bn128_G1 curve *)
let alt_bn128_G1_g =
  let one = 0x0000000000000000000000000000000000000000000000000000000000000001 in
  let two = 0x0000000000000000000000000000000000000000000000000000000000000002 in
  Pair {ByStr32 ByStr32} one two

(* identity point or point at infinity for the bn128_G1 curve *)
let alt_bn128_G1_zero =
  let zero = 0x0000000000000000000000000000000000000000000000000000000000000000 in
  Pair {ByStr32 ByStr32} zero zero

(* base point multiplication *)
let alt_bn128_G1_bmul : ByStr32 -> Option (Pair (ByStr32) (ByStr32)) =
  fun (s : ByStr32) =>
  builtin alt_bn128_G1_mul alt_bn128_G1_g s

(* point equality *)
let pair_bystr32_eq : Pair ByStr32 ByStr32 -> Pair ByStr32 ByStr32 -> Bool =
  fun (p1 : Pair ByStr32 ByStr32) =>
  fun (p2 : Pair ByStr32 ByStr32) =>
  match p1 with
  | Pair x1 y1 =>
  match p2 with
  | Pair x2 y2 =>
    let b1 = builtin eq x1 x2 in
    let b2 = builtin eq y1 y2 in
    andb b1 b2
  end
  end

`;

export const IntUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

library IntUtils

let int_neq : forall 'A. ('A -> 'A -> Bool) -> 'A -> 'A -> Bool =
  tfun 'A =>
  fun (eq : 'A -> 'A -> Bool) =>
  fun (a : 'A) =>
  fun (b : 'A) =>
    let eqr = eq a b in
    match eqr with
    | True => False
    | False => True
    end

let int_le : forall 'A. ('A -> 'A -> Bool) -> ('A -> 'A -> Bool) -> 
  'A -> 'A -> Bool =
  tfun 'A =>
  fun (eq : 'A -> 'A -> Bool) =>
  fun (lt : 'A -> 'A -> Bool) =>
  fun (a : 'A) =>
  fun (b : 'A) =>
    let ltr = lt a b in
    match ltr with
    | True => True
    | False => eq a b
    end

let int_gt : forall 'A. ('A -> 'A -> Bool) -> 'A -> 'A -> Bool =
  tfun 'A =>
  fun (lt : 'A -> 'A -> Bool) =>
  fun (a : 'A) =>
  fun (b : 'A) =>
    lt b a

let int_ge : forall 'A. ('A -> 'A -> Bool) -> ('A -> 'A -> Bool) -> 
  'A -> 'A -> Bool =
  tfun 'A =>
  fun (eq : 'A -> 'A -> Bool) =>
  fun (lt : 'A -> 'A -> Bool) =>
  fun (a : 'A) =>
  fun (b : 'A) =>
    let le = @int_le 'A in
    le eq lt b a

(* int_eq instantiations *)
let int32_eq : Int32 -> Int32 -> Bool =
  fun (a : Int32) =>
  fun (b : Int32) =>
    builtin eq a b
let int64_eq : Int64 -> Int64 -> Bool =
  fun (a : Int64) =>
  fun (b : Int64) =>
    builtin eq a b
let int128_eq : Int128 -> Int128 -> Bool =
  fun (a : Int128) =>
  fun (b : Int128) =>
    builtin eq a b
let int256_eq : Int256 -> Int256 -> Bool =
  fun (a : Int256) =>
  fun (b : Int256) =>
    builtin eq a b
let uint32_eq : Uint32 -> Uint32 -> Bool =
  fun (a : Uint32) =>
  fun (b : Uint32) =>
    builtin eq a b
let uint64_eq : Uint64 -> Uint64 -> Bool =
  fun (a : Uint64) =>
  fun (b : Uint64) =>
    builtin eq a b
let uint128_eq : Uint128 -> Uint128 -> Bool =
  fun (a : Uint128) =>
  fun (b : Uint128) =>
    builtin eq a b
let uint256_eq : Uint256 -> Uint256 -> Bool =
  fun (a : Uint256) =>
  fun (b : Uint256) =>
    builtin eq a b

(* int_lt instantiations *)
let int32_lt : Int32 -> Int32 -> Bool =
  fun (a : Int32) =>
  fun (b : Int32) =>
    builtin lt a b
let int64_lt : Int64 -> Int64 -> Bool =
  fun (a : Int64) =>
  fun (b : Int64) =>
    builtin lt a b
let int128_lt : Int128 -> Int128 -> Bool =
  fun (a : Int128) =>
  fun (b : Int128) =>
    builtin lt a b
let int256_lt : Int256 -> Int256 -> Bool =
  fun (a : Int256) =>
  fun (b : Int256) =>
    builtin lt a b
let uint32_lt : Uint32 -> Uint32 -> Bool =
  fun (a : Uint32) =>
  fun (b : Uint32) =>
    builtin lt a b
let uint64_lt : Uint64 -> Uint64 -> Bool =
  fun (a : Uint64) =>
  fun (b : Uint64) =>
    builtin lt a b
let uint128_lt : Uint128 -> Uint128 -> Bool =
  fun (a : Uint128) =>
  fun (b : Uint128) =>
    builtin lt a b
let uint256_lt : Uint256 -> Uint256 -> Bool =
  fun (a : Uint256) =>
  fun (b : Uint256) =>
    builtin lt a b

(* int_neq instantiations *)
let int32_neq = let t = @int_neq Int32 in t int32_eq
let int64_neq = let t = @int_neq Int64 in t int64_eq
let int128_neq = let t = @int_neq Int128 in t int128_eq
let int256_neq = let t = @int_neq Int256 in t int256_eq
let uint32_neq = let t = @int_neq Uint32 in t uint32_eq
let uint64_neq = let t = @int_neq Uint64 in t uint64_eq
let uint128_neq = let t = @int_neq Uint128 in t uint128_eq
let uint256_neq = let t = @int_neq Uint256 in t uint256_eq
(* int_le instantiations *)
let int32_le = let t = @int_le Int32 in t int32_eq int32_lt
let int64_le = let t = @int_le Int64 in t int64_eq int64_lt
let int128_le = let t = @int_le Int128 in t int128_eq int128_lt
let int256_le = let t = @int_le Int256 in t int256_eq int256_lt
let uint32_le = let t = @int_le Uint32 in t uint32_eq uint32_lt
let uint64_le = let t = @int_le Uint64 in t uint64_eq uint64_lt
let uint128_le = let t = @int_le Uint128 in t uint128_eq uint128_lt
let uint256_le = let t = @int_le Uint256 in t uint256_eq uint256_lt
(* int_gt instantiations *)
let int32_gt = let t = @int_gt Int32 in t int32_lt
let int64_gt = let t = @int_gt Int64 in t int64_lt
let int128_gt = let t = @int_gt Int128 in t int128_lt
let int256_gt = let t = @int_gt Int256 in t int256_lt
let uint32_gt = let t = @int_gt Uint32 in t uint32_lt
let uint64_gt = let t = @int_gt Uint64 in t uint64_lt
let uint128_gt = let t = @int_gt Uint128 in t uint128_lt
let uint256_gt = let t = @int_gt Uint256 in t uint256_lt
(* int_ge instantiations *)
let int32_ge = let t = @int_ge Int32 in t int32_eq int32_lt
let int64_ge = let t = @int_ge Int64 in t int64_eq int64_lt
let int128_ge = let t = @int_ge Int128 in t int128_eq int128_lt
let int256_ge = let t = @int_ge Int256 in t int256_eq int256_lt
let uint32_ge = let t = @int_ge Uint32 in t uint32_eq uint32_lt
let uint64_ge = let t = @int_ge Uint64 in t uint64_eq uint64_lt
let uint128_ge = let t = @int_ge Uint128 in t uint128_eq uint128_lt
let uint256_ge = let t = @int_ge Uint256 in t uint256_eq uint256_lt
`;

export const ListUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

library ListUtils

let list_foldk : forall 'A. forall 'B. ('B -> 'A -> ('B -> 'B) -> 'B) -> 'B -> (List 'A) -> 'B =
  tfun 'A => tfun 'B =>
  fun (f: 'B -> 'A -> ('B -> 'B) -> 'B) =>
  fun (z : 'B) => fun (l: List 'A) =>
  let g = fun (a: 'B) => fun (b: List 'A) =>
    match b with
    | Cons h t => let partial = fun (k : 'B) => g k t in
      f a h partial
    | Nil => a
    end
  in
  g z l

let list_foldl : forall 'A. forall 'B. ( 'B -> 'A -> 'B) -> 'B -> List 'A -> 'B =
tfun 'A => tfun 'B =>
fun (f : 'B -> 'A -> 'B) =>
let left_f = fun (z: 'B) => fun (x: 'A) =>
  fun (recurse : 'B -> 'B) => let res = f z x in
  recurse res in
let folder = @list_foldk 'A 'B in
folder left_f

let list_foldr : forall 'A. forall 'B. ('A -> 'B -> 'B) -> 'B -> List 'A -> 'B =
tfun 'A => tfun 'B =>
fun (f : 'A -> 'B -> 'B) =>
let right_f = fun (z: 'B) => fun (x: 'A) =>
  fun (recurse : 'B -> 'B) => let res = recurse z in f x res in
let folder = @list_foldk 'A 'B in
folder right_f

(* list_foldl with early termination. Continues recursing as long as f returns Some _.
   Once f returns None, the recursion terminates. *)
let list_foldl_while : forall 'A. forall 'B.
  ('B -> 'A -> Option 'B) -> 'B -> List 'A -> 'B =
  tfun 'A => tfun 'B =>
  fun (f : 'B -> 'A -> Option 'B) =>
    let foldk = @list_foldk 'A 'B in
    let whilel_f = fun (z : 'B) => fun(x : 'A) => fun (recurse : 'B -> 'B) =>
      let res = f z x in
      match res with
      | Some acc => recurse acc
      | None => z
      end in
    foldk whilel_f

(* Apply (f : 'A -> 'B) to every element of List 'A *)
(* to generate List 'B. *)
let list_map : forall 'A. forall 'B. ('A -> 'B) -> List 'A -> List 'B = 
  tfun 'A => tfun 'B =>
  fun (f : 'A -> 'B) =>
  let foldr = @list_foldr 'A (List 'B) in
  let iter = fun (h : 'A) => fun (z : List 'B) =>
    let h1 = f h in
    Cons {'B} h1 z in
  let init = Nil {'B} in
  foldr iter init

(* Preserving the order of elements in (l : List 'A),  *)
(* return new list containing only those elements *)
(* that satisfy the function f. *)
let list_filter : forall 'A. ('A -> Bool) -> List 'A -> List 'A =
  tfun 'A =>
  fun (f : 'A -> Bool) =>
    let foldr = @list_foldr 'A (List 'A) in
    let iter = fun (h : 'A) => fun (z : List 'A) =>
      let h1 = f h in
      match h1 with
      | True => Cons {'A} h z
      | False => z
      end in
    let init = Nil {'A} in
    foldr iter init

(* Return the head element of a list as Some 'A, None otherwise *)
let list_head : forall 'A. List 'A -> Option 'A =
  tfun 'A =>
  fun (l : List 'A) =>
    match l with
    | Cons h t => Some {'A} h
    | Nil => None {'A}
    end

(* Return the list except for the head *)
let list_tail : forall 'A. List 'A -> Option (List 'A) =
  tfun 'A =>
  fun (l : List 'A) =>
    match l with
    | Cons _ t => Some {(List 'A)} t
    | Nil => None {(List 'A)}
    end

(* Append the second list to the first one and return a new List *)
let list_append : forall 'A. List 'A -> List 'A -> List 'A =
  tfun 'A =>
  fun (l1 : List 'A) =>
  fun (l2 : List 'A) =>
    (* Fold right over l1 and keep prepending elements to l2 *)
    let foldr = @list_foldr 'A (List 'A) in
    let iter = fun (h : 'A) => fun (z : List 'A) =>
      Cons {'A} h z in
    (* l2 is the initial accumulator *)
    foldr iter l2 l1

(* Return the reverse of the argument list *)
let list_reverse : forall 'A. List 'A -> List 'A =
  tfun 'A =>
  let foldl = @list_foldl 'A (List 'A) in
  let iter = fun (z : List 'A) => fun (h : 'A) =>
    Cons {'A} h z in
  let init = Nil {'A} in
  foldl iter init

(* Concatenate a list of lists. The elements of the argument are all *)
(* concatenated together (in the same order) to give the result. *)
let list_flatten : forall 'A. List (List 'A) -> List 'A =
  tfun 'A =>
  let foldr = @list_foldr (List 'A) (List 'A) in
  let app = @list_append 'A in
  let iter = fun (h : List 'A) => fun (z : List 'A) =>
    app h z in
  let init = Nil {'A} in
  foldr iter init

(* Number of elements in list *)
let list_length : forall 'A. List 'A -> Uint32 =
  tfun 'A =>
  let foldl = @list_foldl 'A Uint32 in
  let one = Uint32 1 in
  let iter = fun (z : Uint32) => fun (h : 'A) =>
    builtin add one z in
  let init = Uint32 0 in
  foldl iter init

(* Not for public use, to be removed in next major version *)
(* Returns Some Nil on successful match. None otherwise. *)
let list_eq_helper : forall 'A. ('A -> 'A -> Bool) ->
  List 'A -> List 'A -> Option (List 'A) =
  tfun 'A =>
  fun (eq : 'A -> 'A -> Bool) =>
  fun (l1 : List 'A) =>
  fun (l2 : List 'A) =>
    let foldl = @list_foldl 'A (Option (List 'A)) in
    let tailF = @list_tail 'A in
    let headF = @list_head 'A in
    let iter = fun (z : Option (List 'A)) => fun (h1 : 'A) =>
      match z with
      | Some ll2 =>
        let h2o = headF ll2 in
        match h2o with
        | Some h2 =>
          let eqb = eq h1 h2 in
          match eqb with
          | True => tailF ll2
          | False => None {(List 'A)}
          end
        | None => None {(List 'A)}
        end
      | None => None {(List 'A)}
      end in
    let init = Some {(List 'A)} l2 in
    foldl iter init l1

(* Return true iff two lists compare equal. *)
(* Comparison is performed using the "equal" function provided. *)
let list_eq : forall 'A. ('A -> 'A -> Bool) -> List 'A -> List 'A -> Bool =
  tfun 'A =>
  fun (equal : 'A -> 'A -> Bool) =>
  fun (l1 : List 'A) => fun (l2 : List 'A) =>
    let foldk = @list_foldk 'A (List 'A) in
    let iter =
      fun (xs2 : List 'A) => fun (x1 : 'A) =>
      fun (recurse : List 'A -> List 'A) =>
        match xs2 with
        | Cons x2 tl2 =>
          let eq_x1_x2 = equal x1 x2 in
          match eq_x1_x2 with
          | True => recurse tl2
          | False => l1   (* l1 is not empty in this context *)
          end
        | Nil => l1       (* l1 is not empty in this context *)
        end in
    let remaining = foldk iter l2 l1 in
    match remaining with
    | Cons _ _ => False
    | Nil => True
    end

(* Return True iff all elements of list "l" satisfy predicate "p". *)
let list_forall : forall 'A. ('A -> Bool) -> List 'A -> Bool =
  tfun 'A =>
  fun (p : 'A -> Bool) =>
    let foldk = @list_foldk 'A Bool in
    let sat_p =
      fun (ignore : Bool) => fun (x: 'A) =>
      fun (recurse: Bool -> Bool) =>
        let p_x = p x in
        match p_x with
        | True => recurse p_x
        | False => False
        end in
    (* vacuously true if empty list *)
    let init = True in
    foldk sat_p init

(* Stable sort the input list "l". *)
(* "flt" returns True if the first argument is lesser-than the second *)
let list_sort : forall 'A. ('A -> 'A -> Bool) -> List 'A -> List 'A =
  (* Insertion sort *)
  tfun 'A =>
  fun (flt : 'A -> 'A -> Bool) =>
  fun (ls : List 'A) =>
    let true = True in 
    let false = False in
    let rec_A_A = @list_foldr 'A (List 'A) in
    let rec_A_Pair = @list_foldr 'A (Pair Bool (List 'A))  in
    let nil_A = Nil {'A} in 
    let sink_down = fun (e : 'A) => fun (ls : List 'A) =>
      let init = Pair {Bool (List 'A)} false nil_A in
      let iter1 = fun (h : 'A) => fun (res : Pair Bool (List 'A)) =>
        match res with
        | Pair True rest =>
          let z = Cons {'A} h rest in
          Pair {Bool (List 'A)} true z
        | Pair False rest =>
          let bl = flt h e in
          match bl with
          | True =>
            let z = Cons {'A} e rest in
            let z2 = Cons {'A} h z in
            Pair {Bool (List 'A)} true z2
          | False =>
            let z = Cons {'A} h rest in
            Pair {Bool (List 'A)} false z
          end
        end in
      let res1 = rec_A_Pair iter1 init ls in
      match res1 with
      | Pair True ls1 => ls1
      | Pair False ls1 => Cons {'A} e ls1
      end in
    rec_A_A sink_down nil_A ls


(* Return Some a, where "a" is the first element of *)
(* "l" that satisfies the predicate "f". *)
(* Return None iff none of the elements in "l" satisfy "f". *)
let list_find : forall 'A. ('A -> Bool) -> List 'A -> Option 'A =
  tfun 'A =>
  fun (p : 'A -> Bool) =>
    let foldk = @list_foldk 'A (Option 'A) in
    let init = None {'A} in
    (* continue fold on None, exit fold when Some compare st. p(compare) *)
    let predicate_step =
      fun (ignore : Option 'A) => fun (x : 'A) =>
      fun (recurse: Option 'A -> Option 'A) =>
        let p_x = p x in
        match p_x with
        | True => Some {'A} x
        | False => recurse init
        end in
    foldk predicate_step init

(* Return True iff at least one element of list "l" satisfy predicate "f". *)
let list_exists : forall 'A. ('A -> Bool) -> List 'A -> Bool =
  tfun 'A =>
  fun (p : 'A -> Bool) => fun (ls: List 'A) =>
    let find = @list_find 'A in
    let search = find p ls in
    match search with
    | Some _ => True
    | None => False
    end

(* Return True iff "m" is in the list "l", as compared by function "f". *)
let list_mem : forall 'A. ('A -> 'A -> Bool) -> 'A -> List 'A -> Bool =
  tfun 'A =>
  fun (f : 'A -> 'A -> Bool) => fun (m : 'A) =>
    let ex_pred = f m in
    let ex = @list_exists 'A in
    ex ex_pred

(* Combine corresponding elements of m1 and m2 using "f" *)
(* and return the resulting list of 'C. In case of different number *)
(* of elements in the lists, the extra elements are ignored. *)
let list_zip_with : forall 'A. forall 'B. forall 'C.
  ('A -> 'B -> 'C) -> List 'A -> List 'B -> List 'C =
  tfun 'A => tfun 'B => tfun 'C =>
  fun (f : 'A -> 'B -> 'C) =>
  fun (l1: List 'A) =>
  fun (l2: List 'B) =>
    (* Fail when l1 empty, empty l2 case done by foldk *)
    let zip_step = fun (state : Pair (List 'C) (List 'A)) => fun (y : 'B) =>
      match state with
      | Pair res_rev rest_a =>
        match rest_a with
        | Cons x xs =>
          let next = f x y in
          let next_res_rev = Cons {'C} next res_rev in
          let next_rest_a = Pair { (List 'C) (List 'A) } next_res_rev xs in
          Some {(Pair (List 'C) (List 'A))} next_rest_a
        | Nil => None {(Pair (List 'C) (List 'A))}
        end
      end in
    let foldl_while = @list_foldl_while 'B (Pair (List 'C) (List 'A)) in
    let nil = Nil {'C} in
    let init = Pair {(List 'C) (List 'A)} nil l1 in
    let zipped_accompanied = foldl_while zip_step init l2 in
    let reverse = @list_reverse 'C in
    match zipped_accompanied with
    | Pair rev_res _ => reverse rev_res
    end

(* Combine corresponding elements of m1 and m2 into a pair *)
(* and return the resulting list. In case of different number *)
(* of elements in the lists, the extra elements are ignored. *)
let list_zip : forall 'A. forall 'B. List 'A -> List 'B -> List (Pair 'A 'B) =
  tfun 'A => tfun 'B =>
  let zip_with = @list_zip_with 'A 'B (Pair 'A 'B) in
  let pair_up = fun (a : 'A) => fun (b : 'B) =>
    Pair {'A 'B} a b in
  zip_with pair_up

let list_unzip : forall 'A. forall 'B.
  List (Pair 'A 'B) -> Pair (List 'A) (List 'B) =
  tfun 'A => tfun 'B =>
  let foldr = @list_foldr (Pair 'A 'B) (Pair (List 'A) (List 'B)) in
  let iter = fun (h : Pair 'A 'B) => fun (z : Pair (List 'A) (List 'B)) =>
    match h with
    | Pair a b =>
      match z with
      | Pair la lb =>
        let nla = Cons {'A} a la in
        let nlb = Cons {'B} b lb in
        Pair {(List 'A)(List 'B)} nla nlb
      end
    end in
  let nila = Nil {'A} in
  let nilb = Nil {'B} in
  let init = Pair {(List 'A) (List 'B)} nila nilb in
  foldr iter init

(* Returns (Some 'A) if n'th element exists in list. None otherwise *)
let list_nth : forall 'A. Uint32 -> List 'A -> Option 'A =
  tfun 'A =>
  fun (n : Uint32) => fun (l : List 'A) =>
    let foldk = @list_foldk 'A (Pair Uint32 (Option 'A)) in
    let none = None {'A} in
    let zero = Uint32 0 in
    let one = Uint32 1 in
    (* each step brings down counter with no result, on zero stop with result *)
    let iter =
      fun (counter : Pair Uint32 (Option 'A)) =>
      fun (x : 'A) =>
      fun (recurse: (Pair Uint32 (Option 'A)) -> (Pair Uint32 (Option 'A))) =>
        match counter with
        | Pair count _ =>
          let is_zero = builtin eq zero count in
          match is_zero with
          | True =>
            let res = Some {'A} x in
            Pair {Uint32 (Option 'A)} count res
          | False => 
            let count_sub_1 = builtin sub count one in
            let res = Pair {Uint32 (Option 'A)} count_sub_1 none in
            recurse res
          end
        end in
    let init = Pair {Uint32 (Option 'A)} n none in
    let search = foldk iter init l in
    match search with
    | Pair _ discovery => discovery
    end
`;

export const NatUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

library NatUtils

let nat_foldk : ('T -> Nat -> ('T -> 'T) -> 'T) -> 'T -> Nat -> 'T =
  tfun 'T =>
  fun (fn : ('T -> Nat -> ('T -> 'T) -> 'T)) =>
  fun (f0 : 'T) => fun (n: Nat) =>
  let g : 'T -> Nat -> 'T =
    fun (f0 : 'T) => fun (n: Nat) =>
    match n with
      | Succ n1 => let partial = fun (k : 'T) => g k n1 in
        fn f0 n partial
      | Zero => f0
    end
  in
  g f0 n

  let nat_fold : ('T -> Nat -> 'T) -> 'T -> Nat -> 'T =
  tfun 'T =>
  fun (fn: 'T -> Nat -> 'T) =>
  fun (f0 : 'T) => fun (n: Nat) =>
  let g : 'T -> Nat -> 'T =
    fun (f0: 'T) => fun (n: Nat) =>
    match n with
      | Succ n1 => let res = fn f0 n1 in
        g res n1
      | Zero => f0
    end
  in
  g f0 n

let nat_prev : Nat -> Option Nat =
  fun (n: Nat) =>
    match n with
    | Succ n1 => Some {Nat} n1
    | Zero => None {Nat}
    end

let is_some_zero : Nat -> Bool =
  fun (n: Nat) =>
    match n with
    | Zero => True
    | _ => False
    end

(* nat_fold with early termination. Continues recursing so long as f returns Some _.
   Once f returns None, the recursion terminates. *)
let nat_fold_while : forall 'T. ('T -> Nat -> Option 'T) -> 'T -> Nat -> 'T =
  tfun 'T =>
  fun (f : 'T -> Nat -> Option 'T) =>
    let foldk = @nat_foldk 'T in
    let iter = fun (z : 'T) => fun (n : Nat) =>
      fun (recurse : 'T -> 'T) =>
        let res_opt = f z n in
        match res_opt with
        | Some res => recurse res
        | None => z
        end in
    foldk iter

(* Return true iff two nats compare equal. *)
let nat_eq : Nat -> Nat -> Bool =
  fun (n : Nat) => fun (m : Nat) =>
    let foldk = @nat_foldk Nat in
    let iter =
      fun (n : Nat) => fun (ignore : Nat) => fun (recurse : Nat -> Nat) =>
        match n with
        | Succ n_pred => recurse n_pred
        | Zero => m   (* m is not zero in this context *)
        end in
    let remaining = foldk iter n m in
    match remaining with
    | Zero => True
    |   _ => False
    end

let nat_to_int : Nat -> Uint32 =
  fun (n : Nat) =>
    let fold = @nat_fold Uint32 in
    let one_int = Uint32 1 in
    let f = fun (z : Uint32) => fun (ignore : Nat) =>
      builtin add z one_int in
    let zero_int = Uint32 0 in
    fold f zero_int n

let uint32_to_nat_helper : Option Uint32 -> Option Nat =
  fun (m : Option Uint32) =>
    match m with
    | Some x =>
      let res = builtin to_nat x in
      Some {Nat} res
    | None => None {Nat}
    end

let uint32_to_nat : Uint32 -> Option Nat =
  fun (n : Uint32) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m

let uint64_to_nat : Uint64 -> Option Nat =
  fun (n : Uint64) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m

let uint128_to_nat : Uint128 -> Option Nat =
  fun (n : Uint128) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m

let int32_to_nat : Int32 -> Option Nat =
  fun (n : Int32) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m

let int64_to_nat : Int64 -> Option Nat  =
  fun (n : Int64) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m

let int128_to_nat : Int128 -> Option Nat  =
  fun (n : Int128) =>
    let m = builtin to_uint32 n in
    uint32_to_nat_helper m
`;
export const PairUtils = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

library PairUtils

let fst : forall 'A. forall 'B. Pair 'A 'B -> 'A =
  tfun 'A =>
  tfun 'B =>
  fun (p : Pair ('A) ('B)) =>
    match p with
    | Pair a b => a
    end

let snd : forall 'A. forall 'B. Pair 'A 'B -> 'B =
  tfun 'A =>
  tfun 'B =>
  fun (p : Pair ('A) ('B)) =>
    match p with
    | Pair a b => b
    end
`;

export const Polynetwork = `
(* ******************************************************************************** *)
(*   This file is part of scilla.                                                   *)
(*                                                                                  *)
(*   Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.                        *)
(*                                                                                  *)
(*   scilla is free software: you can redistribute it and/or modify it under the    *)
(*   terms of the GNU General Public License as published by the Free Software      *)
(*   Foundation, either version 3 of the License, or (at your option) any later     *)
(*   version.                                                                       *)
(*                                                                                  *)
(*   scilla is distributed in the hope that it will be useful, but WITHOUT ANY      *)
(*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR  *)
(*   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.    *)
(*                                                                                  *)
(*   You should have received a copy of the GNU General Public License along with   *)
(*   scilla.  If not, see <http://www.gnu.org/licenses/>.                           *)
(* ******************************************************************************** *)
scilla_version 0

import Conversions ListUtils BoolUtils

library Polynetwork

type Header =
  | Header of
    Uint32                        (* version *)
    Uint64                        (* chainID *)
    ByStr32                       (* prevBlockHash *)
    ByStr32                       (* transactionRoot *)
    ByStr32                       (* crossStatesRoot *)
    ByStr32                       (* blockRoot *)
    Uint32                        (* timestamp *)
    Uint32                        (* height *)
    Uint64                        (* consensusData *)
    ByStr                         (* consensusPayload *)
    ByStr20                       (* nextBookkeeper *)

type TxParam =
  | TxParam of
    ByStr                       (* txHash *)
    ByStr                       (* crossChainId *)
    ByStr                       (* fromContract *)
    Uint64                      (* toChainId *)
    ByStr                       (* toContract *)
    ByStr                       (* method *)
    ByStr                       (* args *)

type ToMerkleValue =
  | ToMerkleValue of
    ByStr                       (* txHash *)
    Uint64                      (* fromChainID *)
    TxParam                     (* makeTxParam *)

type Pubkey = | Pubkey of ByStr67
let pubkey_length = Uint32 67

type Signature = | Signature of ByStr65
let signature_length = Uint32 65

type Unit = | Unit
let unit = Unit
let zero_uint32 = Uint32 0

type Proof =
  | Proof of ByStr (List (Pair ByStr1 ByStr32))

let little_endian = LittleEndian
let extract_uint32_le = extract_uint32 little_endian
let extract_uint64_le = extract_uint64 little_endian
let extract_uint128_le = extract_uint128 little_endian
let extract_uint256_le = extract_uint256 little_endian

let append_uint32_le = append_uint32 little_endian
let append_uint64_le = append_uint64 little_endian
let append_uint128_le = append_uint128 little_endian
let append_uint256_le = append_uint256 little_endian

(* Raise an exception and return a default value. *)
let raise_exception =
  tfun 'A =>
  fun (default_val : 'A) =>
  let ignore = builtin div zero_uint32 zero_uint32 in
  default_val

let some_exn =
  tfun 'A =>
  fun (option_val : Option 'A) =>
  fun (default_val : 'A) =>
    match option_val with
    | Some val => val
    | None =>
      let exceptioner = @raise_exception 'A in
      let ignore = exceptioner default_val in
      default_val
    end

(* Return (as Uint64) the next VarUint at pos in bystr, and the next position. *)
(* ZeroCopySource.sol : NextVarUint *)
let next_var_uint : ByStr -> Uint32 -> Option (Pair Uint64 Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let first_byte_nextpos = extract_bystr1 bs pos in
    match first_byte_nextpos with
    | Some (Pair first_byte nextpos) =>
      let bit16_marker = 0xfd in
      let is_16bit = builtin eq first_byte bit16_marker in
      match is_16bit with
      | True =>
        (* Extract the next 16bits and convert to an integer. *)
        let b16_nextpos = extract_bystr2 bs nextpos in
        match b16_nextpos with
        | Some (Pair b16 nextpos) =>
          let b16_little = builtin strrev b16 in
          let b16_u32 = builtin to_uint64 b16_little in
          let res = Pair {Uint64 Uint32} b16_u32 nextpos in
          Some {(Pair Uint64 Uint32)} res
        | None =>
          None {(Pair Uint64 Uint32)}
        end
      | False =>
        (* It's either 32bits or 64bits or 8bits. *)
        let b32_marker = 0xfe in
        let is_32b = builtin eq first_byte b32_marker in
        match is_32b with
        | True =>
          (* This is a 32b integer *)
          let uint32val_nextpos = extract_uint32_le bs nextpos in
          match uint32val_nextpos with
          | Some (Pair uint32val nextpos) =>
            (* Upcast to a Uint64 value. *)
            let uint64val_opt = builtin to_uint64 uint32val in
            match uint64val_opt with
            | Some uint64val =>
              let res = Pair {Uint64 Uint32} uint64val nextpos in
              Some {(Pair Uint64 Uint32)} res
            | None => None {(Pair Uint64 Uint32)}
            end
          | None => None {(Pair Uint64 Uint32)}
          end
        | False =>
          (* 64b or 8bits. *)
          let b64_marker = 0xff in
          let is_64b = builtin eq first_byte b64_marker in
          match is_64b with
          | True =>
            extract_uint64_le bs nextpos
          | False =>
            let u8_val = builtin to_uint64 first_byte in
            let res = Pair {Uint64 Uint32} u8_val nextpos in
            Some {(Pair Uint64 Uint32)} res
          end
        end
      end
    | None =>
      None {(Pair Uint64 Uint32)}
    end

(* Serialize and append to given byte string, a Uint64 as a VarInt value. *)
let append_var_uint : ByStr -> Uint64 -> ByStr =
  fun (bs : ByStr) =>
  fun (ui : Uint64) =>
    let uibystrx = builtin to_bystr8 ui in
    let uibystr = builtin to_bystr uibystrx in
    let uint8_limit = Uint64 253 in (* 0xfd *)
    let uint16_limit = Uint64 65536 in (* uint16_max + 1 *)
    let uint32_limit =  Uint64 4294967296 in (* Uint32_max + 1 *)
    let fits_in_byte1 = builtin lt ui uint8_limit in
    let res =
      match fits_in_byte1 with
      | True =>
        (* ui < 0xfd. Write just a byte and be done. *)
        let pos = Uint32 7 in
        let len = Uint32 1 in
        (* Extract out the last byte; our integers are big-endian. *)
        builtin substr uibystr pos len
      | False =>
        (* Does the value fit in Uint16? ui <= 0xffff *)
        let fits_in_byte2 = builtin lt ui uint16_limit in
        match fits_in_byte2 with
        | True =>
          let markerx = 0xfd in
          let marker = builtin to_bystr markerx in
          (* Extract out the last two bytes; our integers are big-endian. *)
          let pos = Uint32 6 in
          let len = Uint32 2 in
          let ui16_bigendian = builtin substr uibystr pos len in
          let ui16_littleendian = builtin strrev ui16_bigendian in
          builtin concat marker ui16_littleendian
        | False =>
          (* Does the value fit in Uint32? ui <= 0xffffffff *)
          let fits_in_byte4 = builtin lt ui uint32_limit in
          match fits_in_byte4 with
          | True =>
            let markerx = 0xfe in
            let marker = builtin to_bystr markerx in
            (* Extract out the last four bytes; our integers are big-endian. *)
            let pos = Uint32 4 in
            let len = Uint32 4 in
            let ui32_bigendian = builtin substr uibystr pos len in
            let ui32_littleendian = builtin strrev ui32_bigendian in
            builtin concat marker ui32_littleendian
          | False =>
            let markerx = 0xff in
            let marker = builtin to_bystr markerx in
            let ui64_littleendian = builtin strrev uibystr in
            builtin concat marker ui64_littleendian
          end
        end
      end
    in
    builtin concat bs res

(* Extract a Bystr value start at pos. See ZeroCopySource.sol:NextVarBytes. *)
let extract_bystr : ByStr -> Uint32 -> Option (Pair ByStr Uint32) =
  fun (bs : ByStr) =>
  fun (pos : Uint32) =>
    let bystr_len_64_opt = next_var_uint bs pos in
    match bystr_len_64_opt with
    | Some (Pair bystr_len_64 nextpos) =>
      let bystr_len_32_opt = builtin to_uint32 bystr_len_64 in
      match bystr_len_32_opt with
      | Some bystr_len_32 =>
        let some = fun (a : ByStr) => Some {ByStr} a in
        let unit = fun (a : ByStr) => a in
        let extractor = @extract_scillaval ByStr ByStr in
        extractor some unit bs nextpos bystr_len_32
      | None => None {(Pair ByStr Uint32)}
      end
    | None => None {(Pair ByStr Uint32)}
    end

(* Append to first byte string, serialized form of the second byte string. *)
(* See ZeroCopySink.WriteVarBytes. *)
let append_varbytes : ByStr -> ByStr -> ByStr =
  fun (bs : ByStr) =>
  fun (varby : ByStr) =>
    let sommer = @some_exn Uint64 in
    let exnobj = Uint64 0 in
    let len_32b = builtin strlen varby in
    let len_64b_opt = builtin to_uint64 len_32b in
    let len = sommer len_64b_opt exnobj in
    let varint_bs = append_var_uint bs len in
    builtin concat varint_bs varby

(* Downcast Uint32 to Uint16, serialize it and append to given byte string *)
let append_uint16 : ByStr -> Uint32 -> ByStr =
  fun (bs : ByStr) =>
  fun (ui : Uint32) =>
    let uibx = builtin to_bystr4 ui in
    let uib = builtin to_bystr uibx in
    let pos = Uint32 2 in
    let len = Uint32 2 in
    let uib_sub2 = builtin substr uib pos len in
    let uib_sub2_littleendian = builtin strrev uib_sub2 in
    builtin concat bs uib_sub2_littleendian

(* Serialize TxParam and append to given byte string. *)
let append_TxParam : ByStr -> TxParam -> ByStr =
  fun (bs : ByStr) =>
  fun (txp : TxParam) =>
    match txp with
    | TxParam txHash crossChainId fromContract toChainId toContract method args =>
      let t1 = append_varbytes bs txHash in
      let t2 = append_varbytes t1 crossChainId in
      let t3 = append_varbytes t2 fromContract in
      let t4 = append_uint64_le t3 toChainId in
      let t7 = append_varbytes t4 toContract in
      let t8 = append_varbytes t7 method in
      let t9 = append_varbytes t8 args in
      t9
    end

(* Deserialize a byte stream into a Header, starting at nextpos. *)
let deserialize_Header : ByStr -> Uint32 -> Option (Pair Header Uint32) =
  fun (header : ByStr) =>
  fun (nextpos : Uint32) =>
    let version_nextpos = extract_uint32_le header nextpos in
    match version_nextpos with
    | Some (Pair version nextpos) =>
      let chainid_nextpos = extract_uint64_le header nextpos in
      match chainid_nextpos with
      | Some (Pair chainid nextpos) =>
        let prevBlockHash_nextpos = extract_bystr32 header nextpos in
        match prevBlockHash_nextpos with
        | Some (Pair prevBlockHash nextpos) =>
          let txnroot_nextpos = extract_bystr32 header nextpos in
          match txnroot_nextpos with
          | Some (Pair txnroot nextpos) =>
            let crossStatesRoot_nextpos = extract_bystr32 header nextpos in
            match crossStatesRoot_nextpos with
            | Some (Pair crossStatesRoot nextpos) =>
              let blockRoot_nextpos = extract_bystr32 header nextpos in
              match blockRoot_nextpos with
              | Some (Pair blockRoot nextpos) =>
                let timestamp_nextpos = extract_uint32_le header nextpos in
                match timestamp_nextpos with
                | Some (Pair timestamp nextpos) =>
                  let height_nextpos = extract_uint32_le header nextpos in
                  match height_nextpos with
                  | Some (Pair height nextpos) =>
                    let consensusData_nextpos = extract_uint64_le header nextpos in
                    match consensusData_nextpos with
                    | Some (Pair consensusData nextpos) =>
                      let consensusPayload_nextpos = extract_bystr header nextpos in
                      match consensusPayload_nextpos with
                      | Some (Pair consensusPayload nextpos) =>
                        let nextBookkeeper_nextpos = extract_bystr20 header nextpos in
                        match nextBookkeeper_nextpos with
                        | Some (Pair nextBookkeeper nextpos) =>
                          let header =
                            Header version chainid prevBlockHash txnroot crossStatesRoot
                            blockRoot timestamp height consensusData consensusPayload
                            nextBookkeeper
                          in
                          let res = Pair {Header Uint32} header nextpos in
                          Some {(Pair Header Uint32)} res
                        | None => None {(Pair Header Uint32)}
                        end
                      | None => None {(Pair Header Uint32)}
                      end
                    | None => None {(Pair Header Uint32)}
                    end
                  | None => None {(Pair Header Uint32)}
                  end
                | None => None {(Pair Header Uint32)}
                end
              | None => None {(Pair Header Uint32)}
              end
            | None => None {(Pair Header Uint32)}
            end
          | None => None {(Pair Header Uint32)}
          end
        | None => None {(Pair Header Uint32)}
        end
      | None => None {(Pair Header Uint32)}
      end
    | None => None {(Pair Header Uint32)}
    end

(* Deserialize a byte stream into a TxParam, starting at nextpos. *)
let deserialize_TxParam : ByStr -> Uint32 -> Option (Pair TxParam Uint32) =
  fun (txparam : ByStr) =>
  fun (nextpos : Uint32) =>
    let txhash_nextpos = extract_bystr txparam nextpos in
    match txhash_nextpos with
    | Some (Pair txhash nextpos) =>
      let crossChainId_nextpos = extract_bystr txparam nextpos in
      match crossChainId_nextpos with
      | Some (Pair crossChainId nextpos) =>
        let fromContract_nextpos = extract_bystr txparam nextpos in
        match fromContract_nextpos with
        | Some (Pair fromContract nextpos) =>
          let toChainId_nextpos = extract_uint64_le txparam nextpos in
          match toChainId_nextpos with
          | Some (Pair toChainId nextpos) =>
            let toContract_nextpos = extract_bystr txparam nextpos in
            match toContract_nextpos with
            | Some (Pair toContract nextpos) =>
              let method_nextpos = extract_bystr txparam nextpos in
              match method_nextpos with
              | Some (Pair method nextpos) =>
                let args_nextpos = extract_bystr txparam nextpos in
                match args_nextpos with
                | Some (Pair args nextpos) =>
                  let txparam =
                    TxParam txhash crossChainId fromContract
                    toChainId toContract method args
                  in
                  let res = Pair {TxParam Uint32} txparam nextpos in
                  Some {(Pair TxParam Uint32)} res
                | None => None {(Pair TxParam Uint32)}
                end
              | None => None {(Pair TxParam Uint32)}
              end
            | None => None {(Pair TxParam Uint32)}
            end
          | None => None {(Pair TxParam Uint32)}
          end
        | None => None {(Pair TxParam Uint32)}
        end
      | None => None {(Pair TxParam Uint32)}
      end
    | None => None {(Pair TxParam Uint32)}
    end

(* Deserialize a byte stream into a ToMerkleValue, starting at nextpos. *)
let deserialize_ToMerkleValue : ByStr -> Uint32 -> Option (Pair ToMerkleValue Uint32) =
  fun (tomerklevalue : ByStr) =>
  fun (nextpos : Uint32) =>
    let txhash_nextpos = extract_bystr tomerklevalue nextpos in
    match txhash_nextpos with
    | Some (Pair txhash nextpos) =>
      let fromChainId_nextpos = extract_uint64_le tomerklevalue nextpos in
      match fromChainId_nextpos with
      | Some (Pair fromChainId nextpos) =>
        let txparam = deserialize_TxParam tomerklevalue nextpos in
        match txparam with
        | Some (Pair txparam nextpos) =>
          let tmkv = ToMerkleValue txhash fromChainId txparam in
          let res = Pair {ToMerkleValue Uint32} tmkv nextpos in
          Some {(Pair ToMerkleValue Uint32)} res
        | None => None {(Pair ToMerkleValue Uint32)}
        end
      | None => None {(Pair ToMerkleValue Uint32)}
      end
    | None => None {(Pair ToMerkleValue Uint32)}
    end

(* See Utils.col:compressMCPubKey *)
let compress_pubkey : Pubkey -> ByStr35 =
  fun (pk : Pubkey) =>
    let exceptioner = @raise_exception ByStr35 in
    let default_ret = 0x0000000000000000000000000000000000000000000000000000000000000000000000 in
    match pk with
    | Pubkey pk =>
      let pk = builtin to_bystr pk in
      let pos = Uint32 66 in
      let one = Uint32 1 in
      let last_byte = builtin substr pk pos one in
      let last_bytex_opt = builtin to_bystr1 last_byte in
      match last_bytex_opt with
      | Some last_bytex =>
        let last_byte_uint = builtin to_uint32 last_bytex in
        let two = Uint32 2 in
        let is_even = let rem = builtin rem last_byte_uint two in builtin eq rem zero_uint32 in
        let flagx =
          match is_even with
          | True => 0x02
          | False => 0x03
          end
        in
        let flag = builtin to_bystr flagx in
        let pos = Uint32 0 in
        let len = Uint32 2 in   (* bytes 0 and 1. *)
        let first = builtin substr pk pos len in
        let pos = Uint32 3 in   (* bytes starting from 3 *)
        let len = Uint32 32 in  (* 35 - 3 bytes *)
        let third = builtin substr pk pos len in
        let partial_res = builtin concat first flag in
        let res = builtin concat partial_res third in
        let resx_opt = builtin to_bystr35 res in
        match resx_opt with
        | Some resx => resx
        | None => exceptioner default_ret
        end
      | None =>
        exceptioner default_ret
      end
    end

(*  @notice              calculate next book keeper according to public key list
 *  @param _m            minimum signature number
 *  @param _pubKeyList   consensus node public key list
 *  @return              two element: next book keeper, consensus node signer addresses
 *  See EthCrossChainUtils.sol:_getBookKeeper()
 *)
let getBookKeeper : Uint32 -> List Pubkey -> Pair ByStr20 (List ByStr20) =
  fun (m : Uint32) =>
  fun (pubKeys : List Pubkey) =>
    let lengther = @list_length Pubkey in
    let mapper = @list_map Pubkey ByStr20 in
    let folder = @list_foldl Pubkey ByStr in

    let keyLen = lengther pubKeys in

    let mapf =
      fun (pk : Pubkey) =>
        let sommer = @some_exn ByStr20 in
        let exnobj = 0x0000000000000000000000000000000000000000 in
        match pk with
        | Pubkey pubKeyX =>
          let pubKey = builtin to_bystr pubKeyX in
          let pos = Uint32 3 in
          let len = Uint32 64 in
          let slice = builtin substr pubKey pos len in
          let hashX = builtin keccak256hash slice in
          (* Pick the lower 20 bytes *)
          let hash = builtin to_bystr hashX in
          let pos = Uint32 12 in
          let len = Uint32 20 in
          let keeperX = builtin substr hash pos len in
          let keeper_opt = builtin to_bystr20 keeperX in
          sommer keeper_opt exnobj
        end
    in
    let keepers = mapper mapf pubKeys in

    let foldf =
      fun (acc : ByStr) =>
      fun (pk : Pubkey) =>
        let compressed_pkx = compress_pubkey pk in
        let compressed_pk = builtin to_bystr compressed_pkx in
        append_varbytes acc compressed_pk
    in
    let empty_bystrx = 0x in
    let empty_bystr = builtin to_bystr empty_bystrx in
    let initv = append_uint16 empty_bystr keyLen in
    let buff = folder foldf initv pubKeys in
    let buff = append_uint16 buff m in
    let buff_sha256 = builtin sha256hash buff in
    let nextBookKeeper = builtin ripemd160hash buff_sha256 in

    Pair {ByStr20 (List ByStr20)} nextBookKeeper keepers

let lengther_pubkey = @list_length Pubkey
let lengther_address = @list_length ByStr20

let compute_m : Uint32 -> Uint32 =
  fun (n : Uint32) =>
    let one = Uint32 1 in
    let t1 = builtin sub n one in
    let three = Uint32 3 in
    let t2 = builtin div t1 three in
    let m = builtin sub n t2 in
    m

(* See EthCrossChainUtils.sol:verifyPubKey *)
let verifyPubkey : List Pubkey -> Pair ByStr20 (List ByStr20) =
  fun (pubKeys : List Pubkey) =>
    let n = lengther_pubkey pubKeys in
    let m = compute_m n in
    getBookKeeper m pubKeys

(* Mimics ethereum's recovery of address from message and a signature. *)
(* https://ethereum.stackexchange.com/questions/13778/get-public-key-of-any-ethereum-account *)
let ecrecover : ByStr -> ByStr64 -> Uint32 -> ByStr20 =
  fun (msg : ByStr) =>
  fun (sig : ByStr64) =>
  fun (recid : Uint32) =>
    let pk = builtin ecdsa_recover_pk msg sig recid in
    let pos = Uint32 1 in
    let len = Uint32 64 in
    let pk_bs = builtin to_bystr pk in
    let pk_ = builtin substr pk_bs pos len in
    let pkHash = builtin keccak256hash pk_ in
    let pkHash_bs = builtin to_bystr pkHash in
    let pos = Uint32 12 in
    let len = Uint32 20 in
    let addr_bs = builtin substr pkHash_bs pos len in
    let addr_opt = builtin to_bystr20 addr_bs in
    let sommer = @some_exn ByStr20 in
    let exnobj = 0x0000000000000000000000000000000000000000 in
    sommer addr_opt exnobj

(* See EthCrossChainUtils.sol:verifySig *)
let verifySig : ByStr -> List Signature -> List ByStr20 -> Uint32 -> Bool =
  fun (rawHeader : ByStr) =>
  fun (siglist : List Signature) =>
  fun (keepers : List ByStr20) =>
  fun ( m : Uint32) =>
    let header_hash =
      fun (header : ByStr) =>
        let h1 = builtin sha256hash header in
        let h2 = builtin sha256hash h1 in
        builtin to_bystr h2
    in
    let hashedHeader = header_hash rawHeader in
    let mapf =
      fun (s : Signature) =>
        match s with
        | Signature rs_v =>
          let rs_v = builtin to_bystr rs_v in
          let pos = Uint32 64 in
          let len = Uint32 1 in
          let v_bs = builtin substr rs_v pos len in
          let v_bsx_opt = builtin to_bystr1 v_bs in
          let exnobj_bystr1 = 0x00 in
          let sommer_bystr1 = @some_exn ByStr1 in
          let v_bsx = sommer_bystr1 v_bsx_opt exnobj_bystr1 in
          let v = builtin to_uint32 v_bsx in
          let pos = Uint32 0 in
          let len = Uint32 64 in
          let rs_bs = builtin substr rs_v pos len in
          let rs_bs64_opt = builtin to_bystr64 rs_bs in
          let exnobj_bystr64 = 0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 in
          let sommer_bystr64 = @some_exn ByStr64 in
          let rs = sommer_bystr64 rs_bs64_opt exnobj_bystr64 in
          ecrecover hashedHeader rs v
        end
    in
    let mapper = @list_map Signature ByStr20 in
    let signers = mapper mapf siglist in
    (* Now check that at least "m" keepers have signed. *)
    let eqquer = fun (a : ByStr20) => fun (b : ByStr20) => builtin eq a b in
    let memmer = @list_mem ByStr20 in
    let folder = @list_foldl ByStr20 Uint32 in
    let foldf =
      fun (n : Uint32) =>
      fun (k : ByStr20) =>
        let is_k_in_signers = memmer eqquer k signers in
        match is_k_in_signers with
        | True =>
          let one = Uint32 1 in
          builtin add n one
        | False =>
          n
        end
     in
     let n = folder foldf zero_uint32 keepers in
     let is_not_sufficient = builtin lt n m in
     negb is_not_sufficient

(* See EthCrossChainUtils.sol:getHeaderHash *)
let get_header_hash : ByStr -> ByStr32 =
  fun (header : ByStr) =>
    let h = builtin sha256hash header in
    builtin sha256hash h

(* Given a proof, trace the path and compare with root hash. Return the merkle value on success. *)
let merkle_prove : Proof -> ByStr32 -> Option ByStr =
  fun (proof : Proof) =>
  fun (root : ByStr32) =>
    let hash_leaf =
      fun (data : ByStr) =>
        let k = 0x00 in
        let k_bs = builtin to_bystr k in
        let r = builtin concat k_bs data in
        builtin sha256hash r
    in
    let hash_children =
      fun (l : ByStr32) =>
      fun (r : ByStr32) =>
        let k = 0x01 in
        let t1 = builtin concat k l in
        let r = builtin concat t1 r in
        builtin sha256hash r
    in
    match proof with
    | Proof value path =>
      let succ = True in
      let fail = False in
      let hash = hash_leaf value in
      let foldf =
        fun (acc : Pair ByStr32 Bool) =>
        fun (el : Pair ByStr1 ByStr32) =>
          match acc with
          | Pair hash False => Pair {ByStr32 Bool} hash fail
          | Pair hash _ =>
            let zerob = 0x00 in
            let oneb = 0x01 in
            match el with
            | Pair pos nodeHash =>
              let eqzero = builtin eq zerob pos in
              match eqzero with
              | True =>
                let r = hash_children nodeHash hash in
                Pair {ByStr32 Bool} r succ
              | False =>
                let eqone = builtin eq oneb pos in
                match eqone with
                | True =>
                  let r = hash_children hash nodeHash in
                  Pair {ByStr32 Bool} r succ
                | False => Pair {ByStr32 Bool} hash fail
                end
              end
            end
          end
      in
      let folder = @list_foldl (Pair ByStr1 ByStr32) (Pair ByStr32 Bool) in
      let init = Pair {ByStr32 Bool} hash succ in
      let res = folder foldf init path in
      match res with
      | Pair _ False => None {ByStr}
      | Pair hash _ =>
        let eq = builtin eq hash root in
        match eq with
        | True => Some {ByStr} value
        | False => None {ByStr}
        end
      end
    end
`;

export const ShogiLib = `
scilla_version 0

library ShogiLib

(* Pieces in the game *)
type Piece =
(* 玉 *)
| King
(* 金 *)
| GoldGeneral
(* 銀 *)
| SilverGeneral
(* 桂 *)
| Knight
(* 香 *)
| Lance
(* 歩 *)
| Pawn
(* 飛 *)
| Rook
(* 角 *)
| Bishop

let lance  = Lance
let knight = Knight
let silver = SilverGeneral
let gold   = GoldGeneral
let king   = King
let rook   = Rook
let bishop = Bishop
let pawn   = Pawn

let piece_to_int =
  fun (piece : Piece) =>
    match piece with
    | King          => Uint32 1
    | GoldGeneral   => Uint32 2
    | SilverGeneral => Uint32 3
    | Knight        => Uint32 4
    | Lance         => Uint32 5
    | Pawn          => Uint32 6
    | Rook          => Uint32 7
    | Bishop        => Uint32 8
    end
    
type PromotionStatus =
| NotPromoted
| Promoted

let not_promoted = NotPromoted
let promoted     = Promoted

type SquareContents =
(* Square is occupied by a possibly promoted piece owned by a player *)
| Occupied of Piece PromotionStatus ByStr20
(* Square is free *)
| Free

let free = Free

(* Direction of movement *)
type Direction =
| East
| SouthEast
| South
| SouthWest
| West
| NorthWest
| North
| NorthEast

(* Shorthand for a set of coordinates on the board *)
type Square =
| Square of Uint32 Uint32

(* Possible actions a player can take *)
type Action =
(* Move a piece *)
(* Square : The current position of the piece *)
(* Direction : The direction of movement *)
(* Uint32 : The number of squares to move *)
(* Bool : Promote the piece after movement *)
| Move of Square Direction Uint32 Bool
(* Place a captured piece *)
(* Piece : The captured piece to be placed *)
(* Square : The position on the board to place it *)
| Place of Piece Square
(* Resign and award win to the opponent *)
| Resign

(* Initial state of the board *)
let initial_board =
  fun (player1 : ByStr20) =>
  fun (player2 : ByStr20) => 
    (* Row and column numbers *)
    let row1 = Uint32 1 in                                 
    let row2 = Uint32 2 in                                 
    let row3 = Uint32 3 in                                 
    let row4 = Uint32 4 in                                 
    let row5 = Uint32 5 in                                 
    let row6 = Uint32 6 in                                 
    let row7 = Uint32 7 in                                 
    let row8 = Uint32 8 in                                 
    let row9 = Uint32 9 in                                 
    let column1 = Uint32 1 in                              
    let column2 = Uint32 2 in                              
    let column3 = Uint32 3 in                              
    let column4 = Uint32 4 in                              
    let column5 = Uint32 5 in                              
    let column6 = Uint32 6 in                              
    let column7 = Uint32 7 in                              
    let column8 = Uint32 8 in                              
    let column9 = Uint32 9 in
    (* Square contents *)
    let lance1   = Occupied lance  not_promoted player1 in
    let knight1  = Occupied knight not_promoted player1 in
    let silver1  = Occupied silver not_promoted player1 in
    let gold1    = Occupied gold   not_promoted player1 in
    let king1    = Occupied king   not_promoted player1 in
    let bishop1  = Occupied bishop not_promoted player1 in
    let rook1    = Occupied rook   not_promoted player1 in
    let pawn1    = Occupied pawn   not_promoted player1 in
    let lance2   = Occupied lance  not_promoted player2 in
    let knight2  = Occupied knight not_promoted player2 in
    let silver2  = Occupied silver not_promoted player2 in
    let gold2    = Occupied gold   not_promoted player2 in
    let king2    = Occupied king   not_promoted player2 in
    let bishop2  = Occupied bishop not_promoted player2 in
    let rook2    = Occupied rook   not_promoted player2 in
    let pawn2    = Occupied pawn   not_promoted player2 in
    (* Insert pieces into board map *)
    let row1_map = Emp Uint32 SquareContents in
    let row1_map = builtin put row1_map column1 lance1  in
    let row1_map = builtin put row1_map column2 knight1 in
    let row1_map = builtin put row1_map column3 silver1 in
    let row1_map = builtin put row1_map column4 gold1   in
    let row1_map = builtin put row1_map column5 king1   in
    let row1_map = builtin put row1_map column6 gold1   in
    let row1_map = builtin put row1_map column7 silver1 in
    let row1_map = builtin put row1_map column8 knight1 in
    let row1_map = builtin put row1_map column9 lance1  in
    let row2_map = Emp Uint32 SquareContents in
    let row2_map = builtin put row2_map column1 free    in
    let row2_map = builtin put row2_map column2 bishop1 in
    let row2_map = builtin put row2_map column3 free    in
    let row2_map = builtin put row2_map column4 free    in
    let row2_map = builtin put row2_map column5 free    in
    let row2_map = builtin put row2_map column6 free    in
    let row2_map = builtin put row2_map column7 free    in
    let row2_map = builtin put row2_map column8 rook1   in
    let row2_map = builtin put row2_map column9 free    in
    let row3_map = Emp Uint32 SquareContents in
    let row3_map = builtin put row3_map column1 pawn1   in
    let row3_map = builtin put row3_map column2 pawn1   in
    let row3_map = builtin put row3_map column3 pawn1   in
    let row3_map = builtin put row3_map column4 pawn1   in
    let row3_map = builtin put row3_map column5 pawn1   in
    let row3_map = builtin put row3_map column6 pawn1   in
    let row3_map = builtin put row3_map column7 pawn1   in
    let row3_map = builtin put row3_map column8 pawn1   in
    let row3_map = builtin put row3_map column9 pawn1   in
    let row4_map = Emp Uint32 SquareContents in
    let row4_map = builtin put row4_map column1 free    in
    let row4_map = builtin put row4_map column2 free    in
    let row4_map = builtin put row4_map column3 free    in
    let row4_map = builtin put row4_map column4 free    in
    let row4_map = builtin put row4_map column5 free    in
    let row4_map = builtin put row4_map column6 free    in
    let row4_map = builtin put row4_map column7 free    in
    let row4_map = builtin put row4_map column8 free    in
    let row4_map = builtin put row4_map column9 free    in
    let row5_map = Emp Uint32 SquareContents in
    let row5_map = builtin put row5_map column1 free    in
    let row5_map = builtin put row5_map column2 free    in
    let row5_map = builtin put row5_map column3 free    in
    let row5_map = builtin put row5_map column4 free    in
    let row5_map = builtin put row5_map column5 free    in
    let row5_map = builtin put row5_map column6 free    in
    let row5_map = builtin put row5_map column7 free    in
    let row5_map = builtin put row5_map column8 free    in
    let row5_map = builtin put row5_map column9 free    in
    let row6_map = Emp Uint32 SquareContents in
    let row6_map = builtin put row6_map column1 free    in
    let row6_map = builtin put row6_map column2 free    in
    let row6_map = builtin put row6_map column3 free    in
    let row6_map = builtin put row6_map column4 free    in
    let row6_map = builtin put row6_map column5 free    in
    let row6_map = builtin put row6_map column6 free    in
    let row6_map = builtin put row6_map column7 free    in
    let row6_map = builtin put row6_map column8 free    in
    let row6_map = builtin put row6_map column9 free    in
    let row7_map = Emp Uint32 SquareContents in
    let row7_map = builtin put row7_map column1 pawn2   in
    let row7_map = builtin put row7_map column2 pawn2   in
    let row7_map = builtin put row7_map column3 pawn2   in
    let row7_map = builtin put row7_map column4 pawn2   in
    let row7_map = builtin put row7_map column5 pawn2   in
    let row7_map = builtin put row7_map column6 pawn2   in
    let row7_map = builtin put row7_map column7 pawn2   in
    let row7_map = builtin put row7_map column8 pawn2   in
    let row7_map = builtin put row7_map column9 pawn2   in
    let row8_map = Emp Uint32 SquareContents in
    let row8_map = builtin put row8_map column1 free    in
    let row8_map = builtin put row8_map column2 rook2   in
    let row8_map = builtin put row8_map column3 free    in
    let row8_map = builtin put row8_map column4 free    in
    let row8_map = builtin put row8_map column5 free    in
    let row8_map = builtin put row8_map column6 free    in
    let row8_map = builtin put row8_map column7 free    in
    let row8_map = builtin put row8_map column8 bishop2 in
    let row8_map = builtin put row8_map column9 free    in
    let row9_map = Emp Uint32 SquareContents in
    let row9_map = builtin put row9_map column1 lance2  in
    let row9_map = builtin put row9_map column2 knight2 in
    let row9_map = builtin put row9_map column3 silver2 in
    let row9_map = builtin put row9_map column4 gold2   in
    let row9_map = builtin put row9_map column5 king2   in
    let row9_map = builtin put row9_map column6 gold2   in
    let row9_map = builtin put row9_map column7 silver2 in
    let row9_map = builtin put row9_map column8 knight2 in
    let row9_map = builtin put row9_map column9 lance2  in
    let board = Emp Uint32 (Map Uint32 SquareContents) in
    let board = builtin put board row1 row1_map in
    let board = builtin put board row2 row2_map in
    let board = builtin put board row3 row3_map in
    let board = builtin put board row4 row4_map in
    let board = builtin put board row5 row5_map in
    let board = builtin put board row6 row6_map in
    let board = builtin put board row7 row7_map in
    let board = builtin put board row8 row8_map in
    let board = builtin put board row9 row9_map in
    board

let init_captured_pieces =
  fun (player1 : ByStr20) =>
  fun (player2 : ByStr20) =>
    let zero = Uint32 0 in
    (* Pieces map - all empty *)
    let pieces = Emp Uint32 Uint32 in
    let v = piece_to_int lance in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int knight in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int pawn in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int silver in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int gold in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int rook in
    let pieces = builtin put pieces v zero in
    let v = piece_to_int bishop in
    let pieces = builtin put pieces v zero in
    (* Kings cannot be captured without ending the game *)
    (* Captured pieces map *)
    let res = Emp ByStr20 (Map Uint32 Uint32) in
    let res = builtin put res player1 pieces in
    let res = builtin put res player2 pieces in
    res

`;
