(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* problem 1 *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

(* problem 2 *)
val longest_string1 = foldl (fn (s, acc) => if (String.size s > String.size acc) then s else acc) "" 

(* problem 3 *)
val longest_string2 = foldl (fn (s, acc) => if (String.size s >= String.size acc) then s else acc) "" 

(* problem 4 *)
fun longest_string_helper f l = foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) "" l

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(*problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* problem 6 *)
val rev_string = String.implode o rev o String.explode

(* problem 7 *)
fun first_answer f l = 
    case l of
	[] => raise NoAnswer
      | [h] => (case (f h) of
		    NONE => raise NoAnswer
		  | SOME a =>  a)
      | h::t => (case (f h) of
		     NONE => first_answer f t
		   | SOME a =>  a)
		    
(* problem 8 *)
fun all_answers f l = foldl (fn (e, a) => case (f e) of
					      NONE => NONE
					    | SOME b => case a of
							    NONE => NONE
							  | SOME a => SOME (b@a)
			    ) (SOME []) l

(* problem 9a *)
fun count_wildcards p = g (fn (x) => 1) (fn(x) => 0) p

(* problem 9b*)
fun count_wild_and_variable_lengths p = g (fn (x) => 1) String.size p

(* problem 9c *)
fun count_some_var (s, p) = g (fn(x) => 0) (fn (x) => if x=s then 1 else 0) p

(* problem 10 *)

fun check_pat p =
    let fun getstrs p =
	    case p of
		Variable v => [v]
	      | TupleP lp => foldl (fn (p, a) => getstrs(p)@a) [] lp
	      | ConstructorP (_,p) => getstrs(p)
	      |  _ => []
    in 
	let fun reps sl = 
		case sl of
		    [] => false
		 (* | h => false*)
		  | h::t => List.exists (fn(s)=>s=h)  t orelse reps(t)

	in 
	    not (reps (getstrs p))
	end
    end


(* problem 11 *)

fun match (v, pat) =
    case pat of
	Wildcard => (SOME [])
      | Variable s => (SOME [(s, v)])
      | UnitP => (case v of
		      Unit => SOME []
		   |  _ => NONE)
      | ConstP a => (case v of
			 Const b => if a=b then SOME [] else NONE
		       | _ => NONE)
      | TupleP ps => (case v of
			  Tuple vs => if length ps = length vs then all_answers match (ListPair.zip(vs, ps)) else NONE
			| _ => NONE)
      | ConstructorP (s1, p) => (case v of
				      Constructor (s2, v) => (if s1=s2 then (match (v, p)) else NONE)
				    | _ => NONE)
	 
				   
(* problem 12 *)

fun first_match v lp =
      first_answer (fn (p) => SOME (match (v, p))) lp 
      handle NoAnswer => NONE
