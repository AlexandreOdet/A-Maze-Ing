module DOOR =
  (
    struct
      type door = (int * int)

      let make x y = (x, y)

      let getX (x, y) = x

      let getY (x, y) = y

      let equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2
    end
    :
      sig
	type door
	val make : int -> int -> door
	val getX : door -> int
	val getY : door -> int
	val equal : door -> door -> bool
      end
  )

module CASE =
  (
    struct
      type case =
	  {
	    sides : int;
	    mutable neighbors : DOOR.door list;
	    mutable color : int;
	  }

      let make s c =
	{
	  sides = s;
	  neighbors = [];
	  color = c;
	}

      let getSides c = c.sides

      let getColor c = c.color

      let setColor c i = c.color <- i

      let sdlcolor_of_case c = (c.color / 65536, (c.color mod 65536) / 256, c.color mod 256)

      let getNeighbors c = c.neighbors

      let append cell dor =
	  if ((List.length cell.neighbors) + 1) > cell.sides
	  then failwith "Too many cases"
	  else if (List.exists (DOOR.equal dor) cell.neighbors) == true
	  then failwith "Already in"
	  else cell.neighbors <- List.append cell.neighbors [dor]

      let link d1 d2 map =
	let c1 = map.(DOOR.getX d1).(DOOR.getY d1) in
	let c2 = map.(DOOR.getX d2).(DOOR.getY d2) in
	if c1.color == c2.color
	then failwith "Same color"
	else
	  begin
	    append c1 d2;
	    append c2 d1;
	  end
    end
    :
      sig
	type case
	val make : int -> int -> case
	val getSides : case -> int
	val getColor : case -> int
	val setColor : case -> int -> unit
	val sdlcolor_of_case : case -> (int * int * int)
	val getNeighbors : case -> DOOR.door list
	val append : case -> DOOR.door -> unit
	val link : DOOR.door -> DOOR.door -> case array array -> unit
      end
  )

module type MAZE =
  sig
    type maze =
	{
	  len : int;
	  wid : int;
	  cases : CASE.case array array;
	}
    val make : int -> int -> maze
    val perfectIt : maze -> unit
    val iter : maze -> (CASE.case -> DOOR.door -> unit) -> unit
    val emptyColor : maze -> unit
    val neighborDirection : DOOR.door -> DOOR.door -> int
    val print : maze -> unit
  end

module Maze : MAZE =
  struct
    type maze =
	{
	  len : int;
	  wid : int;
	  cases : CASE.case array array;
	}

    let make l w =
      {
	len = l;
	wid = w;
	cases = Array.init l (fun y -> Array.init w (fun x -> CASE.make 4 (y * w + x)))
      }

    let perfectIt map =
      let rec changeColor ar dor c =
	let (cell : CASE.case) = ar.(DOOR.getX dor).(DOOR.getY dor) in
	CASE.setColor cell c;
	List.iter (
	    fun dr ->
	    try
	      let cl = ar.(DOOR.getX dr).(DOOR.getY dr) in
	      if (CASE.getColor cl) <> c
	      then changeColor ar dr c;
	    with
	      _ -> ();
	  ) (CASE.getNeighbors cell);
      in let randNeighbor d =
	   let doors =
	     [
	       (DOOR.make ((DOOR.getX d) + 1) (DOOR.getY d));
	       (DOOR.make ((DOOR.getX d) - 1) (DOOR.getY d));
	       (DOOR.make (DOOR.getX d) ((DOOR.getY d) + 1));
	       (DOOR.make (DOOR.getX d) ((DOOR.getY d) - 1));
	     ]
	   in List.nth doors (Random.int (List.length doors))
	 in let rec findRand map =
	      try
		let (d1 : DOOR.door) = DOOR.make (Random.int map.len) (Random.int map.wid) in
		let (d2 : DOOR.door) = randNeighbor d1 in
		let _ = CASE.link d1 d2 map.cases in
		(d1, d2)
	      with
		_ -> findRand map
	    in let rec uniqueColor nbColor map =
		 if nbColor == 1
		 then ()
		 else let (c1, c2) = findRand map
		      in
		      begin
			changeColor map.cases c2 (CASE.getColor map.cases.(DOOR.getX c1).(DOOR.getY c1));
			uniqueColor (nbColor - 1) map
		      end
	       in uniqueColor (map.wid * map.len) map

    let iter map func =
      let rec aux (m : maze) pos =
	if pos / m.len == m.wid
	then ()
	else let ps = DOOR.make (pos / m.wid) (pos mod m.wid) in
	     let (cell : CASE.case) = m.cases.(DOOR.getX ps).(DOOR.getY ps)
	     in
	     begin
	       func cell ps;
	       aux m (pos + 1);
	     end
      in aux map 0

    let emptyColor map =
      iter map (fun c _ -> CASE.setColor c 16777215)

    let neighborDirection d1 d2 =
      let dx = (DOOR.getX d2) - (DOOR.getX d1) in
      let dy = (DOOR.getY d2) - (DOOR.getY d1) in
      if dx == 1 then 0
      else if dx == -1
      then 1
      else if dy == 1
      then 2
      else 3
		     
    let print map =
      begin
	print_string (String.make (map.wid * 2 + 1) '_');
	print_char '\n';
	iter map (fun cell ps ->
		  begin
		    if (DOOR.getY ps) == 0
		    then print_char '|';
		    if (List.exists (
	       		    fun d ->
	       		    if (neighborDirection ps d) = 0
	       		    then true
	       		    else false
	       		  ) (CASE.getNeighbors cell)) == true
		    then print_char ' '
		    else print_char '_';
		    if (List.exists (
	       		    fun d ->
	       		    if (neighborDirection ps d) = 2
	       		    then true
	       		    else false
	       		  ) (CASE.getNeighbors cell)) == true
		    then print_char '_'
		    else print_char '|';
		    if (((DOOR.getY ps) + 1) mod map.wid) == 0
		    then print_char '\n';
		  end);
	flush_all ();
      end
  end
