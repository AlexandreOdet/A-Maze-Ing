exception Solved of int

open Maze.Maze

let solve map beg endd screen size =
  let begX = Maze.DOOR.getX beg in
  let begY = Maze.DOOR.getY beg in
  let endX = Maze.DOOR.getX endd in
  let endY = Maze.DOOR.getY endd in
  Maze.CASE.setColor map.cases.(begX).(begY) 0;
  SDL.printCase screen size map.cases.(begX).(begY) beg;
  Maze.CASE.setColor map.cases.(endX).(endY) 0;
  SDL.printCase screen size map.cases.(endX).(endY) endd;
  try
    let rec aux x y ways steps =
      if x == endX && y == endY
      then raise (Solved steps)
      else
	begin
          Maze.CASE.setColor map.cases.(x).(y) 52480;
	  SDL.printCase screen size map.cases.(x).(y) (Maze.DOOR.make x y);
          if List.length (Maze.CASE.getNeighbors map.cases.(x).(y)) = 1 && (x <> begX or y <> begY)
	  then
            begin
              Maze.CASE.setColor map.cases.(x).(y) 16711680;
	      SDL.printCase screen size map.cases.(x).(y) (Maze.DOOR.make x y);
            end
          else let epurWays doors toEpur =
		 List.fold_left (fun acc dr ->
				 if (Maze.DOOR.equal dr toEpur) == true
				 then acc
				 else dr::acc) [] doors
	       in let rec browse it = function
		    | dr::tl ->
		       let operation i d = 
			 let neighbor = map.cases.(Maze.DOOR.getX d).(Maze.DOOR.getY d) in
			 let newWays = epurWays (Maze.CASE.getNeighbors neighbor) (Maze.DOOR.make x y) in
			 let _ = aux (Maze.DOOR.getX d) (Maze.DOOR.getY d) newWays (steps + 1) in
			 if List.length ways = (i + 1)
			 then
			   begin
			     Maze.CASE.setColor map.cases.(x).(y) 16711680;
			     SDL.printCase screen size map.cases.(x).(y) (Maze.DOOR.make x y);
			   end
		       in operation it dr; browse (it + 1) tl;
		    | _ -> ()
		  in browse 0 ways
	end
    in aux begX begY (Maze.CASE.getNeighbors map.cases.(begX).(begY)) 0
  with
    Solved s -> print_int s; print_endline " step(s) to the end";
