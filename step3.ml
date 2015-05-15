let main argc argv =
  begin
    try
      Random.self_init ();
      if argc >= 3
      then let len = (int_of_string argv.(2)) in
	   let wid = (int_of_string argv.(1)) in
	   if len > 0 && wid > 0
	   then let maze = Maze.Maze.make len wid in
		begin
		  Maze.Maze.perfectIt maze;
		  Maze.Maze.print maze;
		  Maze.Maze.emptyColor maze;
		  let screen = SDL.initGraph () in
		  SDL.printMaze maze screen;
		  SDL.amazeIt ();
		  SDL.playBGM ();
		  Solver.solve maze (Maze.DOOR.make 0 0) (Maze.DOOR.make (len - 1) (wid - 1)) screen (700 / (max len wid));
		  SDL.wait ();
		end
	   else failwith "Negative or null values"
      else failwith "Not enough argument";
    with
    | Failure m -> print_string m; print_char '\n';
    | Invalid_argument m -> print_string m; print_char '\n';
  end

let start = main (Array.length Sys.argv) Sys.argv
