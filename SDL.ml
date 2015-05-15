open Sdlevent
open Sdlkey
open Maze.Maze

let amazing = "tools/amazing.wav"

let bgm = "tools/Into the Maze.wav"

(*Event Handling*)

let rec wait () =
  match wait_event () with
  | QUIT-> ()
  | KEYDOWN {keysym = KEY_ESCAPE}-> ()
  | _ -> wait ()


let amazeIt () =
  begin
    try
      let music = Sdlmixer.load_music amazing in
      Sdlmixer.play_music music;
      Sdltimer.delay 2800;
      Sdlmixer.halt_music ();
      Sdlmixer.free_music music;
    with
      Sdlmixer.SDLmixer_exception m -> print_endline m;
  end

let playBGM () =
  begin
    try
      let music = Sdlmixer.load_music bgm in
      Sdlmixer.play_music music;
      at_exit (fun _ -> (Sdlmixer.free_music music));
    with
      Sdlmixer.SDLmixer_exception m -> print_endline m;
  end

(*Init Graphic with SDL*)

let initGraph () =
  try
    Sdl.init [`VIDEO; `AUDIO];
    at_exit Sdl.quit;
    Sdlmixer.open_audio ();
    at_exit Sdlmixer.close_audio;
    let screen = Sdlvideo.set_video_mode 701 701 [`DOUBLEBUF] in
    Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen (255, 255, 255));
    Sdlvideo.flip screen;
    Sdlwm.set_caption ~title:"A-Maze-Ing" ~icon:"";
    screen;
  with
    Sdl.SDL_init_exception(msg) ->
    begin
      print_endline "Error: Can't init SDL library";
      exit (1);
    end

let rec drawHor x y screen size color =
  if size > 0 then
    begin
      Sdlvideo.put_pixel_color screen x y color;
      drawHor (x + 1) y screen (size - 1) color;
    end

let rec drawVert x y screen size color =
  if size > 0 then
    begin
      Sdlvideo.put_pixel_color screen x y color;
      drawVert x (y + 1) screen (size - 1) color;
    end

let drawSquare x y screen size color =
  begin
    drawHor (x * size) (y * size) screen size color;
    drawVert (x * size) (y * size) screen size color;
    drawHor (x * size) ((y  + 1) * size) screen size color;
    drawVert ((x + 1) * size) (y * size) screen size color;
  end

(*Print The Maze With OCaml SDL*)

let printCase screen size cell pos =
  let x = Maze.DOOR.getX pos in
  let y = Maze.DOOR.getY pos in
  let c = Maze.CASE.sdlcolor_of_case cell in
  let case = Sdlvideo.rect (y * size) (x * size) size size in
  begin
    Sdlvideo.fill_rect ~rect:case screen (Sdlvideo.map_RGB screen c);
    drawSquare y x screen size (0, 0, 0);
    List.iter (fun dor ->
	       let dir = neighborDirection pos dor in
	       match dir with
	       | 0 -> drawHor (y * size) ((x + 1) * size) screen size c
	       | 1 -> drawHor (y * size) (x * size) screen size c
	       | 2 -> drawVert ((y + 1) * size) (x * size) screen size c
	       | 3 -> drawVert (y * size) (x * size) screen size c
	       | _ -> failwith "No such direction"
	      ) (Maze.CASE.getNeighbors cell);
    Sdlvideo.flip screen;
  end

let printMaze maze screen =
  let cellSide = 700 / (max maze.wid maze.len) in
  Maze.Maze.iter maze (printCase screen cellSide);
