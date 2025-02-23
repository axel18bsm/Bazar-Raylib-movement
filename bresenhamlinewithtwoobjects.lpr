
 program bresenhamlinewithtwoobjects;

uses raylib;

type
  TObject = record
    x, y: Integer;
    targetX, targetY: Integer;
    speed: Integer;
    color: TColor;
  end;

procedure BresenhamLine(x0, y0, x1, y1: Integer; color: TColor);
var
  dx, dy, sx, sy, err, e2: Integer;
begin
  dx := Abs(x1 - x0);
  dy := Abs(y1 - y0);

  if x0 < x1 then sx := 1 else sx := -1;
  if y0 < y1 then sy := 1 else sy := -1;

  err := dx - dy;

  while True do
  begin
    DrawPixel(x0, y0, color);  // Draw point

    if (x0 = x1) and (y0 = y1) then Break; // Line reached the endpoint

    e2 := 2 * err;

    if e2 > -dy then
    begin
      err := err - dy;
      x0 := x0 + sx;
    end;

    if e2 < dx then
    begin
      err := err + dx;
      y0 := y0 + sy;
    end;
  end;
end;

procedure MoveObject(var obj: TObject);
var
  dx, dy, dist: Single;
  stepX, stepY: Single;
begin
  dx := obj.targetX - obj.x;
  dy := obj.targetY - obj.y;
  dist := Sqrt(dx * dx + dy * dy);

  // Normalize direction and multiply by speed
  if dist <> 0 then
  begin
    stepX := (dx / dist) * obj.speed;
    stepY := (dy / dist) * obj.speed;

    obj.x := obj.x + Round(stepX);
    obj.y := obj.y + Round(stepY);
  end;
end;

var
  obj1, obj2: TObject;
begin
  // Initialize Raylib window
  InitWindow(800, 600, 'Bresenham Line with Two Moving Objects');

  // Initialize object 1
  obj1.x := 50; obj1.y := 100;
  obj1.targetX := 700; obj1.targetY := 100;
  obj1.speed := 10;
  obj1.color := RED;

  // Initialize object 2
  obj2.x := 50; obj2.y := 50;
  obj2.targetX := 700; obj2.targetY := 50;
  obj2.speed := 3;
  obj2.color := BLUE;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    // Move objects based on their speed
    MoveObject(obj1);
    MoveObject(obj2);

    BeginDrawing();
    ClearBackground(RAYWHITE);

     //Draw Bresenham lines for both objects
    BresenhamLine(50, 100, obj1.x, obj1.y, obj1.color);
    BresenhamLine(50, 50, obj2.x, obj2.y, obj2.color);

    EndDrawing();
  end;

  CloseWindow();
end.
