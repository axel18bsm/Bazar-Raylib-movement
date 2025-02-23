program DrawNumberedGrid10x12;
uses raylib, sysutils; // sysutils for IntToStr conversion

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Padding to place the text inside the cell

var
  screenWidth, screenHeight: Integer;
  i, j, number: Integer;

// Procedure to draw grid lines and numbers
procedure DrawGridWithNumbers();
var
  xPos, yPos: Integer;
begin
  number := 1;  // Start numbering from 1

  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;

      // Draw cell number at top-left corner of the cell
      DrawText(PChar(IntToStr(number)), xPos + TextPadding, yPos + TextPadding, 20, BLACK);

      // Draw the vertical and horizontal lines
      DrawLine(xPos, 0, xPos, screenHeight, RED); // Vertical line
      DrawLine(0, yPos, screenWidth, yPos, RED);  // Horizontal line

      Inc(number); // Move to the next number
    end;
  end;

  // Draw the final lines to close the grid
  DrawLine(screenWidth - 1, 0, screenWidth - 1, screenHeight, RED); // Rightmost vertical line
  DrawLine(0, screenHeight - 1, screenWidth, screenHeight - 1, RED); // Bottommost horizontal line
end;

begin
  // Calculate screen size based on grid size
  screenWidth := Columns * CellSize;
  screenHeight := Rows * CellSize;

  InitWindow(screenWidth, screenHeight, '10x12 Numbered Grid with Red Lines');

  SetTargetFPS(60); // Set FPS to 60

  while not WindowShouldClose() do
  begin
    BeginDrawing();

    ClearBackground(RAYWHITE); // Background color

    DrawGridWithNumbers(); // Call function to draw grid and numbers

    EndDrawing();
  end;

  CloseWindow();
end.






