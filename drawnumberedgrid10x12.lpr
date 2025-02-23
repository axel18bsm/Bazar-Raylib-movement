program DrawFixedColoredNumberedGrid;
uses raylib, sysutils; // sysutils for IntToStr conversion

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Padding for placing the text inside the cell
  SmallFontSize = 15; // Reduced font size for text in the cells

var
  screenWidth, screenHeight: Integer;
  i, j, number, colorIndex: Integer;
  Colors: array[1..10] of TColor;
  CellColors: array[0..Rows-1, 0..Columns-1] of Integer; // Array to store color indexes for each cell

// Procedure to randomly assign colors to cells (done once)
procedure AssignRandomColors();
begin
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
      CellColors[j, i] := GetRandomValue(1, 10); // Store the color index for each cell
end;

// Procedure to draw the grid with the fixed colored cells and display numbers
procedure DrawColoredGridWithNumbers();
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

      // Fill the cell with the stored color
      colorIndex := CellColors[j, i];
      DrawRectangle(xPos, yPos, CellSize, CellSize, Colors[colorIndex]);

      // Draw the cell number at top-left corner
      DrawText(PChar(IntToStr(number)), xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Draw the color index below the number
      DrawText(PChar(IntToStr(colorIndex)), xPos + TextPadding, yPos + 25, SmallFontSize, BLACK);

      // Draw the grid lines
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
  // Define 10 different colors excluding red
  Colors[1] := BLUE;
  Colors[2] := GREEN;
  Colors[3] := YELLOW;
  Colors[4] := ORANGE;
  Colors[5] := PURPLE;
  Colors[6] := SKYBLUE;
  Colors[7] := PINK;
  Colors[8] := BROWN;
  Colors[9] := DARKGREEN;
  Colors[10] := LIME;

  // Calculate screen size based on grid size
  screenWidth := Columns * CellSize;
  screenHeight := Rows * CellSize;

  InitWindow(screenWidth, screenHeight, '10x12 Colored Numbered Grid with Fixed Colors');

  SetTargetFPS(60); // Set FPS to 60

  AssignRandomColors(); // Assign random colors to each cell once

  while not WindowShouldClose() do
  begin
    BeginDrawing();

    ClearBackground(RAYWHITE); // Background color

    DrawColoredGridWithNumbers(); // Call function to draw grid with stored colors and numbers

    EndDrawing();
  end;

  CloseWindow();
end.








