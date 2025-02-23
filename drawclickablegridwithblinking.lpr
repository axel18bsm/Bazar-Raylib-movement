program DrawClickableGridWithRealTimeBlinking;
uses raylib, sysutils; // sysutils for IntToStr conversion

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Padding for placing the text inside the cell
  SmallFontSize = 15; // Reduced font size for text in the cells
  BlinkInterval = 0.5; // Time interval in seconds for blinking (0.5 seconds)

var
  screenWidth, screenHeight: Integer;
  i, j, number, colorIndex: Integer;
  Colors: array[1..10] of TColor;
  CellColors: array[0..Rows-1, 0..Columns-1] of Integer; // Array to store color indexes for each cell
  SelectedRow, SelectedCol: Integer; // Tracks the currently selected cell
  IsSelected: Boolean; // Whether a cell is selected
  LastBlinkTime: Double; // Stores the last time the blink status changed
  BlinkState: Boolean; // Current state of the blinking (on/off)

// Procedure to randomly assign colors to cells (done once)
procedure AssignRandomColors();
begin
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
      CellColors[j, i] := GetRandomValue(1, 10); // Store the color index for each cell
end;

// Procedure to draw the grid with colors and display numbers
procedure DrawColoredGridWithNumbers();
var
  xPos, yPos: Integer;
  DrawColor: TColor; // Color to use for the cell
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

      // Determine the color to draw based on blink state if this is the selected cell
      if (j = SelectedRow) and (i = SelectedCol) and IsSelected then
      begin
        if BlinkState then
          DrawColor := Colors[colorIndex] // Regular color
        else
          DrawColor := Fade(Colors[colorIndex], 0.3); // Dimmed color for blinking

        DrawRectangle(xPos, yPos, CellSize, CellSize, DrawColor);

        // Draw the orange border for the selected cell
        DrawRectangleLinesEx(RectangleCreate(xPos, yPos, CellSize, CellSize), 3, ORANGE);
      end
      else
      begin
        // Draw the regular cell
        DrawRectangle(xPos, yPos, CellSize, CellSize, Colors[colorIndex]);
      end;

      // Draw the cell number at top-left corner
      DrawText(PChar(IntToStr(number)), xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Draw the color index below the number
      DrawText(PChar(IntToStr(colorIndex)), xPos + TextPadding, yPos + 25, SmallFontSize, BLACK);

      Inc(number); // Move to the next number
    end;
  end;
end;

// Procedure to check if a cell was clicked and select it
procedure HandleMouseClick();
var
  mouseX, mouseY: Integer;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();

  // Check if the mouse is clicked
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    // Calculate the row and column of the clicked cell
    SelectedCol := mouseX div CellSize;
    SelectedRow := mouseY div CellSize;

    // Set the cell as selected
    IsSelected := True;

    // Reset the blinking state and timer
    BlinkState := True;
    LastBlinkTime := GetTime();
  end;
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

  InitWindow(screenWidth, screenHeight, 'Clickable Grid with Real-Time Blinking');

  SetTargetFPS(60); // Set FPS to 60

  AssignRandomColors(); // Assign random colors to each cell once

  SelectedRow := -1; // Initially, no cell is selected
  SelectedCol := -1;
  IsSelected := False;
  LastBlinkTime := GetTime();
  BlinkState := True;

  while not WindowShouldClose() do
  begin
    // Update the blinking state based on time interval
    if (GetTime() - LastBlinkTime) >= BlinkInterval then
    begin
      BlinkState := not BlinkState; // Toggle blink state
      LastBlinkTime := GetTime(); // Reset the blink timer
    end;

    // Handle mouse clicks and select cells
    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE); // Background color

    DrawColoredGridWithNumbers(); // Call function to draw grid with stored colors and numbers

    EndDrawing();
  end;

  CloseWindow();
end.










