program nouveaudepart11;
uses raylib, sysutils, classes;

const
  CellSize = 50;
  SmallCellSize = 40;
  Rows = 10;
  Columns = 12;
  TextPadding = 5;
  SmallFontSize = 12;
  InfoBoxWidth = 300;
  ButtonWidth = 150;
  ButtonHeight = 40;
  SaveFileName = 'grid_save.txt';

type
  TTerrainType = (Grass, Swamp, Field, Forest, Village, House);

  TCell = record
    Number: Integer;
    Color: TColor;
    ColorIndex: Integer;
    TerrainType: TTerrainType;
    HasRoad: Boolean;
    HasRiver: Boolean;
    HasBridge: Boolean;
    HasPath: Boolean;
    Level: Integer;
  end;

var
  screenWidth, screenHeight: Integer;
  Cells: array[0..Rows-1, 0..Columns-1] of TCell;
  Colors: array[1..10] of TColor;
  SelectedRow, SelectedCol: Integer;
  SelectedCells: array of TPoint;  // Tableau des cellules sélectionnées
  CellSelected, LargeCellSelected: Boolean;
  SelectedCellInfo: array[0..255] of Char;
  SaveButtonRec, LoadButtonRec, NewGridButtonRec, EmptyGridButtonRec, RouteButtonRec: TRectangle;
  Fields: array of String;
  SaveButtonPressed, LoadButtonPressed, NewGridButtonPressed, EmptyGridButtonPressed, RouteButtonPressed: Boolean;

// Convertit une couleur en chaîne contenant ses valeurs RGB
function ColorToRGBString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
end;

// Vérifie si un point est présent dans un tableau de points
function PointInArray(p: TPoint; arr: array of TPoint): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(arr) do
    if (arr[i].x = p.x) and (arr[i].y = p.y) then
    begin
      Result := True;
      Break;
    end;
end;

// Initialise les cellules avec des informations aléatoires
procedure InitializeCells();
var
  i, j, number, colorIndex: Integer;
begin
  number := 1;
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
    begin
      Cells[j, i].Number := number;
      colorIndex := GetRandomValue(1, 10);
      Cells[j, i].Color := Colors[colorIndex];
      Cells[j, i].ColorIndex := colorIndex;
      Cells[j, i].TerrainType := Grass;
      Cells[j, i].HasRoad := False;
      Cells[j, i].HasRiver := False;
      Cells[j, i].HasBridge := False;
      Cells[j, i].HasPath := False;
      Cells[j, i].Level := 0;
      Inc(number);
    end;
end;

// Efface la grille actuelle et en génère une nouvelle
procedure ResetGrid();
begin
  InitializeCells();
  SetLength(SelectedCells, 0);  // Réinitialise la sélection
  CellSelected := False;
end;

// Efface toutes les couleurs et informations de la grille
procedure ClearGrid();
var
  i, j, number: Integer;
begin
  number := 1;
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
    begin
      Cells[j, i].Number := number;
      Cells[j, i].Color := BLANK;  // Efface la couleur (BLANK est transparent)
      Cells[j, i].ColorIndex := 0;
      Cells[j, i].TerrainType := Grass;
      Cells[j, i].HasRoad := False;
      Cells[j, i].HasRiver := False;
      Cells[j, i].HasBridge := False;
      Cells[j, i].HasPath := False;
      Cells[j, i].Level := 0;
      Inc(number);
    end;
  SetLength(SelectedCells, 0);
  CellSelected := False;
end;

// Gère la détection de clic et de sélection multiple
procedure HandleMouseClick();
var
  mouseX, mouseY, col, row: Integer;
  newPoint: TPoint;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();
  if (mouseX >= 0) and (mouseX < Columns * CellSize) and (mouseY >= 0) and (mouseY < Rows * CellSize) then
  begin
    col := mouseX div CellSize;
    row := mouseY div CellSize;
    newPoint := Point(col, row);

    if IsMouseButtonDown(MOUSE_LEFT_BUTTON) then
    begin
      if LargeCellSelected and (Length(SelectedCells) > 0) then
      begin
        // Ajoute seulement si la case n'est pas déjà dans la liste
        if not PointInArray(newPoint, SelectedCells) then
        begin
          SetLength(SelectedCells, Length(SelectedCells) + 1);
          SelectedCells[High(SelectedCells)] := newPoint;
        end;
      end
      else
      begin
        // Si aucune cellule n'est sélectionnée, démarre une nouvelle sélection
        SetLength(SelectedCells, 1);
        SelectedCells[0] := newPoint;
        LargeCellSelected := True;
      end;
    end;
  end;
end;

// Applique la couleur sélectionnée aux cases sélectionnées
procedure HandleSmallSquareClick();
var
  xPos, yPos, colorIndex: Integer;
  i: Integer;
begin
  for colorIndex := 1 to 10 do
  begin
    xPos := (colorIndex - 1) mod 5 * (SmallCellSize + 10) + Columns * CellSize + 30;
    yPos := ((colorIndex - 1) div 5) * (SmallCellSize + 10) + 200;

    if CheckCollisionPointRec(GetMousePosition(), RectangleCreate(xPos, yPos, SmallCellSize, SmallCellSize)) then
    begin
      if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and (Length(SelectedCells) > 0) then
      begin
        for i := 0 to High(SelectedCells) do
        begin
          Cells[SelectedCells[i].y, SelectedCells[i].x].Color := Colors[colorIndex];
          Cells[SelectedCells[i].y, SelectedCells[i].x].ColorIndex := colorIndex;
        end;
      end;
    end;
  end;
end;

// Gère les clics de la souris sur les boutons et les cases
procedure HandleMouseInput();
begin
  SaveButtonPressed := False;
  LoadButtonPressed := False;
  NewGridButtonPressed := False;
  EmptyGridButtonPressed := False;
  RouteButtonPressed := False;

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    if CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
    begin
      SaveButtonPressed := True;
     // SaveGridToFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    begin
      LoadButtonPressed := True;
     // LoadGridFromFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), NewGridButtonRec) then
    begin
      NewGridButtonPressed := True;
      ResetGrid();
    end;
    if CheckCollisionPointRec(GetMousePosition(), EmptyGridButtonRec) then
    begin
      EmptyGridButtonPressed := True;
      ClearGrid();
    end;
  end;

  HandleMouseClick();
  HandleSmallSquareClick();
end;

// Dessine les grands carrés avec informations
procedure DrawGridWithCellInfo();
var
  xPos, yPos, i, j: Integer;
begin
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;
      DrawRectangle(xPos, yPos, CellSize, CellSize, Cells[j, i].Color);
      DrawRectangleLines(xPos, yPos, CellSize, CellSize, DARKGRAY);
    end;

  for i := 0 to High(SelectedCells) do
  begin
    DrawRectangleLinesEx(RectangleCreate(SelectedCells[i].x * CellSize, SelectedCells[i].y * CellSize, CellSize, CellSize), 3, ORANGE);
  end;
end;

// Programme principal
begin
  Colors[1] := BLUE; Colors[2] := GREEN; Colors[3] := YELLOW; Colors[4] := ORANGE;
  Colors[5] := PURPLE; Colors[6] := SKYBLUE; Colors[7] := PINK; Colors[8] := BROWN;
  Colors[9] := DARKGREEN; Colors[10] := LIME;

  screenWidth := Columns * CellSize + InfoBoxWidth + 40;
  screenHeight := Rows * CellSize + 500;

  InitWindow(screenWidth, screenHeight, 'Grid with Bounce Effect');
  SetTargetFPS(60);

  InitializeCells();
  CellSelected := False;

  SaveButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 300, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 250, ButtonWidth, ButtonHeight);
  NewGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 200, ButtonWidth, ButtonHeight);
  EmptyGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 150, ButtonWidth, ButtonHeight);

  while not WindowShouldClose() do
  begin
    HandleMouseInput();
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawGridWithCellInfo();
    EndDrawing();
  end;

  CloseWindow();
end.


