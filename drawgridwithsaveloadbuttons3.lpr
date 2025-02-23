program DrawGridWithSaveLoadButtons;
uses raylib, sysutils, classes;

const
  CellSize = 50;
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
  i, j, number: Integer;
  Cells: array[0..Rows-1, 0..Columns-1] of TCell;
  Colors: array[1..10] of TColor;
  SelectedRow, SelectedCol: Integer;
  CellSelected: Boolean;
  SelectedCellInfo: array[0..255] of Char;
  SaveButtonRec, LoadButtonRec: TRectangle;

// Convertit une couleur en chaîne contenant ses valeurs RGB
function ColorToRGBString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
end;

// Initialise les cellules avec des informations aléatoires
procedure InitializeCells();
var
  colorIndex: Integer;
begin
  number := 1;
  for j := 0 to Rows - 1 do
  begin
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
end;

// Sauvegarde les informations de la grille dans un fichier texte
procedure SaveGridToFile();
var
  FileHandle: TextFile;
  row, col: Integer;
begin
  AssignFile(FileHandle, SaveFileName);
  Rewrite(FileHandle);
  for row := 0 to Rows - 1 do
  begin
    for col := 0 to Columns - 1 do
    begin
      with Cells[row, col] do
      begin
        WriteLn(FileHandle, Format('%d,%d,%d,%d,%d,%s,%s,%s,%s,%d',
          [Number, ColorIndex, Color.r, Color.g, Color.b,
           BoolToStr(HasRoad, True), BoolToStr(HasRiver, True),
           BoolToStr(HasBridge, True), BoolToStr(HasPath, True), Level]));
      end;
    end;
  end;
  CloseFile(FileHandle);
end;

// Charge les informations de la grille à partir du fichier texte
procedure LoadGridFromFile();
var
  FileHandle: TextFile;
  row, col: Integer;
  r, g, b, ColorIndex, Number, Level: Integer;
  HasRoad, HasRiver, HasBridge, HasPath: Boolean;
  line: String;
begin
  AssignFile(FileHandle, SaveFileName);
  Reset(FileHandle);
  row := 0;
  col := 0;
  while not EOF(FileHandle) do
  begin
    ReadLn(FileHandle, line);
    with Cells[row, col] do
    begin
      Number := StrToInt(ExtractDelimited(1, line, [',']));
      ColorIndex := StrToInt(ExtractDelimited(2, line, [',']));
      r := StrToInt(ExtractDelimited(3, line, [',']));
      g := StrToInt(ExtractDelimited(4, line, [',']));
      b := StrToInt(ExtractDelimited(5, line, [',']));
      HasRoad := StrToBool(ExtractDelimited(6, line, [',']));
      HasRiver := StrToBool(ExtractDelimited(7, line, [',']));
      HasBridge := StrToBool(ExtractDelimited(8, line, [',']));
      HasPath := StrToBool(ExtractDelimited(9, line, [',']));
      Level := StrToInt(ExtractDelimited(10, line, [',']));

      Color := ColorCreate(r, g, b, 255);
      TerrainType := Grass; // Valeur par défaut
    end;
    Inc(col);
    if col >= Columns then
    begin
      col := 0;
      Inc(row);
    end;
  end;
  CloseFile(FileHandle);
end;

// Gère la détection de clic sur une case et met à jour les informations
procedure HandleMouseClick();
var
  mouseX, mouseY, col, row: Integer;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();

  if (mouseX >= 0) and (mouseX < Columns * CellSize) and (mouseY >= 0) and (mouseY < Rows * CellSize) then
  begin
    col := mouseX div CellSize;
    row := mouseY div CellSize;

    SelectedRow := row;
    SelectedCol := col;
    CellSelected := True;

    StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur: %d\n%s',
      [Cells[row, col].Number, Cells[row, col].ColorIndex, ColorToRGBString(Cells[row, col].Color)]));
  end;
end;

// Dessiner la grille avec les informations de chaque case
procedure DrawGridWithCellInfo();
var
  xPos, yPos: Integer;
  numText, clrText: array[0..15] of Char;
begin
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;

      DrawRectangle(xPos, yPos, CellSize, CellSize, Cells[j, i].Color);

      StrPCopy(numText, IntToStr(Cells[j, i].Number));
      DrawText(numText, xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      StrPCopy(clrText, 'Clr: ' + IntToStr(Cells[j, i].ColorIndex));
      DrawText(clrText, xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
    end;
  end;

  if CellSelected then
  begin
    xPos := SelectedCol * CellSize;
    yPos := SelectedRow * CellSize;
    DrawRectangleLinesEx(RectangleCreate(xPos, yPos, CellSize, CellSize), 3, ORANGE);
  end;
end;

// Affiche les informations de la case sélectionnée
procedure DrawSelectedCellInfo();
begin
  DrawRectangle(Columns * CellSize + 20, 20, InfoBoxWidth, 150, LIGHTGRAY);
  DrawRectangleLines(Columns * CellSize + 20, 20, InfoBoxWidth, 150, DARKGRAY);

  if CellSelected then
    DrawText(SelectedCellInfo, Columns * CellSize + 30, 30, SmallFontSize, BLACK)
  else
    DrawText('Cliquez sur une case pour afficher ses informations.', Columns * CellSize + 30, 30, SmallFontSize, BLACK);
end;

// Dessiner les boutons de sauvegarde et de chargement
procedure DrawButtons();
begin
  DrawRectangleRec(SaveButtonRec, LIGHTGRAY);
  DrawRectangleRec(LoadButtonRec, LIGHTGRAY);
  DrawText('Sauvegarder', Round(SaveButtonRec.x + 10), Round(SaveButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Charger', Round(LoadButtonRec.x + 10), Round(LoadButtonRec.y + 10), SmallFontSize, BLACK);
end;

// Gère les clics de la souris sur les boutons et les cases
procedure HandleMouseInput();
begin
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
    SaveGridToFile();

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    LoadGridFromFile();

  HandleMouseClick();
end;

begin
  Colors[1] := BLUE; Colors[2] := GREEN; Colors[3] := YELLOW; Colors[4] := ORANGE;
  Colors[5] := PURPLE; Colors[6] := SKYBLUE; Colors[7] := PINK; Colors[8] := BROWN;
  Colors[9] := DARKGREEN; Colors[10] := LIME;

  screenWidth := Columns * CellSize + InfoBoxWidth + 40;
  screenHeight := Rows * CellSize + 200;

  InitWindow(screenWidth, screenHeight, 'Grid with Save/Load Buttons');

  SetTargetFPS(60);

  InitializeCells();

  CellSelected := False;

  SaveButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 100, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 50, ButtonWidth, ButtonHeight);

  while not WindowShouldClose() do
  begin
    HandleMouseInput();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawGridWithCellInfo();
    DrawSelectedCellInfo();
    DrawButtons();

    EndDrawing();
  end;

  CloseWindow();
end.



















