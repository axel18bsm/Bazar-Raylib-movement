program drawgridwithbounceeffect8;
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
  Cells: array[0..Rows-1, 0..Columns-1] of TCell;
  Colors: array[1..10] of TColor;
  SelectedRow, SelectedCol: Integer;
  CellSelected: Boolean;
  SelectedCellInfo: array[0..255] of Char;
  SaveButtonRec, LoadButtonRec, NewGridButtonRec: TRectangle; // Ajouter le rectangle pour le bouton Nouvelle Grille
  Fields: array of String;
  SaveButtonPressed, LoadButtonPressed, NewGridButtonPressed: Boolean; // Ajouter une variable pour l'état du bouton Nouvelle Grille

// Convertit une couleur en chaîne contenant ses valeurs RGB
function ColorToRGBString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
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
  InitializeCells();  // Réinitialise toutes les cellules de la grille
  CellSelected := False; // Réinitialise la sélection de cellule
end;

// Découpe une ligne de texte en utilisant un délimiteur et stocke dans le tableau global Fields
procedure SplitLineIntoFields(const line: String; const delimiter: Char);
var
  i, startPos, fieldIndex: Integer;
begin
  startPos := 1;
  fieldIndex := 0;
  SetLength(Fields, 0);

  for i := 1 to Length(line) do
  begin
    if (line[i] = delimiter) or (i = Length(line)) then
    begin
      SetLength(Fields, fieldIndex + 1);
      if i = Length(line) then
        Fields[fieldIndex] := Copy(line, startPos, i - startPos + 1)
      else
        Fields[fieldIndex] := Copy(line, startPos, i - startPos);
      Inc(fieldIndex);
      startPos := i + 1;
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
    for col := 0 to Columns - 1 do
      with Cells[row, col] do
        WriteLn(FileHandle, Format('%d,%d,%d,%d,%d,%s,%s,%s,%s,%d',
          [Number, ColorIndex, Color.r, Color.g, Color.b,
           BoolToStr(HasRoad, True), BoolToStr(HasRiver, True),
           BoolToStr(HasBridge, True), BoolToStr(HasPath, True), Level]));
  CloseFile(FileHandle);
end;

// Charge les informations de la grille à partir du fichier texte
procedure LoadGridFromFile();
var
  FileHandle: TextFile;
  row, col: Integer;
  line: String;
begin
  AssignFile(FileHandle, SaveFileName);
  Reset(FileHandle);
  row := 0;
  col := 0;
  while not EOF(FileHandle) do
  begin
    ReadLn(FileHandle, line);
    SplitLineIntoFields(line, ',');
    if Length(Fields) = 10 then
      with Cells[row, col] do
      begin
        Number := StrToInt(Fields[0]);
        ColorIndex := StrToInt(Fields[1]);
        Color := ColorCreate(StrToInt(Fields[2]), StrToInt(Fields[3]), StrToInt(Fields[4]), 255);
        HasRoad := StrToBool(Fields[5]);
        HasRiver := StrToBool(Fields[6]);
        HasBridge := StrToBool(Fields[7]);
        HasPath := StrToBool(Fields[8]);
        Level := StrToInt(Fields[9]);
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

// Gère les clics de la souris sur les boutons et les cases
procedure HandleMouseInput();
begin
  SaveButtonPressed := False;
  LoadButtonPressed := False;
  NewGridButtonPressed := False;

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    if CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
    begin
      SaveButtonPressed := True;
      SaveGridToFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    begin
      LoadButtonPressed := True;
      LoadGridFromFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), NewGridButtonRec) then
    begin
      NewGridButtonPressed := True;
      ResetGrid(); // Réinitialise la grille avec une nouvelle configuration
    end;
  end;

  HandleMouseClick();
end;

// Dessine les boutons de sauvegarde, chargement, et "Nouvelle Grille"
procedure DrawButtons();
begin
  if SaveButtonPressed then
    DrawRectangleRec(SaveButtonRec, GRAY)
  else
    DrawRectangleRec(SaveButtonRec, LIGHTGRAY);

  if LoadButtonPressed then
    DrawRectangleRec(LoadButtonRec, GRAY)
  else
    DrawRectangleRec(LoadButtonRec, LIGHTGRAY);

  if NewGridButtonPressed then
    DrawRectangleRec(NewGridButtonRec, GRAY)
  else
    DrawRectangleRec(NewGridButtonRec, LIGHTGRAY);

  DrawText('Sauvegarder', Round(SaveButtonRec.x + 10), Round(SaveButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Charger', Round(LoadButtonRec.x + 10), Round(LoadButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Nouvelle Grille', Round(NewGridButtonRec.x + 10), Round(NewGridButtonRec.y + 10), SmallFontSize, BLACK);
end;

// Programme principal
begin
  Colors[1] := BLUE; Colors[2] := GREEN; Colors[3] := YELLOW; Colors[4] := ORANGE;
  Colors[5] := PURPLE; Colors[6] := SKYBLUE; Colors[7] := PINK; Colors[8] := BROWN;
  Colors[9] := DARKGREEN; Colors[10] := LIME;

  screenWidth := Columns * CellSize + InfoBoxWidth + 40;
  screenHeight := Rows * CellSize + 250; // Augmenter la hauteur de la fenêtre pour le troisième bouton

  InitWindow(screenWidth, screenHeight, 'Grid with Bounce Effect');
  SetTargetFPS(60);

  InitializeCells();
  CellSelected := False;

  SaveButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 150, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 100, ButtonWidth, ButtonHeight);
  NewGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 50, ButtonWidth, ButtonHeight); // Nouveau bouton "Nouvelle Grille"

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























