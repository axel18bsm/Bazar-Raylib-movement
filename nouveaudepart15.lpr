program nouveaudepart15;
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
  SelectedRow, SelectedCol, SelectedLargeRow, SelectedLargeCol: Integer;
  CellSelected, LargeCellSelected: Boolean;
  SelectedCellInfo: array[0..255] of Char;
  RouteButtonRec,SaveButtonRec, LoadButtonRec, NewGridButtonRec,EmptyGridButtonRec: TRectangle;
  Fields: array of String;
  RouteButtonPressed,SaveButtonPressed, LoadButtonPressed, NewGridButtonPressed,EmptyGridButtonPressed: Boolean;

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
  InitializeCells();
  CellSelected := False;
end;
// Efface toutes les couleurs et informations de la grille
procedure ClearGrid();
var
  i, j, number: Integer;
begin
  number := 1;  // Initialiser le numéro à 1 pour le premier carré
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
    begin
      Cells[j, i].Number := number;  // Assigner le numéro unique basé sur la position
      Cells[j, i].Color := BLANK;  // Efface la couleur (BLANK est transparent)
      Cells[j, i].ColorIndex := 0;  // Réinitialiser l'index de la couleur
      Cells[j, i].TerrainType := Grass;  // Réinitialiser le type de terrain
      Cells[j, i].HasRoad := False;  // Réinitialiser les chemins
      Cells[j, i].HasRiver := False;  // Réinitialiser les rivières
      Cells[j, i].HasBridge := False;  // Réinitialiser les ponts
      Cells[j, i].HasPath := False;  // Réinitialiser les sentiers
      Cells[j, i].Level := 0;  // Réinitialiser le niveau

      Inc(number);  // Incrémenter le numéro pour le prochain carré
    end;
  CellSelected := False;  // Désélectionner toutes les cases
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
procedure HandleLargeSquareClick();
var
  mouseX, mouseY, col, row: Integer;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();

  // Vérifie si le clic est dans la zone des grands carrés (grille principale)
  if (mouseX >= 0) and (mouseX < Columns * CellSize) and (mouseY >= 0) and (mouseY < Rows * CellSize) then
  begin
    col := mouseX div CellSize;
    row := mouseY div CellSize;

    // Sélectionne le grand carré et met à jour les informations de sélection
    SelectedLargeRow := row;
    SelectedLargeCol := col;
    LargeCellSelected := True;  // Marque le grand carré comme sélectionné
    CellSelected := True;       // Met à jour l'indicateur de cellule sélectionnée

    // Met à jour les informations de la cellule sélectionnée
    StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur: %d\n%s',
      [Cells[row, col].Number, Cells[row, col].ColorIndex, ColorToRGBString(Cells[row, col].Color),BoolToStr(Cells[SelectedLargeRow, SelectedLargeCol].HasRoad, True)]));
  end;
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
// Gère le clic sur les petits carrés et applique la couleur sélectionnée
procedure HandleSmallSquareClick();
var
  xPos, yPos, colorIndex: Integer;
begin
  for colorIndex := 1 to 10 do
  begin
    // Coordonnées pour chaque petit carré
    xPos := (colorIndex - 1) mod 5 * (SmallCellSize + 10) + Columns * CellSize + 30;
    yPos := ((colorIndex - 1) div 5) * (SmallCellSize + 10) + 200;

    // Vérifie si le clic est dans un petit carré et qu'un grand carré est sélectionné
    if CheckCollisionPointRec(GetMousePosition(), RectangleCreate(xPos, yPos, SmallCellSize, SmallCellSize)) then
    begin
      if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and LargeCellSelected then
      begin
        // Applique la couleur sélectionnée et son index dans le grand carré
        Cells[SelectedLargeRow, SelectedLargeCol].ColorIndex := colorIndex;  // Numéro de la couleur
        Cells[SelectedLargeRow, SelectedLargeCol].Number := (SelectedLargeRow * Columns) + SelectedLargeCol + 1;  // Numéro du grand carré
      // Numéro du petit carré (1 à 10)
        Cells[SelectedLargeRow, SelectedLargeCol].Color := Colors[colorIndex];  // Couleur RGB associée

        // Met à jour les informations affichées dans le rectangle d'information
        StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur Index: %d\n%s',
          [Cells[SelectedLargeRow, SelectedLargeCol].Number,               // Numéro du petit carré (1-10)
           Cells[SelectedLargeRow, SelectedLargeCol].ColorIndex,           // Index de la couleur
           ColorToRGBString(Cells[SelectedLargeRow, SelectedLargeCol].Color)]));  // Valeurs RGB
      end;
    end;
  end;
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
    SelectedLargeRow := row;
    SelectedLargeCol := col;
    LargeCellSelected := True;  // Grand carré sélectionné
    CellSelected := True;
    StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur: %d\n%s\nRoute: %s',
        [Cells[SelectedLargeRow, SelectedLargeCol].Number,
         Cells[SelectedLargeRow, SelectedLargeCol].ColorIndex,
         ColorToRGBString(Cells[SelectedLargeRow, SelectedLargeCol].Color),
         BoolToStr(Cells[SelectedLargeRow, SelectedLargeCol].HasRoad, True)]));
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
      SaveGridToFile();
    end
    else if CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    begin
      LoadButtonPressed := True;
      LoadGridFromFile();
    end
    else if CheckCollisionPointRec(GetMousePosition(), NewGridButtonRec) then
    begin
      NewGridButtonPressed := True;
      ResetGrid();
    end
    else if CheckCollisionPointRec(GetMousePosition(), EmptyGridButtonRec) then
    begin
      EmptyGridButtonPressed := True;
      ClearGrid();
    end
    else if CheckCollisionPointRec(GetMousePosition(), RouteButtonRec) and LargeCellSelected then
    begin
      RouteButtonPressed := True;
      // Basculer la valeur de HasRoad dans le grand carré sélectionné
      if Cells[SelectedLargeRow, SelectedLargeCol].HasRoad = false then
       Cells[SelectedLargeRow, SelectedLargeCol].HasRoad:=true
       else Cells[SelectedLargeRow, SelectedLargeCol].HasRoad:=false ;
      // Mettre à jour les informations affichées
      StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur: %d\n%s\nRoute: %s',
        [Cells[SelectedLargeRow, SelectedLargeCol].Number,
         Cells[SelectedLargeRow, SelectedLargeCol].ColorIndex,
         ColorToRGBString(Cells[SelectedLargeRow, SelectedLargeCol].Color),
         BoolToStr(Cells[SelectedLargeRow, SelectedLargeCol].HasRoad, false)]));
    end
    else
    begin
      // Si le clic n'est pas sur un bouton, on vérifie les grands carrés
      HandleLargeSquareClick();
      HandleSmallSquareClick();  // Puis on vérifie les petits carrés de couleur
    end;
  end;
end;

// Dessine la grille avec les informations de chaque case
procedure DrawGridWithCellInfo();
var
  xPos, yPos, j, i: Integer;
  numText, clrText: array[0..15] of Char;
begin
  for j := 0 to Rows - 1 do
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
  if CellSelected then
  begin
    xPos := SelectedCol * CellSize;
    yPos := SelectedRow * CellSize;
    DrawRectangleLinesEx(RectangleCreate(xPos, yPos, CellSize, CellSize), 3, ORANGE);
  end;
end;
procedure DrawColorPalette();
var
  xPos, yPos, colorIndex: Integer;
  numText: array[0..5] of Char;
begin
  for colorIndex := 1 to 10 do
  begin
    // Coordonnées pour chaque petit carré
    xPos := (colorIndex - 1) mod 5 * (SmallCellSize + 10) + Columns * CellSize + 30;
    yPos := ((colorIndex - 1) div 5) * (SmallCellSize + 10) + 200;  // Dessine 2 lignes (2 x 5)

    // Dessine chaque petit carré de couleur
    DrawRectangle(xPos, yPos, SmallCellSize, SmallCellSize, Colors[colorIndex]);

    // Affiche le numéro correspondant à la couleur dans le carré
    StrPCopy(numText, IntToStr(colorIndex));
    DrawText(numText, xPos + 15, yPos + 10, SmallFontSize, BLACK);
  end;
end;

// Affiche les informations de la case sélectionnée dans un rectangle dédié
procedure DrawSelectedCellInfo();
begin
  DrawRectangle(Columns * CellSize + 20, 20, InfoBoxWidth, 150, LIGHTGRAY);
  DrawRectangleLines(Columns * CellSize + 20, 20, InfoBoxWidth, 150, DARKGRAY);
  if CellSelected then
    DrawText(SelectedCellInfo, Columns * CellSize + 30, 30, SmallFontSize, BLACK)
  else
    DrawText('Cliquez sur une case pour afficher ses informations.', Columns * CellSize + 30, 30, SmallFontSize, BLACK);
end;

// Dessine les boutons de sauvegarde, chargement et "Nouvelle Grille"
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

  if EmptyGridButtonPressed then
    DrawRectangleRec(EmptyGridButtonRec, GRAY)
  else
    DrawRectangleRec(EmptyGridButtonRec, LIGHTGRAY);

  if EmptyGridButtonPressed then
    DrawRectangleRec(RouteButtonRec, GRAY)
  else
    DrawRectangleRec(RouteButtonRec, LIGHTGRAY);

  DrawText('Sauvegarder', Round(SaveButtonRec.x + 10), Round(SaveButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Charger', Round(LoadButtonRec.x + 10), Round(LoadButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Nouvelle Grille', Round(NewGridButtonRec.x + 10), Round(NewGridButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Grille Vide', Round(EmptyGridButtonRec.x + 10), Round(EmptyGridButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Route', Round(RouteButtonRec.x + 10), Round(RouteButtonRec.y + 10), SmallFontSize, BLACK);

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

  SaveButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 200, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 150, ButtonWidth, ButtonHeight);
  NewGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 100, ButtonWidth, ButtonHeight);
  EmptyGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 50, ButtonWidth, ButtonHeight); // Ajout du bouton "Grille Vide"
  RouteButtonRec := RectangleCreate(Columns * CellSize + 50, 310, ButtonWidth, ButtonHeight);

  while not WindowShouldClose() do
  begin
    HandleMouseInput();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawGridWithCellInfo();
    DrawSelectedCellInfo();
    DrawButtons();
    DrawColorPalette(); // Affiche la palette de couleurs
    EndDrawing();
  end;

  CloseWindow();
end.






















