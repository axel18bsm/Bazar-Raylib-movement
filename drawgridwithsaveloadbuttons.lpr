program DrawGridWithSaveLoadButtons;
uses raylib, sysutils, classes; // sysutils pour IntToStr conversion et classes pour TFileStream

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Espacement pour placer le texte dans la case
  SmallFontSize = 12; // Taille de la police réduite pour le texte dans les cases
  InfoBoxWidth = 300; // Largeur du rectangle pour afficher les informations de la case
  ButtonWidth = 150; // Largeur des boutons
  ButtonHeight = 40; // Hauteur des boutons
  SaveFileName = 'grid_save.txt'; // Nom du fichier de sauvegarde

type
  // Définition des différents types de terrains
  TTerrainType = (Grass, Swamp, Field, Forest, Village, House);

  // Définition de la structure pour chaque case de la grille
  TCell = record
    Number: Integer;         // Numéro de la case
    Color: TColor;           // Couleur de la case (RGB)
    ColorIndex: Integer;     // Indice de la couleur (1 à 10)
    TerrainType: TTerrainType; // Type de terrain
    HasRoad: Boolean;        // Indique si la case a une route
    HasRiver: Boolean;       // Indique si la case a une rivière
    HasBridge: Boolean;      // Indique si la case a un pont
    HasPath: Boolean;        // Indique si la case a un chemin
    Level: Integer;          // Niveau de la case (-2, -1, 0, 1, 2)
  end;

var
  screenWidth, screenHeight: Integer;
  i, j,numText,clrText, number: Integer;
  Cells: array[0..Rows-1, 0..Columns-1] of TCell; // Tableau pour stocker les informations de chaque case
  Colors: array[1..10] of TColor;
  SelectedRow, SelectedCol: Integer; // Stocke la position de la case sélectionnée (ligne, colonne)
  CellSelected: Boolean; // Indique si une case a été sélectionnée
  SelectedCellInfo: String; // Informations de la case sélectionnée
  SaveButtonRec, LoadButtonRec: TRectangle; // Rectangles des boutons

// Convertit une couleur en chaîne contenant ses valeurs RGB
function ColorToRGBString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
end;

// Découpe une chaîne en fonction d'un séparateur et retourne le mot à l'index spécifié
function GetWordAtIndex(const line: String; index: Integer; delimiter: Char): String;
var
  i, startPos, endPos, count: Integer;
begin
  startPos := 1;
  count := 1;
  Result := '';

  for i := 1 to Length(line) do
  begin
    if (line[i] = delimiter) or (i = Length(line)) then
    begin
      if count = index then
      begin
        endPos := i;
        if i = Length(line) then
          endPos := i + 1; // In case of last word, include last character
        Result := Copy(line, startPos, endPos - startPos);
        Exit;
      end;
      Inc(count);
      startPos := i + 1;
    end;
  end;
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
      Cells[j, i].Number := number;          // Numérotation séquentielle
      colorIndex := GetRandomValue(1, 10);   // Choisir une couleur aléatoire parmi les 10 disponibles
      Cells[j, i].Color := Colors[colorIndex]; // Assigner la couleur à la case
      Cells[j, i].ColorIndex := colorIndex;  // Stocker l'indice de la couleur
      Cells[j, i].TerrainType := Grass; // Mettre un type de terrain par défaut
      Cells[j, i].HasRoad := False;
      Cells[j, i].HasRiver := False;
      Cells[j, i].HasBridge := False;
      Cells[j, i].HasPath := False;
      Cells[j, i].Level := 0;
      Inc(number); // Passer à la case suivante
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
    // Lire les informations de la ligne et mettre à jour les cases
    with Cells[row, col] do
    begin
      Number := StrToInt(GetWordAtIndex(line, 1, ','));
      ColorIndex := StrToInt(GetWordAtIndex(line, 2, ','));
      r := StrToInt(GetWordAtIndex(line, 3, ','));
      g := StrToInt(GetWordAtIndex(line, 4, ','));
      b := StrToInt(GetWordAtIndex(line, 5, ','));
      HasRoad := StrToBool(GetWordAtIndex(line, 6, ','));
      HasRiver := StrToBool(GetWordAtIndex(line, 7, ','));
      HasBridge := StrToBool(GetWordAtIndex(line, 8, ','));
      HasPath := StrToBool(GetWordAtIndex(line, 9, ','));
      Level := StrToInt(GetWordAtIndex(line, 10, ','));

      Color := ColorCreate(r, g, b, 255); // Reconstruire la couleur
      TerrainType := Grass; // Terrain par défaut (pas dans le fichier)
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

// Dessiner la grille avec les informations de chaque case
procedure DrawGridWithCellInfo();
var
  xPos, yPos: Integer;
begin
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;

      // Dessiner la case avec sa couleur
      DrawRectangle(xPos, yPos, CellSize, CellSize, Cells[j, i].Color);

      // Afficher le numéro de la case
      StrPCopy(numText, IntToStr(Cells[j, i].Number));
      DrawText(numText, xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Convertir l'indice de la couleur en PChar et l'afficher
      StrPCopy(clrText, 'Clr: ' + IntToStr(Cells[j, i].ColorIndex));
      DrawText(clrText, xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
    //  DrawText(IntToStr(Cells[j, i].Number), xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Afficher l'indice de la couleur
      //DrawText('Clr: ' + IntToStr(Cells[j, i].ColorIndex), xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
    end;
  end;

  // Si une case est sélectionnée, dessiner un contour orange
  if CellSelected then
  begin
    xPos := SelectedCol * CellSize;
    yPos := SelectedRow * CellSize;
    DrawRectangleLinesEx(RectangleCreate(xPos, yPos, CellSize, CellSize), 3, ORANGE);
  end;
end;

// Affiche les informations de la case sélectionnée dans un rectangle dédié
procedure DrawSelectedCellInfo();
begin
  // Dessiner le rectangle d'informations
  DrawRectangle(Columns * CellSize + 20, 20, InfoBoxWidth, 150, LIGHTGRAY);
  DrawRectangleLines(Columns * CellSize + 20, 20, InfoBoxWidth, 150, DARKGRAY);

  // Afficher le texte avec les informations de la case sélectionnée
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

// Gère les entrées de la souris pour les boutons et la sélection des cases
procedure HandleMouseInput();
begin
  // Vérifie si le bouton "Sauvegarder" est cliqué
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
    SaveGridToFile();

  // Vérifie si le bouton "Charger" est cliqué
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    LoadGridFromFile();

  // Vérifie si une case a été cliquée
  HandleMouseClick();
end;

begin
  // Définir les couleurs
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

















