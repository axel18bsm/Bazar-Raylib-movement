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
  i, j, number: Integer;
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

// Dessiner la grille avec les informations de chaque case
procedure DrawGridWithCellInfo();
var
  xPos, yPos: Integer;
  numText, clrText: array[0..15] of Char; // Buffers pour stocker les PChar
begin
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;

      // Dessiner la case avec sa couleur
      DrawRectangle(xPos, yPos, CellSize, CellSize, Cells[j, i].Color);

      // Convertir le numéro de la case en PChar et l'afficher
      StrPCopy(numText, IntToStr(Cells[j, i].Number));
      DrawText(numText, xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Convertir l'indice de la couleur en PChar et l'afficher
      StrPCopy(clrText, 'Clr: ' + IntToStr(Cells[j, i].ColorIndex));
      DrawText(clrText, xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
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
    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawGridWithCellInfo();

    EndDrawing();
  end;

  CloseWindow();
end.


















