program DrawGridWithCellSelection;
uses raylib, sysutils; // sysutils pour IntToStr conversion

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Espacement pour placer le texte dans la case
  SmallFontSize = 12; // Taille de la police réduite pour le texte dans les cases
  InfoBoxWidth = 300; // Largeur du rectangle pour afficher les informations de la case

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
      // Définir les informations de base pour chaque case
      Cells[j, i].Number := number;          // Numérotation séquentielle
      colorIndex := GetRandomValue(1, 10);   // Choisir une couleur aléatoire parmi les 10 disponibles
      Cells[j, i].Color := Colors[colorIndex]; // Assigner la couleur à la case
      Cells[j, i].ColorIndex := colorIndex;  // Stocker l'indice de la couleur

      // Valeurs par défaut pour les autres attributs
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

// Gère la détection de clic sur une case et retourne le numéro et la couleur
procedure HandleMouseClick();
var
  mouseX, mouseY, col, row: Integer;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();

  // Vérifie si la souris est dans les limites de la grille
  if (mouseX >= 0) and (mouseX < Columns * CellSize) and (mouseY >= 0) and (mouseY < Rows * CellSize) then
  begin
    // Calculer la colonne et la ligne de la case cliquée
    col := mouseX div CellSize;
    row := mouseY div CellSize;

    // Sélectionner la case
    SelectedRow := row;
    SelectedCol := col;
    CellSelected := True;

    // Construire la chaîne d'informations à afficher
    SelectedCellInfo := Format('Case #%d\nCouleur: %d\n%s',
      [Cells[row, col].Number, Cells[row, col].ColorIndex, ColorToRGBString(Cells[row, col].Color)]);
  end;
end;

// Dessiner la grille avec les informations de base
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
      DrawText(PChar(IntToStr(Cells[j, i].Number)), xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);

      // Afficher l'indice de la couleur
      DrawText(PChar('Clr: ' + IntToStr(Cells[j, i].ColorIndex)), xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
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
    DrawText(PChar(SelectedCellInfo), Columns * CellSize + 30, 30, SmallFontSize, BLACK)
  else
    DrawText('Cliquez sur une case pour afficher ses informations.', Columns * CellSize + 30, 30, SmallFontSize, BLACK);
end;

begin
  // Définir 10 couleurs différentes excluant le rouge
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

  // Calculer la taille de l'écran en fonction de la grille et du rectangle d'informations
  screenWidth := Columns * CellSize + InfoBoxWidth + 40;
  screenHeight := Rows * CellSize;

  InitWindow(screenWidth, screenHeight, 'Grid with Cell Selection and Info Box');

  SetTargetFPS(60); // Régler la fréquence d'images sur 60 FPS

  InitializeCells(); // Initialiser les cellules avec des attributs aléatoires
  CellSelected := False; // Aucune case n'est sélectionnée au démarrage

  while not WindowShouldClose() do
  begin
    HandleMouseClick(); // Vérifie si une case a été cliquée

    BeginDrawing();
    ClearBackground(RAYWHITE); // Couleur de fond

    DrawGridWithCellInfo(); // Dessiner la grille avec les informations
    DrawSelectedCellInfo(); // Afficher les informations de la case sélectionnée

    EndDrawing();
  end;

  CloseWindow();
end.













