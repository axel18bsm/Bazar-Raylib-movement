program DrawGridWithCellStructures;
uses raylib, sysutils; // sysutils pour IntToStr conversion

const
  CellSize = 50;
  Rows = 10;
  Columns = 12;
  TextPadding = 5; // Espacement pour placer le texte dans la case
  SmallFontSize = 12; // Taille de la police réduite pour le texte dans les cases

type
  // Définition des différents types de terrains
  TTerrainType = (Grass, Swamp, Field, Forest, Village, House);

  // Définition de la structure pour chaque case de la grille
  TCell = record
    Number: Integer;         // Numéro de la case
    Color: TColor;           // Couleur de la case (RGB)
    TerrainType: TTerrainType; // Type de terrain
    HasRoad: Boolean;        // Indique si la case a une route
    HasRiver: Boolean;       // Indique si la case a une rivière
    HasBridge: Boolean;      // Indique si la case a un pont
    HasPath: Boolean;        // Indique si la case a un chemin
    Level: Integer;          // Niveau de la case (-2 pour -100m, -1 pour -50m, 0 pour 0m, 1 pour +50m, 2 pour +100m)
  end;

var
  screenWidth, screenHeight: Integer;
  i, j, number: Integer;
  Cells: array[0..Rows-1, 0..Columns-1] of TCell; // Tableau pour stocker les informations de chaque case
  TerrainNames: array[TTerrainType] of String = ('Herbe', 'Marais', 'Champ', 'Forêt', 'Village', 'Maison'); // Noms des types de terrain
  Colors: array[1..10] of TColor;

// Procedure to randomly assign attributes to cells (done once)
procedure InitializeCells();
var
  terrainType: TTerrainType;
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
      terrainType := TTerrainType(GetRandomValue(0, 5)); // Terrain aléatoire parmi les 6 types
      Cells[j, i].TerrainType := terrainType; // Assigner le type de terrain
      Cells[j, i].HasRoad := False;           // Initialiser à False pour les booléens
      Cells[j, i].HasRiver := False;
      Cells[j, i].HasBridge := False;
      Cells[j, i].HasPath := False;
      Cells[j, i].Level := 0;                 // Initialiser à 0 pour le niveau

      Inc(number); // Passer à la case suivante
    end;
  end;
end;

// Procedure to draw the grid with colors and display information
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

      // Afficher le type de terrain
      DrawText(PChar(TerrainNames[Cells[j, i].TerrainType]), xPos + TextPadding, yPos + 25, SmallFontSize, BLACK);

      // Afficher le niveau de la case
      DrawText(PChar('Niv: ' + IntToStr(Cells[j, i].Level)), xPos + TextPadding, yPos + 40, SmallFontSize, BLACK);
    end;
  end;
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

  // Calculer la taille de l'écran en fonction de la grille
  screenWidth := Columns * CellSize;
  screenHeight := Rows * CellSize;

  InitWindow(screenWidth, screenHeight, 'Grid with Cell Structures');

  SetTargetFPS(60); // Régler la fréquence d'images sur 60 FPS

  InitializeCells(); // Initialiser les cellules avec des attributs aléatoires

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE); // Couleur de fond

    DrawGridWithCellInfo(); // Appeler la fonction pour dessiner la grille avec les informations des cases

    EndDrawing();
  end;

  CloseWindow();
end.











