program DrawGridWithSimpleInfo;
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
    ColorIndex: Integer;     // Indice de la couleur (1 à 10)
    TerrainType: TTerrainType; // Type de terrain (non utilisé dans cet affichage)
    HasRoad: Boolean;        // Route (non utilisé dans cet affichage)
    HasRiver: Boolean;       // Rivière (non utilisé dans cet affichage)
    HasBridge: Boolean;      // Pont (non utilisé dans cet affichage)
    HasPath: Boolean;        // Chemin (non utilisé dans cet affichage)
    Level: Integer;          // Niveau de la case (non utilisé dans cet affichage)
  end;

var
  screenWidth, screenHeight: Integer;
  i, j, number: Integer;
  Cells: array[0..Rows-1, 0..Columns-1] of TCell; // Tableau pour stocker les informations de chaque case
  Colors: array[1..10] of TColor;

// Convertit une couleur en chaîne contenant ses valeurs RGB
function ColorToRGBString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
end;

// Procedure to randomly assign attributes to cells (done once)
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

      Inc(number); // Passer à la case suivante
    end;
  end;
end;

// Procedure to draw the grid with colors and display information
procedure DrawGridWithCellInfo();
var
  xPos, yPos: Integer;
  RGBString: String; // Chaîne contenant les informations RGB
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

      // Générer la chaîne RGB et l'afficher
      RGBString := ColorToRGBString(Cells[j, i].Color);
      DrawText(PChar(RGBString), xPos + TextPadding, yPos + 35, SmallFontSize, BLACK);
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

  InitWindow(screenWidth, screenHeight, 'Grid with RGB and Color Info');

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












