program hexflattopinfo;

uses raylib, math, sysutils;

const
  HexDiameter = 60;         // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexWidth = HexDiameter;    // Largeur de l'hexagone (flat-top)
  HexHeight = HexRadius * sqrt(3);  // Hauteur totale d'un hexagone (distance entre deux sommets opposés)
  Columns = 6;               // Nombre d'hexagones en largeur
  Rows = 5;                  // Nombre d'hexagones en hauteur
  InfoBoxHeight = 150;       // Hauteur de la zone d'information
  WindowWidth = 800;         // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre, inclut la zone d'information

type
  TPoint = record
    x, y: Integer;
  end;

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: Integer;           // Numéro de l'hexagone (de 1 à 30)
    Center: TVector2;          // Vecteur du centre de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;             // Couleur de l'hexagone
    Selected: Boolean;         // État de sélection
    Neighbors: array[0..5] of Integer;  // Numéros des voisins contigus (6 voisins)
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j, hexNumber: Integer;
  SelectedHex: THexCell;  // Hexagone actuellement sélectionné
  HexSelected: Boolean;   // Indique si un hexagone est sélectionné
  BlinkCounter: Integer;  // Compteur pour le clignotement

// Calcule les 6 sommets d'un hexagone "tête plate"
procedure CalculateHexVertices(var Hex: THexCell);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 60 * k;  // Angle de rotation pour un hexagone "tête plate"
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
    Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
  end;
end;

// Calcule les voisins d'un hexagone
procedure CalculateNeighbors();
var
  k: Integer;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      // Initialise les voisins à 0 (pas de voisin)
      for k := 0 to 5 do
        HexGrid[i][j].Neighbors[k] := 0;

      // Détermine les voisins en fonction de la position
      if j > 0 then HexGrid[i][j].Neighbors[0] := HexGrid[i][j - 1].Number; // Voisin du haut

      if (i < Columns - 1) and ((i mod 2 = 0) or (j > 0)) then
        HexGrid[i][j].Neighbors[1] := HexGrid[i + 1][j - (i mod 2)].Number; // Voisin haut-droite

      if (i < Columns - 1) and ((i mod 2 = 1) or (j < Rows - 1)) then
        HexGrid[i][j].Neighbors[2] := HexGrid[i + 1][j + (1 - (i mod 2))].Number; // Voisin bas-droite

      if j < Rows - 1 then HexGrid[i][j].Neighbors[3] := HexGrid[i][j + 1].Number; // Voisin du bas

      if (i > 0) and ((i mod 2 = 1) or (j < Rows - 1)) then
        HexGrid[i][j].Neighbors[4] := HexGrid[i - 1][j + (1 - (i mod 2))].Number; // Voisin bas-gauche

      if (i > 0) and ((i mod 2 = 0) or (j > 0)) then
        HexGrid[i][j].Neighbors[5] := HexGrid[i - 1][j - (i mod 2)].Number; // Voisin haut-gauche
    end;
  end;
end;

// Initialise la grille hexagonale "tête plate" avec des numéros, couleurs et centre
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  hexNumber := 1;  // Initialisation du compteur d'hexagones

  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      offsetX := i * (HexWidth * 0.75);  // Décalage horizontal avec 3/4 de la largeur d'un hexagone
      offsetY := j * HexHeight;
      if (i mod 2) = 1 then
        offsetY := offsetY + (HexHeight / 2);  // Décalage pour créer la disposition en "nid d'abeille"

      // Définition du centre de l'hexagone
      HexGrid[i][j].Center := Vector2Create(Round(HexRadius + offsetX), Round(HexRadius + offsetY));

      // Définit le numéro de l'hexagone
      HexGrid[i][j].Number := hexNumber;
      Inc(hexNumber);  // Incrémentation du numéro

      // Applique un motif de couleur en damier
      if (i + j) mod 2 = 0 then
        HexGrid[i][j].Color := GREEN
      else
        HexGrid[i][j].Color := LIGHTGRAY;

      HexGrid[i][j].Selected := False;
      CalculateHexVertices(HexGrid[i][j]); // Calcule les sommets de l'hexagone
    end;
  end;

  CalculateNeighbors(); // Calcule les voisins contigus de chaque hexagone
end;

// Dessine la grille hexagonale avec les numéros d'hexagones au centre
procedure DrawHexGrid();
var
  k: Integer;
  hexPoints: array[0..5] of tVector2;
  hexNumberText: array[0..5] of Char;
  outlineColor: TColor;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      // Convertir les sommets pour l'API de raylib
      for k := 0 to 5 do
        hexPoints[k] := Vector2Create(HexGrid[i][j].Vertices[k].x, HexGrid[i][j].Vertices[k].y);

      // Remplir chaque hexagone avec la couleur de fond
      DrawPoly(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius - 1, 0, HexGrid[i][j].Color);

      // Choisir la couleur de contour en fonction de la sélection et du clignotement
      if HexGrid[i][j].Selected and (BlinkCounter mod 60 < 30) then
        outlineColor := ORANGE
      else
        outlineColor := DARKGRAY;

      DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 2, outlineColor);

      // Affiche le numéro de l'hexagone au centre
      StrPCopy(hexNumberText, IntToStr(HexGrid[i][j].Number));
      DrawText(hexNumberText, Round(HexGrid[i][j].Center.x - 10), Round(HexGrid[i][j].Center.y - 10), 20, BLACK);
    end;
  end;
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Flat Top with Neighbors and Info');
  SetTargetFPS(60);

  InitializeHexGrid();
  BlinkCounter := 0;

  while not WindowShouldClose() do
  begin
    BlinkCounter := BlinkCounter + 1;

    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawHexGrid();
    EndDrawing();
  end;

  CloseWindow();
end.






