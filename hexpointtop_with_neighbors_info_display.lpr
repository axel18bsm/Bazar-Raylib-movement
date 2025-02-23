program hexgrid_pointed_top_neighbors_test;

uses raylib, sysutils, Math;

const
  HexDiameter = 60;         // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexWidth = HexDiameter * sqrt(3) / 2;  // Largeur de l'hexagone (pointed-top)
  HexHeight = HexDiameter;               // Hauteur totale d'un hexagone
  Columns = 6;               // Nombre d'hexagones en largeur
  Rows = 5;                  // Nombre d'hexagones en hauteur
  WindowWidth = 800;         // Largeur de la fenêtre
  WindowHeight = 600;        // Hauteur de la fenêtre

type
  TPoint = record
    x, y: Integer;
  end;

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: Integer;           // Numéro de l'hexagone (de 1 à 30)
    Center: TVector2;          // Centre de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;             // Couleur de l'hexagone
    Selected: Boolean;         // État de sélection
    Neighbors: array[0..5] of Integer;  // Numéros des voisins contigus (6 voisins)
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j, hexNumber: Integer;

// Calcule les 6 sommets d'un hexagone "tête pointue"
procedure CalculateHexVertices(var Hex: THexCell);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 60 * k - 30;  // Angle de rotation pour un hexagone "tête pointue"
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
    Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
  end;
end;

// Calcule les voisins d'un hexagone en fonction de sa position
procedure CalculateNeighbors();
var
  k: Integer;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      for k := 0 to 5 do
        HexGrid[i][j].Neighbors[k] := 0;  // Initialisation des voisins à 0

      // Voisin Haut-Droit
      if (j > 0) and ((i < Columns - 1) or (j mod 2 = 0)) then
        HexGrid[i][j].Neighbors[0] := HexGrid[i][j - 1].Number;

      // Voisin Droit
      if i < Columns - 1 then
        HexGrid[i][j].Neighbors[1] := HexGrid[i + 1][j].Number;

      // Voisin Bas-Droit
      if (j < Rows - 1) and ((i < Columns - 1) or (j mod 2 = 1)) then
        HexGrid[i][j].Neighbors[2] := HexGrid[i][j + 1].Number;

      // Voisin Bas-Gauche
      if (j < Rows - 1) and ((i > 0) or (j mod 2 = 1)) then
        HexGrid[i][j].Neighbors[3] := HexGrid[i - 1][j + 1].Number;

      // Voisin Gauche
      if i > 0 then
        HexGrid[i][j].Neighbors[4] := HexGrid[i - 1][j].Number;

      // Voisin Haut-Gauche
      if (j > 0) and ((i > 0) or (j mod 2 = 0)) then
        HexGrid[i][j].Neighbors[5] := HexGrid[i - 1][j - 1].Number;
    end;
  end;
end;

// Initialise la grille hexagonale avec les numéros et centre
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  hexNumber := 1;  // Initialisation du compteur d'hexagones
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      offsetX := i * HexWidth;
      offsetY := j * (HexHeight * 3 / 4); // Décalage pour hexagones tête pointue

      if (i mod 2) = 1 then
        offsetY := offsetY + (HexHeight / 2);  // Décalage pour nid d'abeilles

      // Définition du centre de l'hexagone
      HexGrid[i][j].Center := Vector2Create(Round(HexRadius + offsetX), Round(HexRadius + offsetY));

      // Définit le numéro de l'hexagone
      HexGrid[i][j].Number := hexNumber;
      Inc(hexNumber);  // Incrémentation du numéro

      CalculateHexVertices(HexGrid[i][j]); // Calcule les sommets de l'hexagone
    end;
  end;

  CalculateNeighbors(); // Calcule les voisins de chaque hexagone
end;

// Vérifie et affiche les voisins de l'hexagone spécifié
procedure TestNeighbors(HexNumber: Integer);
var
  i, j, k: Integer;
  found: Boolean = False;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      if HexGrid[i][j].Number = HexNumber then
      begin
        WriteLn('Hexagone ', HexNumber, ' :');
        WriteLn('Voisins (dans l''ordre Haut-Droit, Droit, Bas-Droit, Bas-Gauche, Gauche, Haut-Gauche) : ');
        for k := 0 to 5 do
          Write(HexGrid[i][j].Neighbors[k], ' ');
        WriteLn;
        found := True;
        Break;
      end;
    end;
    if found then
      Break;
  end;
end;

begin
  // Initialisation
  InitializeHexGrid();

  // Tests de vérification des voisins pour plusieurs hexagones
  TestNeighbors(1);  // Doit être 0, 2, 7, 0, 0, 0
  TestNeighbors(7);  // Doit être 2, 8, 13, 12, 6, 1
  TestNeighbors(30); // Doit être 24, 0, 0, 0, 29, 23
end.



















