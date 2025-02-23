
program hexagonalgridastar2;

uses raylib, math, sysutils;

const
  HexRadius = 30; // Rayon de chaque hexagone
  Columns = 6;    // Nombre d'hexagones en largeur
  Rows = 5;       // Nombre d'hexagones en hauteur
  TotalHexes = Columns * Rows;
  WindowWidth = 800;
  WindowHeight = 600;

type
  TPoint = record
    x, y: Single;
  end;

  THexCell = record
    Number: Integer;
    Center: TPoint;
    Neighbors: array[0..5] of Integer;
    Color: TColor;
    Selected: Boolean;
    Parent: Integer;    // Utilisé pour le backtracking du chemin
    GCost, HCost, FCost: Integer;  // Coûts pour A*
    Open, Closed: Boolean;
  end;

var
  HexGrid: array[1..TotalHexes] of THexCell;
  startHex, endHex: Integer;
  Path: array of Integer;
  PathFound: Boolean = False;

// Calcule les voisins pour chaque hexagone
procedure CalculateNeighbors();
var
  i: Integer;
begin
  for i := 1 to TotalHexes do
  begin
    // Initialisation par défaut à zéro (pas de voisin)
    FillChar(HexGrid[i].Neighbors, SizeOf(HexGrid[i].Neighbors), 0);

    // Définir les voisins selon la position dans la grille
    case i of
      // Ligne 1
      1: begin HexGrid[i].Neighbors[1] := 2; HexGrid[i].Neighbors[2] := 7; end;
      2: begin HexGrid[i].Neighbors[0] := 1; HexGrid[i].Neighbors[1] := 3; HexGrid[i].Neighbors[2] := 8; HexGrid[i].Neighbors[5] := 7; end;
      3: begin HexGrid[i].Neighbors[0] := 2; HexGrid[i].Neighbors[1] := 4; HexGrid[i].Neighbors[2] := 9; HexGrid[i].Neighbors[5] := 8; end;
      4: begin HexGrid[i].Neighbors[0] := 3; HexGrid[i].Neighbors[1] := 5; HexGrid[i].Neighbors[2] := 10; HexGrid[i].Neighbors[5] := 9; end;
      5: begin HexGrid[i].Neighbors[0] := 4; HexGrid[i].Neighbors[1] := 6; HexGrid[i].Neighbors[2] := 11; HexGrid[i].Neighbors[5] := 10; end;
      6: begin HexGrid[i].Neighbors[0] := 5; HexGrid[i].Neighbors[2] := 12; HexGrid[i].Neighbors[5] := 11; end;

      // Ligne 2
      7: begin HexGrid[i].Neighbors[0] := 1; HexGrid[i].Neighbors[1] := 8; HexGrid[i].Neighbors[2] := 13; end;
      8: begin HexGrid[i].Neighbors[0] := 2; HexGrid[i].Neighbors[1] := 9; HexGrid[i].Neighbors[2] := 14; HexGrid[i].Neighbors[3] := 13; HexGrid[i].Neighbors[4] := 7; end;
      9: begin HexGrid[i].Neighbors[0] := 3; HexGrid[i].Neighbors[1] := 10; HexGrid[i].Neighbors[2] := 15; HexGrid[i].Neighbors[3] := 14; HexGrid[i].Neighbors[4] := 8; end;
      10: begin HexGrid[i].Neighbors[0] := 4; HexGrid[i].Neighbors[1] := 11; HexGrid[i].Neighbors[2] := 16; HexGrid[i].Neighbors[3] := 15; HexGrid[i].Neighbors[4] := 9; end;
      11: begin HexGrid[i].Neighbors[0] := 5; HexGrid[i].Neighbors[1] := 12; HexGrid[i].Neighbors[2] := 17; HexGrid[i].Neighbors[3] := 16; HexGrid[i].Neighbors[4] := 10; end;
      12: begin HexGrid[i].Neighbors[0] := 6; HexGrid[i].Neighbors[2] := 18; HexGrid[i].Neighbors[3] := 17; HexGrid[i].Neighbors[4] := 11; end;

      // Ligne 3
      13: begin HexGrid[i].Neighbors[0] := 7; HexGrid[i].Neighbors[1] := 14; HexGrid[i].Neighbors[2] := 19; end;
      14: begin HexGrid[i].Neighbors[0] := 8; HexGrid[i].Neighbors[1] := 15; HexGrid[i].Neighbors[2] := 20; HexGrid[i].Neighbors[3] := 19; HexGrid[i].Neighbors[4] := 13; end;
      15: begin HexGrid[i].Neighbors[0] := 9; HexGrid[i].Neighbors[1] := 16; HexGrid[i].Neighbors[2] := 21; HexGrid[i].Neighbors[3] := 20; HexGrid[i].Neighbors[4] := 14; end;
      16: begin HexGrid[i].Neighbors[0] := 10; HexGrid[i].Neighbors[1] := 17; HexGrid[i].Neighbors[2] := 22; HexGrid[i].Neighbors[3] := 21; HexGrid[i].Neighbors[4] := 15; end;
      17: begin HexGrid[i].Neighbors[0] := 11; HexGrid[i].Neighbors[1] := 18; HexGrid[i].Neighbors[2] := 23; HexGrid[i].Neighbors[3] := 22; HexGrid[i].Neighbors[4] := 16; end;
      18: begin HexGrid[i].Neighbors[0] := 12; HexGrid[i].Neighbors[2] := 24; HexGrid[i].Neighbors[3] := 23; HexGrid[i].Neighbors[4] := 17; end;

      // Ligne 4
      19: begin HexGrid[i].Neighbors[0] := 13; HexGrid[i].Neighbors[1] := 20; HexGrid[i].Neighbors[2] := 25; end;
      20: begin HexGrid[i].Neighbors[0] := 14; HexGrid[i].Neighbors[1] := 21; HexGrid[i].Neighbors[2] := 26; HexGrid[i].Neighbors[3] := 25; HexGrid[i].Neighbors[4] := 19; end;
      21: begin HexGrid[i].Neighbors[0] := 15; HexGrid[i].Neighbors[1] := 22; HexGrid[i].Neighbors[2] := 27; HexGrid[i].Neighbors[3] := 26; HexGrid[i].Neighbors[4] := 20; end;
      22: begin HexGrid[i].Neighbors[0] := 16; HexGrid[i].Neighbors[1] := 23; HexGrid[i].Neighbors[2] := 28; HexGrid[i].Neighbors[3] := 27; HexGrid[i].Neighbors[4] := 21; end;
      23: begin HexGrid[i].Neighbors[0] := 17; HexGrid[i].Neighbors[1] := 24; HexGrid[i].Neighbors[2] := 29; HexGrid[i].Neighbors[3] := 28; HexGrid[i].Neighbors[4] := 22; end;
      24: begin HexGrid[i].Neighbors[0] := 18; HexGrid[i].Neighbors[2] := 30; HexGrid[i].Neighbors[3] := 29; HexGrid[i].Neighbors[4] := 23; end;

      // Ligne 5
      25: begin HexGrid[i].Neighbors[0] := 19; HexGrid[i].Neighbors[1] := 26; end;
      26: begin HexGrid[i].Neighbors[0] := 20; HexGrid[i].Neighbors[1] := 27; HexGrid[i].Neighbors[4] := 25; end;
      27: begin HexGrid[i].Neighbors[0] := 21; HexGrid[i].Neighbors[1] := 28; HexGrid[i].Neighbors[4] := 26; end;
      28: begin HexGrid[i].Neighbors[0] := 22; HexGrid[i].Neighbors[1] := 29; HexGrid[i].Neighbors[4] := 27; end;
      29: begin HexGrid[i].Neighbors[0] := 23; HexGrid[i].Neighbors[1] := 30; HexGrid[i].Neighbors[4] := 28; end;
      30: begin HexGrid[i].Neighbors[0] := 24; HexGrid[i].Neighbors[4] := 29; end;
    end;
  end;
end;

// Initialiser la grille hexagonale
procedure InitializeHexGrid();
var
  i, j, index: Integer;
  offsetX, offsetY: Single;
begin
  index := 1;
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      offsetX := i * (HexRadius * 1.5);
      offsetY := j * (HexRadius * sqrt(3));

      // Décalage des colonnes impaires
      if (i mod 2) <> 0 then
        offsetY := offsetY + (HexRadius * sqrt(3) / 2);

      HexGrid[index].Number := index;
      HexGrid[index].Center.x := offsetX + HexRadius;
      HexGrid[index].Center.y := offsetY + HexRadius;
      HexGrid[index].Color := LIGHTGRAY; // Couleur par défaut
      HexGrid[index].Selected := False;

      Inc(index);
    end;
  end;

  CalculateNeighbors(); // Calcul des voisins
end;

// Dessiner la grille hexagonale
procedure DrawHexGrid();
var
  i, k: Integer;
  hexPoints: array[0..5] of TPoint;
  angle_deg, angle_rad: Single;
begin
  for i := 1 to TotalHexes do
  begin
    // Calcul des sommets de l'hexagone
    for k := 0 to 5 do
    begin
      angle_deg := 60 * k + 30; // Départ à 30° pour tête pointue
      angle_rad := PI / 180 * angle_deg;
      hexPoints[k].x := HexGrid[i].Center.x + HexRadius * cos(angle_rad);
      hexPoints[k].y := HexGrid[i].Center.y + HexRadius * sin(angle_rad);
    end;

    // Dessiner l'hexagone
    DrawPoly(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y), 6, HexRadius - 1, 0, HexGrid[i].Color);

    // Dessiner les côtés de l'hexagone
    for k := 0 to 5 do
      DrawLineV(Vector2Create(hexPoints[k].x, hexPoints[k].y),
                Vector2Create(hexPoints[(k + 1) mod 6].x, hexPoints[(k + 1) mod 6].y), DARKGRAY);

    // Surlignage si sélectionné
    if HexGrid[i].Selected then
      for k := 0 to 5 do
        DrawLineV(Vector2Create(hexPoints[k].x, hexPoints[k].y),
                  Vector2Create(hexPoints[(k + 1) mod 6].x, hexPoints[(k + 1) mod 6].y), ORANGE);
  end;
end;

// Gérer le clic sur un hexagone pour sélectionner départ ou arrivée
procedure HandleMouseClick();
var
  mouseX, mouseY, i: Integer;
  dist, dx, dy: Single;
begin
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    mouseX := GetMouseX();
    mouseY := GetMouseY();

    for i := 1 to TotalHexes do
    begin
      dx := mouseX - HexGrid[i].Center.x;
      dy := mouseY - HexGrid[i].Center.y;
      dist := sqrt(dx * dx + dy * dy);

      if dist <= HexRadius then
      begin
        if startHex = 0 then
          startHex := i
        else if endHex = 0 then
          endHex := i;
        HexGrid[i].Selected := True;
        Break;
      end;
    end;
  end;
end;

// Affichage des informations et pathfinding
procedure DisplayInfo();
begin
  if startHex <> 0 then
    DrawText(PChar('Start Hex: ' + IntToStr(startHex)), 20, 20, 20, BLACK);
  if endHex <> 0 then
    DrawText(PChar('End Hex: ' + IntToStr(endHex)), 20, 50, 20, BLACK);

  if PathFound then
    DrawText('Path Found!', 20, 80, 20, GREEN);
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - A* Pathfinding');
  SetTargetFPS(60);
  InitializeHexGrid();

  while not WindowShouldClose() do
  begin
    HandleMouseClick(); // Gérer les clics
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawHexGrid();
    DisplayInfo();
    EndDrawing();
  end;

  CloseWindow();
end.
