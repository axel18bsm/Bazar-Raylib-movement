
program astarhexgrid;

uses
  raylib, math, sysutils,raymath;

const
  HexDiameter = 60;
  HexRadius = HexDiameter / 2;
  HexWidth = HexRadius * sqrt(3);
  HexHeight = HexDiameter;
  Columns = 6;
  Rows = 5;
  TotalHexes = 30;
  WindowWidth = 800;
  WindowHeight = 600;

type
  TEmplacement = (Classic, Bloque);

  THexagon = record
    Number: Integer;
    Center: TVector2;
    Color: TColor;
    GCost, HCost, FCost: Integer;
    Parent: Integer;
    Neighbors: array[0..5] of Integer;
    Poshexagone: TEmplacement;
    Closed: Boolean;
    Open: Boolean;
    Selected: Boolean;
  end;

var
  HexGrid: array[1..TotalHexes] of THexagon;
  StartHex, EndHex: Integer;
  PathFound: Boolean;
  Path: array of Integer;

// Heuristique de distance (hex distance)
function Heuristic(a, b: TVector2): Integer;
begin
  Result := Round(abs(a.x - b.x) + abs(a.y - b.y)) div 2;
end;

// Fonction pour calculer les voisins
procedure CalculateNeighbors();
var
  i: Integer;
begin
  for i := 1 to TotalHexes do
  begin
    // Initialisation par défaut à zéro (pas de voisin)
    FillChar(HexGrid[i].Neighbors, SizeOf(HexGrid[i].Neighbors), 0);
      HexGrid[i].Number:=i;
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

// Affiche la grille avec le chemin
procedure DrawHexGrid();
var
  i, k: Integer;
  hexPoints: array[0..5] of TVector2;
  angle_deg, angle_rad: Single;
begin
  for i := 1 to TotalHexes do
  begin
    // Calculer les sommets de l'hexagone (tête pointue)
    for k := 0 to 5 do
    begin
      angle_deg := 60 * k + 30; // Départ à 30 degrés pour tête pointue
      angle_rad := PI / 180 * angle_deg;
      hexPoints[k].x := HexGrid[i].Center.x + HexRadius * cos(angle_rad);
      hexPoints[k].y := HexGrid[i].Center.y + HexRadius * sin(angle_rad);
    end;

    // Dessiner l'hexagone avec sa couleur
    DrawPoly(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y), 6, HexRadius - 1, 0, HexGrid[i].Color);

    // Dessiner les contours de l'hexagone
    for k := 0 to 5 do
    begin
      DrawLineV(hexPoints[k], hexPoints[(k + 1) mod 6], DARKGRAY);
    end;

    // Si sélectionné, ajouter un surlignage
    if HexGrid[i].Selected then
    begin
      for k := 0 to 5 do
      begin
        DrawLineV(hexPoints[k], hexPoints[(k + 1) mod 6], ORANGE);
      end;
    end;
  end;
end;

// Initialisation de la grille
procedure InitializeHexGrid();
var
  i, j: Integer;
  offsetX, offsetY: Single;
begin
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      offsetX := i * (HexRadius * 1.5);  // Décalage horizontal
      offsetY := j * (HexRadius * sqrt(3));  // Décalage vertical

      // Décalage pour les colonnes impaires
      if (i mod 2) <> 0 then
        offsetY := offsetY + (HexRadius * sqrt(3) / 2);

      HexGrid[i + j * Columns + 1].Center := Vector2Create(offsetX + HexRadius, offsetY + HexRadius);
      HexGrid[i + j * Columns + 1].Color := GREEN;  // Définir la couleur par défaut
      HexGrid[i + j * Columns + 1].Selected := False;
    end;
  end;
  CalculateNeighbors();
end;

// Algorithme A* : Trouver le chemin
procedure AStarPathfinding(startHex, endHex: Integer);
var
  openList: array of Integer;
  closedList: array of Integer;
  current, neighbor, i, j: Integer;
  tempGCost, tempFCost: Integer;
  PathFound: Boolean;

  procedure AddToOpenList(hex: Integer);
  begin
    SetLength(openList, Length(openList) + 1);
    openList[High(openList)] := hex;
  end;

  procedure RemoveFromOpenList(hex: Integer);
  var
    index, lastIndex: Integer;
  begin
    lastIndex := High(openList);
    for index := 0 to lastIndex do
    begin
      if openList[index] = hex then
      begin
        openList[index] := openList[lastIndex];
        SetLength(openList, Length(openList) - 1);
        Break;
      end;
    end;
  end;

  procedure AddToClosedList(hex: Integer);
  begin
    SetLength(closedList, Length(closedList) + 1);
    closedList[High(closedList)] := hex;
  end;

  function IsInClosedList(hex: Integer): Boolean;
  var
    k: Integer;
  begin
    Result := False;
    for k := 0 to High(closedList) do
    begin
      if closedList[k] = hex then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  SetLength(openList, 0);
  SetLength(closedList, 0);
  PathFound := False;

  // Initialisation du point de départ
  HexGrid[startHex].GCost := 0;
  HexGrid[startHex].HCost := Heuristic(HexGrid[startHex].Center, HexGrid[endHex].Center);
  HexGrid[startHex].FCost := HexGrid[startHex].GCost + HexGrid[startHex].HCost;
  HexGrid[startHex].Open := True;

  AddToOpenList(startHex);

  // Boucle principale de l'algorithme A*
  while Length(openList) > 0 do
  begin
    // Trouver le noeud avec le plus petit F cost dans openList
    current := openList[0];
    for i := 1 to High(openList) do
    begin
      if HexGrid[openList[i]].FCost < HexGrid[current].FCost then
        current := openList[i];
    end;

    // Si on atteint la destination
    if current = endHex then
    begin
      PathFound := True;
      Break;
    end;

    // Retirer le noeud courant de la openList et l'ajouter à la closedList
    RemoveFromOpenList(current);
    AddToClosedList(current);
    HexGrid[current].Closed := True;

    // Examiner les voisins du noeud courant
    for j := 0 to 5 do
    begin
      neighbor := HexGrid[current].Neighbors[j];
      if (neighbor = 0) or (HexGrid[neighbor].Poshexagone = Bloque) or IsInClosedList(neighbor) then
        Continue;

      tempGCost := HexGrid[current].GCost + 1; // Chaque déplacement coûte 1

      if not HexGrid[neighbor].Open then
      begin
        HexGrid[neighbor].GCost := tempGCost;
        HexGrid[neighbor].HCost := Heuristic(HexGrid[neighbor].Center, HexGrid[endHex].Center);
        HexGrid[neighbor].FCost := HexGrid[neighbor].GCost + HexGrid[neighbor].HCost;
        HexGrid[neighbor].Parent := current;
        HexGrid[neighbor].Open := True;

        AddToOpenList(neighbor);
      end
      else if tempGCost < HexGrid[neighbor].GCost then
      begin
        HexGrid[neighbor].GCost := tempGCost;
        HexGrid[neighbor].FCost := HexGrid[neighbor].GCost + HexGrid[neighbor].HCost;
        HexGrid[neighbor].Parent := current;
      end;
    end;
  end;

  // Si un chemin a été trouvé
  if PathFound then
  begin
    SetLength(Path, 0);
    current := endHex;

    // Backtracking pour reconstruire le chemin
    while current <> startHex do
    begin
      SetLength(Path, Length(Path) + 1);
      Path[High(Path)] := current;
      current := HexGrid[current].Parent;
    end;

    SetLength(Path, Length(Path) + 1);
    Path[High(Path)] := startHex;
  end;
end;

// Dessiner le chemin trouvé
procedure DrawPath();
var
  i: Integer;
begin
  if PathFound then
  begin
    for i := 0 to High(Path) do
    begin
      HexGrid[Path[i]].Color := red;
      HexGrid[Path[i]].Selected := True;
    end;
  end;
end;

// Initialisation de la fenêtre et boucle principale
begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - A* Pathfinding');
  SetTargetFPS(60);

  InitializeHexGrid();
  StartHex := 3;
  EndHex := 30;

  // Lancer l'algorithme A* et trouver le chemin
  AStarPathfinding(StartHex, EndHex);

  // Boucle principale
  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawHexGrid();     // Dessine la grille hexagonale
    DrawPath();        // Dessine le chemin trouvé en jaune

    EndDrawing();
  end;

  CloseWindow();
end.

