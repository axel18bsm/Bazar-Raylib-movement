program HexGridFullWithNeighbors;

uses
  raylib, math, sysutils;

const
  HexDiameter = 60;         // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexHeight = HexDiameter;      // Hauteur d'un hexagone (pointu sur le haut)
  HexWidth = HexRadius * sqrt(3);  // Largeur totale d'un hexagone (distance entre deux côtés opposés)
  Columns = 6;               // Nombre d'hexagones en largeur
  Rows = 5;                  // Nombre d'hexagones en hauteur
  InfoBoxHeight = 150;       // Hauteur de la zone d'information
  WindowWidth = 800;         // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre, inclut la zone d'information

type
  THexagon = record
    Number: Integer;
    Line: Integer;
    Column: Integer;
    Center: TVector2;
    Vertices: array[0..5] of TVector2;
    Color: TColor;
    Neighbors: array[0..5] of Integer; // 6 voisins
  end;

var
  HexGrid: array of THexagon;
  SelectedHex: THexagon;
  HexSelected: Boolean;
  TotalHexagons: Integer = Columns * Rows;
  InfoBox: String;

// Retourne vrai si la ligne est paire
function IsEvenLine(Line: Integer): Boolean;
begin
  Result := (Line mod 2 = 0);
end;

// Retourne le numéro de ligne en fonction du numéro de l'hexagone
function GetLineNumber(HexNumber: Integer): Integer;
begin
  Result := (HexNumber - 1) div Columns + 1;
end;

// Retourne le numéro de colonne en fonction du numéro de l'hexagone
function GetColumnNumber(HexNumber: Integer): Integer;
begin
  Result := (HexNumber - 1) mod Columns + 1;
end;

// Calcule les voisins pour chaque hexagone
procedure CalculateNeighbors();
var
  i: Integer;
begin
  for i := 0 to TotalHexagons - 1 do
  begin
    HexGrid[i].Neighbors[0] := ifthen((i mod Columns <> Columns - 1) and (i < TotalHexagons - Columns), i + 1, 0);     // Haut-droit
    HexGrid[i].Neighbors[1] := ifthen((i < TotalHexagons - Columns), i + Columns, 0);                                  // Droite
    HexGrid[i].Neighbors[2] := ifthen((i mod Columns <> 0) and (i < TotalHexagons - Columns), i + Columns - 1, 0);     // Bas-droit
    HexGrid[i].Neighbors[3] := ifthen((i mod Columns <> 0) and (i >= Columns), i - 1, 0);                              // Bas-gauche
    HexGrid[i].Neighbors[4] := ifthen((i >= Columns), i - Columns, 0);                                                 // Gauche
    HexGrid[i].Neighbors[5] := ifthen((i mod Columns <> Columns - 1) and (i >= Columns), i - Columns + 1, 0);          // Haut-gauche
  end;
end;

// Calcule les sommets de l'hexagone
procedure CalculateHexagonVertices(var Hex: THexagon);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 60 * k - 30;  // Tête pointue (orientée vers le haut)
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Hex.Center.x + HexRadius * cos(angle_rad);
    Hex.Vertices[k].y := Hex.Center.y + HexRadius * sin(angle_rad);
  end;
end;

// Initialise la grille d'hexagones
procedure InitializeHexGrid();
var
  i, line, column: Integer;
  offsetX, offsetY: Single;
begin
  SetLength(HexGrid, TotalHexagons);

  for i := 0 to TotalHexagons - 1 do
  begin
    line := GetLineNumber(i + 1);
    column := GetColumnNumber(i + 1);
    offsetX := (column - 1) * HexWidth;
    offsetY := (line - 1) * (HexHeight * 0.75);

    if not IsEvenLine(line) then
      offsetX := offsetX + (HexWidth / 2);  // Décalage pour les lignes impaires

    // Initialisation des paramètres de l'hexagone
    HexGrid[i].Number := i + 1;
    HexGrid[i].Line := line;
    HexGrid[i].Column := column;
    HexGrid[i].Center := Vector2Create(offsetX + HexWidth / 2, offsetY + HexHeight / 2);
    HexGrid[i].Color := LIGHTGRAY;

    CalculateHexagonVertices(HexGrid[i]);  // Calcul des sommets
  end;

  CalculateNeighbors();  // Calcul des voisins
end;

// Dessine la grille d'hexagones
procedure DrawHexGrid();
var
  i, k: Integer;
begin
  for i := 0 to TotalHexagons - 1 do
  begin
    for k := 0 to 5 do
      DrawLineV(HexGrid[i].Vertices[k], HexGrid[i].Vertices[(k + 1) mod 6], DARKGRAY);

    DrawText(PChar(IntToStr(HexGrid[i].Number)), Round(HexGrid[i].Center.x - 10), Round(HexGrid[i].Center.y - 10), 10, BLACK);
  end;
end;

// Sélectionne un hexagone en fonction du clic de la souris
procedure HandleMouseClick();
var
  mouseX, mouseY, dx, dy, dist: Single;
  i: Integer;
begin
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    mouseX := GetMouseX();
    mouseY := GetMouseY();

    for i := 0 to TotalHexagons - 1 do
    begin
      dx := mouseX - HexGrid[i].Center.x;
      dy := mouseY - HexGrid[i].Center.y;
      dist := sqrt(dx * dx + dy * dy);

      if dist <= HexRadius then
      begin
        HexSelected := True;
        SelectedHex := HexGrid[i];
        InfoBox := Format('Hexagone: %d\nLigne: %d, Colonne: %d\nVoisins: %d, %d, %d, %d, %d, %d',
          [SelectedHex.Number, SelectedHex.Line, SelectedHex.Column,
           SelectedHex.Neighbors[0], SelectedHex.Neighbors[1], SelectedHex.Neighbors[2],
           SelectedHex.Neighbors[3], SelectedHex.Neighbors[4], SelectedHex.Neighbors[5]]);
        Exit;
      end;
    end;
  end;
end;

// Affiche la boîte d'information sous la grille
procedure DrawHexInfoBox();
begin
  DrawRectangle(0, WindowHeight - InfoBoxHeight, WindowWidth, InfoBoxHeight, LIGHTGRAY);
  DrawRectangleLines(0, WindowHeight - InfoBoxHeight, WindowWidth, InfoBoxHeight, DARKGRAY);

  if HexSelected then
    DrawText(PChar(InfoBox), 10, WindowHeight - InfoBoxHeight + 10, 20, BLACK)
  else
    DrawText('Cliquez sur un hexagone pour voir ses informations.', 10, WindowHeight - InfoBoxHeight + 10, 20, BLACK);
end;

// Programme principal
begin
  InitWindow(WindowWidth, WindowHeight, 'Grille d''hexagones avec voisins');
  SetTargetFPS(60);

  InitializeHexGrid();

  while not WindowShouldClose() do
  begin
    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawHexGrid();
    DrawHexInfoBox();

    EndDrawing();
  end;

  CloseWindow();
end.



















