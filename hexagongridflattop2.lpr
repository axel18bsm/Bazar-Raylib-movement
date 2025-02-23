program HexagonGridFlatTop;

uses raylib, math, sysutils;

const
  HexDiameter = 60;         // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexWidth = HexDiameter;    // Largeur de l'hexagone (flat-top)
  HexHeight = HexRadius * sqrt(3);  // Hauteur totale d'un hexagone (distance entre deux sommets opposés)
  Columns = 6;               // Nombre d'hexagones en largeur
  Rows = 5;                  // Nombre d'hexagones en hauteur
  WindowWidth = Round(Columns * (HexWidth * 0.75) + HexWidth * 0.25);
  WindowHeight = Round(Rows * HexHeight + HexHeight / 2);

type
  TPoint = record
    x, y: Integer;
  end;

  THexCell = record
    Center: TPoint;         // Centre de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;          // Couleur de l'hexagone
    Selected: Boolean;      // Hexagone sélectionné ou non
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j: Integer;

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

// Initialise la grille hexagonale "tête plate"
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  for i := 0 to Columns - 1 do
  begin
    offsetX := i * (HexWidth * 0.75);  // Décalage horizontal avec 3/4 de la largeur d'un hexagone

    for j := 0 to Rows - 1 do
    begin
      offsetY := j * HexHeight;
      if (i mod 2) = 1 then
        offsetY := offsetY + (HexHeight / 2);  // Décalage pour créer la disposition en "nid d'abeille"

      HexGrid[i][j].Center.x := Round(HexRadius + offsetX);
      HexGrid[i][j].Center.y := Round(HexRadius + offsetY);
      HexGrid[i][j].Color := LIGHTGRAY;
      HexGrid[i][j].Selected := False;
      CalculateHexVertices(HexGrid[i][j]); // Calcule les sommets de l'hexagone
    end;
  end;
end;

// Dessine la grille hexagonale
procedure DrawHexGrid();
var
  k: Integer;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      if HexGrid[i][j].Selected then
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 3, ORANGE)
      else
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 1, BLACK);

      for k := 0 to 5 do
        DrawLine(HexGrid[i][j].Vertices[k].x, HexGrid[i][j].Vertices[k].y,
                 HexGrid[i][j].Vertices[(k + 1) mod 6].x, HexGrid[i][j].Vertices[(k + 1) mod 6].y, HexGrid[i][j].Color);
    end;
  end;
end;

// Gère la détection de clic sur un hexagone
procedure HandleMouseClick();
var
  mouseX, mouseY: Integer;
  dx, dy: Single;
  dist: Single;
begin
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    mouseX := GetMouseX();
    mouseY := GetMouseY();
    for i := 0 to Columns - 1 do
    begin
      for j := 0 to Rows - 1 do
      begin
        dx := mouseX - HexGrid[i][j].Center.x;
        dy := mouseY - HexGrid[i][j].Center.y;
        dist := sqrt(dx * dx + dy * dy);
        if dist <= HexRadius then
        begin
          HexGrid[i][j].Selected := not HexGrid[i][j].Selected; // Bascule l'état de sélection
        end;
      end;
    end;
  end;
end;

begin
  // Initialisation de la fenêtre
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Flat Top');
  SetTargetFPS(60);

  // Initialisation de la grille d'hexagones
  InitializeHexGrid();

  // Boucle principale
  while not WindowShouldClose() do
  begin
    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessiner la grille d'hexagones
    DrawHexGrid();

    EndDrawing();
  end;

  // Fermeture de la fenêtre
  CloseWindow();
end.


