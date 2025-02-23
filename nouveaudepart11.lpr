program HexagonGridTetepointue;

uses raylib, math, sysutils;

const
  HexDiameter = 60;  // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;
  HexWidth = HexDiameter;  // Largeur d'un hexagone (tête plate)
  HexHeight = HexRadius * sqrt(3);  // Hauteur totale d'un hexagone
  Columns = 6;  // Nombre d'hexagones en largeur
  Rows = 5;     // Nombre d'hexagones en hauteur
  WindowWidth = Round(Columns * HexWidth * 0.75 + HexRadius);
  WindowHeight = Round(Rows * HexHeight + HexRadius);

type
  TPoint = record
    x, y: Integer;
  end;

  THexCell = record
    Center: TPoint;    // Centre de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;     // Couleur de l'hexagone
    Selected: Boolean; // Hexagone sélectionné ou non
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j: Integer;
  ScreenCenterX, ScreenCenterY: Integer;

procedure CalculateHexVertices(var Hex: THexCell);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 60 * k;
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
    Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
  end;
end;

procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  offsetX := 0;
  for i := 0 to Columns - 1 do
  begin
    if Odd(i) then
      offsetY := HexHeight / 2
    else
      offsetY := 0;

    for j := 0 to Rows - 1 do
    begin
      HexGrid[i][j].Center.x := Round(i * HexWidth * 0.75 + HexRadius);
      HexGrid[i][j].Center.y := Round(j * HexHeight + HexRadius + offsetY);
      HexGrid[i][j].Color := LIGHTGRAY;
      HexGrid[i][j].Selected := False;
      CalculateHexVertices(HexGrid[i][j]);
    end;
  end;
end;

procedure DrawHexGrid();
var
  k: Integer;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      if HexGrid[i][j].Selected then
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 30, 3, ORANGE)
      else
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 30, 1, BLACK);

      for k := 0 to 5 do
        DrawLine(HexGrid[i][j].Vertices[k].x, HexGrid[i][j].Vertices[k].y,
                 HexGrid[i][j].Vertices[(k + 1) mod 6].x, HexGrid[i][j].Vertices[(k + 1) mod 6].y, HexGrid[i][j].Color);
    end;
  end;
end;

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
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid');
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
