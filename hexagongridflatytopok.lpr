program hexagongridflatytopok;

uses raylib, math, sysutils;

const
  HexDiameter = 60;            // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexHeight = HexRadius * sqrt(3);  // Hauteur totale d'un hexagone (distance entre deux sommets opposés)
  HexWidth = HexDiameter;       // Largeur de l'hexagone (pour les têtes pointues)
  Columns = 6;                  // Nombre d'hexagones en largeur
  Rows = 5;                     // Nombre d'hexagones en hauteur
  InfoBoxHeight = 150;          // Hauteur de la zone d'information
  WindowWidth = 800;            // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight;  // Hauteur de la fenêtre (inclut la zone d'information)

type
  THexagon = record
    Number: Integer;                // Numéro de l'hexagone
    Row, Col: Integer;              // Ligne et colonne de l'hexagone
    Center: TVector2;               // Centre de l'hexagone
    Vertices: array[0..5] of TVector2;  // Sommets de l'hexagone
    Color: TColor;                  // Couleur de l'hexagone
    Selected: Boolean;              // État de sélection
  end;

var
  HexGrid: array[0..Columns * Rows - 1] of THexagon;  // Grille d'hexagones
  SelectedHex: THexagon;  // Hexagone sélectionné
  HexSelected: Boolean;   // Indique si un hexagone est sélectionné

// Calcule les 6 sommets d'un hexagone "tête pointue"
procedure CalculateHexVertices(var Hex: THexagon);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 60 * k + 30;  // Rotation pour un hexagone tête pointue (décalé de 30°)
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Hex.Center.x + HexRadius * cos(angle_rad);
    Hex.Vertices[k].y := Hex.Center.y + HexRadius * sin(angle_rad);
  end;
end;

// Initialise la grille hexagonale avec les hexagones têtes pointues
procedure InitializeHexGrid();
var
  i, j, HexIndex: Integer;
  offsetX, offsetY: Single;
begin
  HexIndex := 0;
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      // Calcule la position centrale de chaque hexagone
      offsetX := i * (HexWidth * 0.75);  // Décalage horizontal pour aligner les hexagones
      offsetY := j * (HexHeight);        // Décalage vertical constant

      // Décalage supplémentaire pour les colonnes impaires
      if (i mod 2) <> 0 then
        offsetY := offsetY + HexHeight / 2;

      HexGrid[HexIndex].Number := HexIndex + 1;
      HexGrid[HexIndex].Row := j + 1;
      HexGrid[HexIndex].Col := i + 1;
      HexGrid[HexIndex].Center := Vector2Create(offsetX + HexWidth / 2, offsetY + HexHeight / 2);
      HexGrid[HexIndex].Color := GREEN;  // Par défaut, tout est vert
      HexGrid[HexIndex].Selected := False;

      CalculateHexVertices(HexGrid[HexIndex]);
      Inc(HexIndex);
    end;
  end;
end;

// Dessine la grille hexagonale
procedure DrawHexGrid();
var
  i, k: Integer;
  hexPoints: array[0..5] of TVector2;
  hexNumberText: array[0..5] of Char;
begin
  for i := 0 to Columns * Rows - 1 do
  begin
    // Prépare les sommets à dessiner
    for k := 0 to 5 do
      hexPoints[k] := HexGrid[i].Vertices[k];

    // Dessine l'hexagone
    DrawPoly(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y), 6, HexRadius - 1, 0, HexGrid[i].Color);
    DrawPolyLinesEx(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y), 6, HexRadius, 0, 2, DARKGRAY);

    if HexGrid[i].Selected then
      DrawPolyLinesEx(Vector2Create(HexGrid[i].Center.x, HexGrid[i].Center.y), 6, HexRadius, 0, 3, ORANGE);

    // Affiche le numéro de l'hexagone
    StrPCopy(hexNumberText, IntToStr(HexGrid[i].Number));
    DrawText(hexNumberText, Round(HexGrid[i].Center.x - 10), Round(HexGrid[i].Center.y - 10), 20, BLACK);
  end;
end;

// Gère la détection de clic sur un hexagone
procedure HandleMouseClick();
var
  mouseX, mouseY, i: Integer;
  dist, dx, dy: Single;
begin
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    mouseX := GetMouseX();
    mouseY := GetMouseY();

    for i := 0 to Columns * Rows - 1 do
    begin
      dx := mouseX - HexGrid[i].Center.x;
      dy := mouseY - HexGrid[i].Center.y;
      dist := sqrt(dx * dx + dy * dy);

      if dist <= HexRadius then
      begin
        HexGrid[i].Selected := True;
        SelectedHex := HexGrid[i];
        HexSelected := True;
      end
      else
        HexGrid[i].Selected := False;
    end;
  end;
end;

// Affiche les informations de l'hexagone sélectionné
procedure DrawHexInfoBox();
var
  InfoText: String;
begin
  DrawRectangle(0, WindowHeight - InfoBoxHeight, WindowWidth, InfoBoxHeight, LIGHTGRAY); // Cadre de fond
  DrawRectangleLines(0, WindowHeight - InfoBoxHeight, WindowWidth, InfoBoxHeight, DARKGRAY); // Contour

  if HexSelected then
  begin
    InfoText := Format('Numéro de l''hexagone: %d'#10 +
                       'Ligne: %d  Colonne: %d'#10 +
                       'Couleur: Vert', [SelectedHex.Number, SelectedHex.Row, SelectedHex.Col]);
    DrawText(PChar(InfoText), 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
  end
  else
    DrawText('Aucun hexagone sélectionné.', 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Pointy Top');
  SetTargetFPS(60);
  InitializeHexGrid();

  while not WindowShouldClose() do
  begin
    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawHexGrid();
    DrawHexInfoBox(); // Dessine la boîte d'information
    EndDrawing();
  end;

  CloseWindow();
end.





















