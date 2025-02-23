program hexpointtop_with_neighbors_info_display_modele;

uses raylib, math, sysutils;

const
  HexDiameter = 60;              // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;    // Rayon de chaque hexagone
  HexHeight = HexDiameter;        // Hauteur de l'hexagone (tête pointue)
  HexWidth = HexRadius * sqrt(3); // Largeur totale d'un hexagone (distance entre deux sommets opposés)
  Columns = 6;                    // Nombre d'hexagones en largeur
  Rows = 6;                       // Nombre d'hexagones en hauteur
  InfoBoxWidth = 300;             // Largeur du cadre d'informations
  InfoBoxHeight = 150;            // Hauteur de la zone d'informations
  WindowWidth = 800;              // Largeur de la fenêtre
  WindowHeight = 600 + InfoBoxHeight; // Hauteur de la fenêtre (inclut la zone d'information)

type
  TPoint = record
    x, y: Integer;
  end;

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: Integer;              // Numéro de l'hexagone (de 1 à 30)
    Center: TPoint;               // Point central de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;                // Couleur de l'hexagone
    Selected: Boolean;            // État de sélection
    Neighbors: array[1..6] of Integer;  // Numéros des voisins contigus (6 voisins)
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j, hexNumber: Integer;
  SelectedHex: THexCell;  // Hexagone actuellement sélectionné
  HexSelected: Boolean;   // Indique si un hexagone est sélectionné

// Calcule les 6 sommets d'un hexagone "tête pointue"
procedure CalculateHexVertices(var Hex: THexCell);
var
  angle_deg, angle_rad: Single;
  k: Integer;
begin
  for k := 0 to 5 do
  begin
    angle_deg := 30 + 60 * k;  // Angle de rotation pour un hexagone "tête pointue"
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
    Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
  end;
end;


// Initialise la grille hexagonale avec des numéros, couleurs et centre
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  hexNumber := 1;  // Initialisation du compteur d'hexagones
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      offsetX := i * HexWidth;  // Décalage horizontal
      offsetY := j * (HexHeight * 0.75);  // Décalage vertical avec 3/4 de la hauteur d'un hexagone

      if (j mod 2) = 1 then
        offsetX := offsetX + (HexWidth / 2);  // Décalage pour créer la disposition en "nid d'abeille"

      // Définition du centre de l'hexagone
      HexGrid[i][j].Center.x := Round(HexRadius + offsetX);
      HexGrid[i][j].Center.y := Round(HexHeight / 2 + offsetY);

      // Définit le numéro de l'hexagone
      HexGrid[i][j].Number := hexNumber;
      Inc(hexNumber);  // Incrémentation du numéro

      if (i + j) mod 2 = 0 then
        HexGrid[i][j].Color := GREEN
      else
        HexGrid[i][j].Color := LIGHTGRAY;

      HexGrid[i][j].Selected := False;
      CalculateHexVertices(HexGrid[i][j]); // Calcule les sommets de l'hexagone
    end;
  end;

 // CalculateNeighbors(); // Calcule les voisins contigus de chaque hexagone
end;

// Dessine la grille hexagonale avec les numéros d'hexagones au centre
procedure DrawHexGrid();
var

  hexNumberText: array[0..5] of Char;
  outlineColor: TColor;
begin
  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      DrawPoly(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius - 1, 30, HexGrid[i][j].Color);

      if HexGrid[i][j].Selected then
        outlineColor := ORANGE
      else
        outlineColor := DARKGRAY;

      DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 30, 2, outlineColor);

      StrPCopy(hexNumberText, IntToStr(HexGrid[i][j].Number));
      DrawText(hexNumberText, Round(HexGrid[i][j].Center.x - 10), Round(HexGrid[i][j].Center.y - 10), 20, BLACK);
    end;
  end;
end;

// Gère la détection de clic sur un hexagone et met à jour les informations
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
    HexSelected := False;

    for i := 0 to Columns - 1 do
    begin
      for j := 0 to Rows - 1 do
      begin
        dx := mouseX - HexGrid[i][j].Center.x;
        dy := mouseY - HexGrid[i][j].Center.y;
        dist := sqrt(dx * dx + dy * dy);
        if dist <= HexRadius then
        begin
          HexGrid[i][j].Selected := True;
          SelectedHex := HexGrid[i][j];
          HexSelected := True;
        end
        else
          HexGrid[i][j].Selected := False;
      end;
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
                       'Point Central: (%d, %d)'#10 +
                       'Couleur: %s'#10 +
                       'Voisins: %d, %d, %d, %d, %d, %d',
                       [SelectedHex.Number, SelectedHex.Center.x, SelectedHex.Center.y,
                        'Vert',
                        SelectedHex.Neighbors[1], SelectedHex.Neighbors[2], SelectedHex.Neighbors[3],
                        SelectedHex.Neighbors[4], SelectedHex.Neighbors[5], SelectedHex.Neighbors[6]]);
    DrawText(PChar(InfoText), 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
  end
  else
    DrawText('Aucun hexagone sélectionné.', 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Pointed Top with Neighbors');
  SetTargetFPS(60);
  InitializeHexGrid();
  //CalculateNeighbors()

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


















