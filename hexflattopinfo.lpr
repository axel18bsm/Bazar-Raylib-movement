program hexflattopinfo;

uses raylib, math, sysutils, DateUtils;

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
  BlinkStartTime: TDateTime; // Moment du début du clignotement

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
      for k := 0 to 5 do
        HexGrid[i][j].Neighbors[k] := 0;  // Initialisation des voisins à 0

      // Calcul des voisins en fonction de la position et de la parité de la colonne
      if j > 0 then
        HexGrid[i][j].Neighbors[0] := HexGrid[i][j - 1].Number; // Voisin du haut

      // Voisin haut-droite
      if (i < Columns - 1) then
      begin
        if (i mod 2 = 0) and (j > 0) then
          HexGrid[i][j].Neighbors[1] := HexGrid[i + 1][j - 1].Number  // Voisin haut-droite pour colonne paire
        else if (i mod 2 = 1) then
          HexGrid[i][j].Neighbors[1] := HexGrid[i + 1][j].Number;  // Voisin haut-droite pour colonne impaire
      end;

      // Voisin bas-droite
      if (i < Columns - 1) then
      begin
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[2] := HexGrid[i + 1][j].Number  // Voisin bas-droite pour colonne paire
        else if (i mod 2 = 1) and (j < Rows - 1) then
          HexGrid[i][j].Neighbors[2] := HexGrid[i + 1][j + 1].Number;  // Voisin bas-droite pour colonne impaire
      end;

      if j < Rows - 1 then
        HexGrid[i][j].Neighbors[3] := HexGrid[i][j + 1].Number; // Voisin du bas

      // Voisin bas-gauche
      if (i > 0) then
      begin
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[4] := HexGrid[i - 1][j].Number  // Voisin bas-gauche pour colonne paire
        else if (i mod 2 = 1) and (j < Rows - 1) then
          HexGrid[i][j].Neighbors[4] := HexGrid[i - 1][j + 1].Number;  // Voisin bas-gauche pour colonne impaire
      end;

      // Voisin haut-gauche
      if (i > 0) then
      begin
        if (i mod 2 = 0) and (j > 0) then
          HexGrid[i][j].Neighbors[5] := HexGrid[i - 1][j - 1].Number  // Voisin haut-gauche pour colonne paire
        else if (i mod 2 = 1) then
          HexGrid[i][j].Neighbors[5] := HexGrid[i - 1][j].Number;  // Voisin haut-gauche pour colonne impaire
      end;
    end;
  end;
end;

// Convertit une couleur TColor en chaîne de caractères
function ColorToString(c: TColor): String;
begin
  Result := Format('R: %d G: %d B: %d', [c.r, c.g, c.b]);
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
      offsetX := i * (HexWidth * 0.75);  // Décalage horizontal avec 3/4 de la largeur d'un hexagone
      offsetY := j * HexHeight;
      if (i mod 2) = 1 then
        offsetY := offsetY + (HexHeight / 2);  // Décalage pour créer la disposition en "nid d'abeille"

      // Définition du centre de l'hexagone
      HexGrid[i][j].Center := Vector2Create(Round(HexRadius + offsetX), Round(HexRadius + offsetY));

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

  CalculateNeighbors(); // Calcule les voisins contigus de chaque hexagone
end;

// Dessine la grille hexagonale avec les numéros d'hexagones au centre
procedure DrawHexGrid();
var
  k: Integer;
  hexPoints: array[0..5] of tVector2;
  hexNumberText: array[0..5] of Char;
  outlineColor: TColor;
  CurrentTime: TDateTime;
  BlinkState: Boolean;
begin
  CurrentTime := Now;  // Obtenir le temps actuel
  BlinkState := MilliSecondsBetween(CurrentTime, BlinkStartTime) mod 1000 < 500;

  for i := 0 to Columns - 1 do
  begin
    for j := 0 to Rows - 1 do
    begin
      for k := 0 to 5 do
        hexPoints[k] := Vector2Create(HexGrid[i][j].Vertices[k].x, HexGrid[i][j].Vertices[k].y);

      DrawPoly(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius - 1, 0, HexGrid[i][j].Color);

      if HexGrid[i][j].Selected and BlinkState then
        outlineColor := ORANGE
      else
        outlineColor := DARKGRAY;

      DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 2, outlineColor);
      StrPCopy(hexNumberText, IntToStr(HexGrid[i][j].Number));
      DrawText(hexNumberText, Round(HexGrid[i][j].Center.x - 10), Round(HexGrid[i][j].Center.y - 10), 20, BLACK);
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
          HexGrid[i][j].Selected := True;
          SelectedHex := HexGrid[i][j];
          HexSelected := True;
          BlinkStartTime := Now;  // Démarrer le clignotement
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
                       'Couleur: %s'#10 +
                       'Voisins: %d, %d, %d, %d, %d, %d',
                       [SelectedHex.Number, ColorToString(SelectedHex.Color),
                        SelectedHex.Neighbors[0], SelectedHex.Neighbors[1], SelectedHex.Neighbors[2],
                        SelectedHex.Neighbors[3], SelectedHex.Neighbors[4], SelectedHex.Neighbors[5]]);
    DrawText(PChar(InfoText), 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
  end
  else
    DrawText('Aucun hexagone sélectionné.', 20, WindowHeight - InfoBoxHeight + 20, 20, BLACK);
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Flat Top with Info');
  SetTargetFPS(60);
  InitializeHexGrid();
  BlinkStartTime := Now;

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








