program hexflattopnumberedstruct2;

uses raylib, math, sysutils, strutils;

const
  HexDiameter = 60;         // Diamètre de chaque hexagone
  HexRadius = HexDiameter / 2;  // Rayon de chaque hexagone
  HexWidth = HexDiameter;    // Largeur de l'hexagone (flat-top)
  HexHeight = HexRadius * sqrt(3);  // Hauteur totale d'un hexagone (distance entre deux sommets opposés)
  Columns = 6;               // Nombre d'hexagones en largeur
  Rows = 5;                  // Nombre d'hexagones en hauteur
  WindowWidth = 1000;        // Largeur de la fenêtre (ajoutée pour les boutons)
  WindowHeight = 600;        // Hauteur de la fenêtre
  ButtonWidth = 140;
  ButtonHeight = 40;

type
  TPoint = record
    x, y: Integer;
  end;

  // Structure d'un hexagone avec un numéro, centre, couleur et sélection
  THexCell = record
    Number: Integer;           // Numéro de l'hexagone (de 1 à 30)
    Center: TVector2;          // Vecteur du centre de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;             // Couleur de l'hexagone
    Selected: Boolean;         // État de sélection
    Neighbors: array[0..5] of Integer;  // Numéros des hexagones voisins
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j, hexNumber: Integer;
  SaveButtonRec, LoadButtonRec: TRectangle;
  SaveButtonPressed, LoadButtonPressed: Boolean;
  Fields: array[0..10] of String;  // Tableau pour stocker les champs de données lors du chargement

// Fonction personnalisée pour diviser une chaîne en un tableau de chaînes
procedure SplitLineIntoFields(const line: String; const delimiter: Char; var Output: array of String);
var
  i, startPos, fieldIndex: Integer;
begin
  startPos := 1;
  fieldIndex := 0;
  for i := 1 to Length(line) do
  begin
    if (line[i] = delimiter) or (i = Length(line)) then
    begin
      if i = Length(line) then
        Output[fieldIndex] := Copy(line, startPos, i - startPos + 1)
      else
        Output[fieldIndex] := Copy(line, startPos, i - startPos);
      Inc(fieldIndex);
      startPos := i + 1;
    end;
  end;
end;

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

// Initialise la grille hexagonale "tête plate" avec des numéros, couleurs et centre
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  hexNumber := 1;  // Initialisation du compteur d'hexagones

  for i := 0 to Columns - 1 do
  begin
    offsetX := i * (HexWidth * 0.75);  // Décalage horizontal avec 3/4 de la largeur d'un hexagone

    for j := 0 to Rows - 1 do
    begin
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
end;

// Sauvegarde la grille dans un fichier
procedure SaveHexGridToFile();
var
  FileHandle: TextFile;
begin
  AssignFile(FileHandle, 'hexgrid.txt');
  Rewrite(FileHandle);
  for i := 0 to Columns - 1 do
    for j := 0 to Rows - 1 do
      with HexGrid[i][j] do
        WriteLn(FileHandle, Format('%d,%d,%d,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d',
          [i, j, Number, BoolToStr(Selected, True), Color.r, Color.g, Color.b,
           Neighbors[0], Neighbors[1], Neighbors[2], Neighbors[3], Neighbors[4], Neighbors[5]]));
  CloseFile(FileHandle);
end;

// Charge la grille à partir d'un fichier
procedure LoadHexGridFromFile();
var
  FileHandle: TextFile;
  line: String;
  row, col: Integer;
begin
  if not FileExists('hexgrid.txt') then Exit;

  AssignFile(FileHandle, 'hexgrid.txt');
  Reset(FileHandle);
  while not EOF(FileHandle) do
  begin
    ReadLn(FileHandle, line);
    SplitLineIntoFields(line, ',', Fields);

    row := StrToInt(Fields[0]);
    col := StrToInt(Fields[1]);
    HexGrid[row][col].Number := StrToInt(Fields[2]);
    HexGrid[row][col].Selected := StrToBool(Fields[3]);
    HexGrid[row][col].Color := ColorCreate(StrToInt(Fields[4]), StrToInt(Fields[5]), StrToInt(Fields[6]), 255);

    for i := 0 to 5 do
      HexGrid[row][col].Neighbors[i] := StrToInt(Fields[7 + i]);
  end;
  CloseFile(FileHandle);
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

// Dessine la grille hexagonale avec les numéros d'hexagones au centre
procedure DrawHexGrid();
var
  k: Integer;
  hexPoints: array[0..5] of tVector2;
  hexNumberText: array[0..5] of Char;
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

      // Dessiner les bords de l'hexagone
      if HexGrid[i][j].Selected then
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 3, ORANGE)
      else
        DrawPolyLinesEx(Vector2Create(HexGrid[i][j].Center.x, HexGrid[i][j].Center.y), 6, HexRadius, 0, 1, DARKGRAY);

      // Affiche le numéro de l'hexagone au centre
      StrPCopy(hexNumberText, IntToStr(HexGrid[i][j].Number));
      DrawText(hexNumberText, Round(HexGrid[i][j].Center.x - 10), Round(HexGrid[i][j].Center.y - 10), 20, BLACK);
    end;
  end;
end;

begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid - Save and Load');
  SetTargetFPS(60);

  InitializeHexGrid();

  SaveButtonRec := RectangleCreate(WindowWidth - 160, 50, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(WindowWidth - 160, 110, ButtonWidth, ButtonHeight);

  while not WindowShouldClose() do
  begin
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
    begin
      if CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
        SaveHexGridToFile()
      else if CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
        LoadHexGridFromFile();
    end;

    HandleMouseClick();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawHexGrid();

    // Dessin des boutons
    DrawRectangleRec(SaveButtonRec, LIGHTGRAY);
    DrawText('Sauvegarder', Round(SaveButtonRec.x + 10), Round(SaveButtonRec.y + 10), 20, BLACK);

    DrawRectangleRec(LoadButtonRec, LIGHTGRAY);
    DrawText('Charger', Round(LoadButtonRec.x + 10), Round(LoadButtonRec.y + 10), 20, BLACK);

    EndDrawing();
  end;

  CloseWindow();
end.









