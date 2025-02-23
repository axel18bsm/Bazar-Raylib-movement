program hexflattop_fullgrid_with_save_load_buttons;

uses raylib, math, sysutils, strutils;

const
  HexDiameter = 60;
  HexRadius = HexDiameter / 2;
  HexWidth = HexDiameter;
  HexHeight = HexRadius * sqrt(3);
  Columns = 6;
  Rows = 5;
  WindowWidth = 1000;
  WindowHeight = 800;
  ButtonWidth = 140;
  ButtonHeight = 40;
  SaveFileName = 'hexgrid.txt';

type
  TPoint = record
    x, y: Integer;
  end;

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: Integer;
    Center: TVector2;
    Vertices: array[0..5] of TPoint;
    Color: TColor;
    Selected: Boolean;
    Neighbors: array[0..5] of Integer;
  end;

var
  HexGrid: array[0..Columns-1, 0..Rows-1] of THexCell;
  i, j, hexNumber: Integer;
  SelectedHex: THexCell;
  SaveButtonRec, LoadButtonRec: TRectangle;
  SaveButtonPressed, LoadButtonPressed: Boolean;
  SelectedHexInfo: String;
  TimePassed: Single;
  Blinking: Boolean;

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
    angle_deg := 60 * k;
    angle_rad := PI / 180 * angle_deg;
    Hex.Vertices[k].x := Round(Hex.Center.x + HexRadius * cos(angle_rad));
    Hex.Vertices[k].y := Round(Hex.Center.y + HexRadius * sin(angle_rad));
  end;
end;

// Calcule les voisins de chaque hexagone
procedure CalculateNeighbors();
begin
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      HexGrid[i][j].Neighbors := [0, 0, 0, 0, 0, 0];
      if j > 0 then HexGrid[i][j].Neighbors[0] := HexGrid[i][j - 1].Number;
      if i < Columns - 1 then
      begin
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[1] := ifthen((j > 0), HexGrid[i + 1][j - 1].Number, 0)
        else
          HexGrid[i][j].Neighbors[1] := HexGrid[i + 1][j];
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[2] := HexGrid[i + 1][j].Number
        else
          HexGrid[i][j].Neighbors[2] := ifthen((j < Rows - 1), HexGrid[i + 1][j + 1].Number, 0);
      end;
      if j < Rows - 1 then HexGrid[i][j].Neighbors[3] := HexGrid[i][j + 1].Number;
      if i > 0 then
      begin
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[4] := HexGrid[i - 1][j].Number
        else
          HexGrid[i][j].Neighbors[4] := ifthen((j < Rows - 1), HexGrid[i - 1][j + 1].Number, 0);
        if (i mod 2 = 0) then
          HexGrid[i][j].Neighbors[5] := ifthen((j > 0), HexGrid[i - 1][j - 1].Number, 0)
        else
          HexGrid[i][j].Neighbors[5] := HexGrid[i - 1][j].Number;
      end;
    end;
  end;
end;

// Initialise la grille hexagonale
procedure InitializeHexGrid();
var
  offsetX, offsetY: Single;
begin
  hexNumber := 1;
  for j := 0 to Rows - 1 do
  begin
    for i := 0 to Columns - 1 do
    begin
      offsetX := i * (HexWidth * 0.75);
      offsetY := j * HexHeight;
      if (i mod 2) = 1 then offsetY := offsetY + (HexHeight / 2);
      HexGrid[i][j].Center := Vector2Create(Round(HexRadius + offsetX), Round(HexRadius + offsetY));
      HexGrid[i][j].Number := hexNumber;
      Inc(hexNumber);
      if (i + j) mod 2 = 0 then HexGrid[i][j].Color := GREEN else HexGrid[i][j].Color := LIGHTGRAY;
      HexGrid[i][j].Selected := False;
      CalculateHexVertices(HexGrid[i][j]);
    end;
  end;
  CalculateNeighbors();
end;

// Sauvegarde la grille dans un fichier
procedure SaveGridToFile();
var
  FileHandle: TextFile;
  row, col: Integer;
  NeighborStr: String;
begin
  AssignFile(FileHandle, SaveFileName);
  Rewrite(FileHandle);
  for row := 0 to Rows - 1 do
    for col := 0 to Columns - 1 do
      with HexGrid[col][row] do
      begin
        NeighborStr := Format('%d,%d,%d,%d,%d,%d',
          [Neighbors[0], Neighbors[1], Neighbors[2], Neighbors[3], Neighbors[4], Neighbors[5]]);
        WriteLn(FileHandle, Format('%d,%d,%d,%d,%s,%s',
          [Number, Round(Center.x), Round(Center.y), Selected.ToString, ColorToInt(Color), NeighborStr]));
      end;
  CloseFile(FileHandle);
end;

// Charge la grille à partir d'un fichier
procedure LoadGridFromFile();
var
  FileHandle: TextFile;
  line: String;
begin
  AssignFile(FileHandle, SaveFileName);
  Reset(FileHandle);
  while not EOF(FileHandle) do
  begin
    ReadLn(FileHandle, line);
  end;
  CloseFile(FileHandle);
end;

// Programme principal
begin
  InitWindow(WindowWidth, WindowHeight, 'Hexagonal Grid with Save/Load');
  SetTargetFPS(60);
  InitializeHexGrid();
  SaveButtonRec := RectangleCreate(WindowWidth - ButtonWidth - 20, 20, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(WindowWidth - ButtonWidth - 20, 80, ButtonWidth, ButtonHeight);

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawText('Hexagon Grid with Save/Load', 20, 20, 20, DARKGRAY);
    DrawHexGrid();
    DrawRectangleRec(SaveButtonRec, LIGHTGRAY);
    DrawRectangleRec(LoadButtonRec, LIGHTGRAY);
    DrawText('Save Grid', Round(SaveButtonRec.x) + 20, Round(SaveButtonRec.y) + 10, 20, BLACK);
    DrawText('Load Grid', Round(LoadButtonRec.x) + 20, Round(LoadButtonRec.y) + 10, 20, BLACK);
    EndDrawing();
  end;
  CloseWindow();
end.










