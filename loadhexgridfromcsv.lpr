program loadhexgridfromcsv;

uses
  raylib, sysutils, classes;

type
  TEmplacement = (inconnu, CoinHG, CoinHD, CoinBG, CoinBD, BordH, BordB, BordG, BordD, Classic);

  // Structure d'un hexagone avec un numéro, centre, couleur, sélection et voisins
  THexCell = record
    Number: integer;              // Numéro de l'hexagone (de 1 à 30)
    Center: TPoint;               // Point central de l'hexagone
    Vertices: array[0..5] of TPoint;  // Les 6 sommets de l'hexagone
    Color: TColor;                // Couleur de l'hexagone
    Selected: boolean;            // État de sélection par clic
    Neighbors: array[1..6] of integer;  // Numéros des voisins contigus (6 voisins)
    Colonne: integer;                 // Quelle est la colonne de cet hexagone
    Ligne: integer;                   // Quelle est la ligne de cet hexagone
    Poshexagone: TEmplacement;        // Emplacement pour la stratégie des voisins
    PairImpairLigne: boolean;         // Aide à déterminer les voisins
  end;

const
  TotalNbreHex = 30;  // Total des hexagones
  LoadFileName = 'hexgrid.csv';

var
  HexGrid: array[1..TotalNbreHex] of THexCell;
  i: Integer;

// Fonction pour convertir une chaîne en couleur
function StringToColor(const ColorStr: string): TColor;
var
  Colors: TStringArray;
begin
  Colors := ColorStr.Split([',']);
  Result.r := StrToInt(Colors[0]);
  Result.g := StrToInt(Colors[1]);
  Result.b := StrToInt(Colors[2]);
  Result.a := 255;  // Alpha par défaut
end;

// Fonction pour convertir une chaîne en TEmplacement
function StringToEmplacement(const EmplacementStr: string): TEmplacement;
begin
  if EmplacementStr = 'CoinHG' then
    Result := CoinHG
  else if EmplacementStr = 'CoinHD' then
    Result := CoinHD
  else if EmplacementStr = 'CoinBG' then
    Result := CoinBG
  else if EmplacementStr = 'CoinBD' then
    Result := CoinBD
  else if EmplacementStr = 'BordH' then
    Result := BordH
  else if EmplacementStr = 'BordB' then
    Result := BordB
  else if EmplacementStr = 'BordG' then
    Result := BordG
  else if EmplacementStr = 'BordD' then
    Result := BordD
  else
    Result := Classic;
end;

// Procédure pour charger la structure HexGrid depuis un fichier CSV
procedure LoadHexGridFromCSV();
var
  F: TextFile;
  Line: string;
  Fields: TStringArray;
  i: Integer;
begin
  AssignFile(F, LoadFileName);
  Reset(F);
  try
    // Ignorer l'en-tête
    Readln(F, Line);

    i := 1;
    while not EOF(F) do
    begin
      Readln(F, Line);
      Fields := Line.Split([',']);

      // Charger les valeurs depuis le fichier CSV dans la structure HexCell
      HexGrid[i].Number := StrToInt(Fields[0]);
      HexGrid[i].Center.x := StrToInt(Fields[1]);
      HexGrid[i].Center.y := StrToInt(Fields[2]);
      HexGrid[i].Color := StringToColor(Fields[3] + ',' + Fields[4] + ',' + Fields[5]);
      HexGrid[i].Selected := StrToBool(Fields[6]);
      HexGrid[i].Colonne := StrToInt(Fields[7]);
      HexGrid[i].Ligne := StrToInt(Fields[8]);
      HexGrid[i].Poshexagone := StringToEmplacement(Fields[9]);
      HexGrid[i].PairImpairLigne := StrToBool(Fields[10]);

      // Charger les voisins
      HexGrid[i].Neighbors[1] := StrToInt(Fields[11]);
      HexGrid[i].Neighbors[2] := StrToInt(Fields[12]);
      HexGrid[i].Neighbors[3] := StrToInt(Fields[13]);
      HexGrid[i].Neighbors[4] := StrToInt(Fields[14]);
      HexGrid[i].Neighbors[5] := StrToInt(Fields[15]);
      HexGrid[i].Neighbors[6] := StrToInt(Fields[16]);

      Inc(i);
    end;
  finally
    CloseFile(F);
  end;
end;

// Initialisation de la fenêtre Raylib
procedure InitializeWindow();
begin
  InitWindow(800, 600, 'Hexagonal Grid - Load from CSV');
  SetTargetFPS(60);
end;

// Fonction principale
begin
  // Initialisation de la fenêtre Raylib
  InitializeWindow();

  // Charger la grille hexagonale depuis un fichier CSV
  LoadHexGridFromCSV();
   writeln('essai');
  // Boucle principale (vide ici car on se concentre sur le chargement)
  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawText('HexGrid has been loaded from hexgrid.csv', 100, 100, 20, DARKGRAY);
    EndDrawing();
  end;

  CloseWindow();
end.

