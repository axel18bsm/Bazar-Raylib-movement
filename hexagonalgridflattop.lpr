program HexagonGrid;

{$mode objfpc}{$H+}

uses
  raylib,
  math;

const
  gridWidth = 8;                 // Largeur de la grille (colonnes)
  gridHeight = 10;                // Hauteur de la grille (lignes)
  hexRadius = 30;                 // Rayon de chaque hexagone
  hexHeight = hexRadius * sqrt(3);  // Hauteur d'un hexagone
  windowWidth = 800;
  windowHeight = 600;

type
  THexagon = record
    X, Y: Integer;                // Position centrale de l'hexagone
    Number: Integer;              // Numéro de l'hexagone
  end;

var
  hexagons: array[1..gridWidth * gridHeight] of THexagon; // Tableau d'hexagones
  i: Integer;  // Variable globale pour les boucles

// Procédure pour dessiner un hexagone à partir de ses coordonnées centrales
procedure DrawHexagon(hex: THexagon);
var
  i: Integer;
  angle: Float;
  point1, point2: TVector2;
begin
  for i := 0 to 5 do
  begin
    // Calcul des sommets de l'hexagone avec un angle de rotation
    angle := i * 2 * Pi / 6;
    point1.x := hex.X + Round(cos(angle) * hexRadius);
    point1.y := hex.Y + Round(sin(angle) * hexRadius);
    point2.x := hex.X + Round(cos(angle + 2 * Pi / 6) * hexRadius);
    point2.y := hex.Y + Round(sin(angle + 2 * Pi / 6) * hexRadius);

    // Dessiner les segments de l'hexagone
    DrawLineV(point1, point2, DARKGRAY);
  end;

  // Afficher le numéro de l'hexagone au centre
  //DrawText(PChar(IntToStr(hex.Number)), hex.X - 5, hex.Y - 5, 10, BLACK);
end;

// Génération des hexagones dans la grille
procedure GenerateHexagons;
var
  x, y, i: Integer;
begin
  i := 1;
  for y := 0 to gridHeight - 1 do
  begin
    for x := 0 to gridWidth - 1 do
    begin
      // Calcul des coordonnées de chaque hexagone
      hexagons[i].X := Round(x * hexRadius * 1.5 + (y mod 2) * hexRadius * 0.75);
      hexagons[i].Y := Round(y * hexHeight * 0.87);  // Décalage vertical

      hexagons[i].Number := i;
      Inc(i);
    end;
  end;
end;

begin
  // Initialisation de Raylib
  InitWindow(windowWidth, windowHeight, 'Hexagonal Grid - Flat Top 8x10');
  SetTargetFPS(60);

  // Génération des hexagones dans la grille
  GenerateHexagons;

  // Boucle principale
  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessin de chaque hexagone dans la grille
    for i := 1 to gridWidth * gridHeight do
    begin
      DrawHexagon(hexagons[i]);
    end;

    EndDrawing();
  end;

  // Fermeture de la fenêtre Raylib
  CloseWindow();
end.


