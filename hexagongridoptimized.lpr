program HexagonGridFlatTop;

{$mode objfpc}{$H+}

uses
  raylib,
  math;

const
  gridWidth = 8;                // Nombre de colonnes
  gridHeight = 10;               // Nombre de lignes
  hexRadius = 30;                // Rayon de chaque hexagone
  hexWidth = 2 * hexRadius;      // Largeur de l'hexagone (tête plate)
  hexHeight = sqrt(3) * hexRadius;  // Hauteur de l'hexagone (distance verticale entre deux points)
  windowWidth = 800;
  windowHeight = 600;

type
  THexagon = record
    X, Y: Integer;              // Position du centre de l'hexagone
    Number: Integer;            // Numéro unique de l'hexagone
  end;

var
  hexagons: array[1..gridWidth * gridHeight] of THexagon;
  i: Integer;  // Variable de boucle

// Fonction pour dessiner un hexagone à partir de son centre (X, Y)
procedure DrawHexagon(hex: THexagon);
var
  i: Integer;
  angle: Float;
  point1, point2: TVector2;
begin
  for i := 0 to 5 do
  begin
    // Calculer les vertices de l'hexagone en utilisant l'angle (hexagone tête plate)
    angle := Pi / 3 * i; // Angle = 60° entre chaque sommet
    point1.x := hex.X + Round(cos(angle) * hexRadius);
    point1.y := hex.Y + Round(sin(angle) * hexRadius);
    point2.x := hex.X + Round(cos(angle + Pi / 3) * hexRadius);
    point2.y := hex.Y + Round(sin(angle + Pi / 3) * hexRadius);

    // Dessiner les côtés de l'hexagone
    DrawLineV(point1, point2, DARKGRAY);
  end;

  // Afficher le numéro de l'hexagone au centre
  //DrawText(PChar(IntToStr(hex.Number)), hex.X - 10, hex.Y - 10, 20, BLACK);
end;

// Procédure pour générer la grille d'hexagones
procedure GenerateHexagons;
var
  x, y, i: Integer;
  offsetX, offsetY: Single;
begin
  i := 1;
  for y := 0 to gridHeight - 1 do
  begin
    for x := 0 to gridWidth - 1 do
    begin
      // Calcul de la position horizontale de chaque hexagone
      offsetX := x * (hexWidth * 0.75); // Décalage horizontal avec 75% de la largeur

      // Décalage vertical pour aligner les lignes paires et impaires
      offsetY := y * (hexHeight);

      // Si la ligne est impaire, on ajoute un décalage à gauche
      if (x mod 2) = 1 then
        offsetY := offsetY + (hexHeight / 2);

      // Enregistrer la position centrale de l'hexagone
      hexagons[i].X := Round(offsetX + hexRadius);
      hexagons[i].Y := Round(offsetY + hexHeight / 2);
      hexagons[i].Number := i;
      Inc(i);
    end;
  end;
end;

begin
  // Initialisation de Raylib
  InitWindow(windowWidth, windowHeight, 'Hexagonal Grid - Flat Top');
  SetTargetFPS(60);

  // Génération de la grille d'hexagones
  GenerateHexagons;

  // Boucle principale
  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessin des hexagones dans la grille
    for i := 1 to gridWidth * gridHeight do
    begin
      DrawHexagon(hexagons[i]);
    end;

    EndDrawing();
  end;

  // Fermeture de la fenêtre Raylib
  CloseWindow();
end.

