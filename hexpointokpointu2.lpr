
program CheckCollisionPointPolyExample;

uses
  raylib, // Bibliothèque raylib
  raymath; // Bibliothèque raymath pour utiliser Vector2

const
  ScreenWidth = 800;
  ScreenHeight = 600;
  PolygonPointsCount = 6; // Nombre de points dans le polygone

var
  Polygon: array[0..PolygonPointsCount-1] of TVector2; // Sommets du polygone
  i: Integer;
  MousePosition: TVector2;
  InsidePolygon: Boolean;

begin
  // Initialisation de la fenêtre
  InitWindow(ScreenWidth, ScreenHeight, 'CheckCollisionPointPoly Example');
  SetTargetFPS(60);

  // Définition des sommets du polygone (hexagone, par exemple)
  Polygon[0] := Vector2Create(400, 200);
  Polygon[1] := Vector2Create(450, 250);
  Polygon[2] := Vector2Create(450, 320);
  Polygon[3] := Vector2Create(400, 370);
  Polygon[4] := Vector2Create(350, 320);
  Polygon[5] := Vector2Create(350, 250);

  while not WindowShouldClose() do
  begin
    // Position de la souris
    MousePosition := GetMousePosition();

    // Vérification de la collision entre le point et le polygone
    InsidePolygon := CheckCollisionPointPoly(MousePosition, @Polygon[0], PolygonPointsCount);

    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Affichage du polygone (change de couleur si la souris est dedans)
    if InsidePolygon then
      DrawPoly(Vector2Create(400, 285), PolygonPointsCount, 80, 0, GREEN) // Polygone vert si collision
    else
      DrawPoly(Vector2Create(400, 285), PolygonPointsCount, 80, 0, RED);  // Polygone rouge sinon

    // Affichage des informations sur la souris
    DrawText('Placez la souris à l''intérieur du polygone', 10, 10, 20, DARKGRAY);
    if InsidePolygon then
      DrawText('Souris à l''intérieur du polygone', 10, 40, 20, GREEN)
    else
      DrawText('Souris en dehors du polygone', 10, 40, 20, RED);

    EndDrawing();
  end;

  CloseWindow();
end.
