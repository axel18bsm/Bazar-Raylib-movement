

program BresenhamRondWithTwoObjects;

uses raylib;

type
  TObject = record
    x, y: Integer;
    targetX, targetY: Integer;
    speed: Integer;
    color: TColor;
  end;

procedure MoveObject(var obj: TObject);
var
  dx, dy, dist: Single;
  stepX, stepY: Single;
begin
  dx := obj.targetX - obj.x;
  dy := obj.targetY - obj.y;
  dist := Sqrt(dx * dx + dy * dy);

  // Si la distance à la cible est inférieure ou égale à 1 pixel, arrêter l'objet
  if dist <= 1 then
  begin
    obj.speed := 0;  // Arrêter l'objet en mettant la vitesse à 0
  end
  else
  begin
    // Normaliser la direction et multiplier par la vitesse
    stepX := (dx / dist) * obj.speed;
    stepY := (dy / dist) * obj.speed;

    obj.x := obj.x + Round(stepX);
    obj.y := obj.y + Round(stepY);
  end;
end;

var
  obj1, obj2: TObject;
begin
  // Initialisation de la fenêtre Raylib
  InitWindow(800, 600, 'Moving Objects Without Drawing Lines');

  // Initialisation de l'objet 1
  obj1.x := 50; obj1.y := 100;
  obj1.targetX := 700; obj1.targetY := 100;
  obj1.speed := 10;
  obj1.color := RED;

  // Initialisation de l'objet 2
  obj2.x := 50; obj2.y := 50;
  obj2.targetX := 700; obj2.targetY := 50;
  obj2.speed := 3;
  obj2.color := BLUE;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    // Déplacer les objets en fonction de leur vitesse
    MoveObject(obj1);
    MoveObject(obj2);

    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessiner les objets (représentés par des cercles)
    DrawCircle(obj1.x, obj1.y, 10, obj1.color); // Objet 1 (rouge)
    DrawCircle(obj2.x, obj2.y, 10, obj2.color); // Objet 2 (bleu)

    EndDrawing();
  end;

  // Fermeture de la fenêtre
  CloseWindow();
end.
