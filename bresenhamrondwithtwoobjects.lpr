

program bresenhamrondavectwoobjects;

uses raylib, sysutils;

type
  TObject = record
    x, y: Integer;
    targetX, targetY: Integer;
    speed: Integer;
    color: TColor;
    startTime: Double;  // Temps de départ
    endTime: Double;    // Temps d'arrêt
    hasStopped: Boolean;  // Indicateur d'arrêt
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
  if (dist <= 2) and (not obj.hasStopped) then
  begin
    obj.speed := 0;             // Arrêter l'objet en mettant la vitesse à 0
    obj.endTime := GetTime();    // Enregistrer le temps d'arrêt
    obj.hasStopped := True;      // Marquer l'objet comme arrêté
  end
  else if not obj.hasStopped then
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
  timeElapsed1, timeElapsed2: string;
begin
  // Initialisation de la fenêtre Raylib
  InitWindow(800, 600, 'mouvement');

  // Initialisation de l'objet 1
  obj1.x := 50; obj1.y := 100;
  obj1.targetX := 600; obj1.targetY := 400;
  obj1.speed := 10;
  obj1.color := RED;
  obj1.startTime := GetTime();  // Enregistrer le temps de départ
  obj1.hasStopped := False;     // L'objet n'est pas encore arrêté

  // Initialisation de l'objet 2
  obj2.x := 50; obj2.y := 50;
  obj2.targetX := 600; obj2.targetY := 50;
  obj2.speed := 5;
  obj2.color := BLUE;
  obj2.startTime := GetTime();  // Enregistrer le temps de départ
  obj2.hasStopped := False;     // L'objet n'est pas encore arrêté

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

    // Afficher le temps écoulé si l'objet 1 s'est arrêté
    if obj1.hasStopped then
    begin
      timeElapsed1 := Format('Time: %.2f sec', [obj1.endTime - obj1.startTime]);
      DrawText(PChar(timeElapsed1), obj1.targetX + 10, obj1.targetY, 20, obj1.color);
    end;

    // Afficher le temps écoulé si l'objet 2 s'est arrêté
    if obj2.hasStopped then
    begin
      timeElapsed2 := Format('Time: %.2f sec', [obj2.endTime - obj2.startTime]);
      DrawText(PChar(timeElapsed2), obj2.targetX + 10, obj2.targetY, 20, obj2.color);
    end;

    EndDrawing();
  end;

  // Fermeture de la fenêtre
  CloseWindow();
end.
