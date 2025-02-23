program bresenhamrondwithtwoobjectsandmountain;

uses raylib, sysutils, math;

 type

  TObject = record
    x, y: Integer;
    targetX, targetY: Integer;
    speed: Integer;
    originalSpeed: Integer;  // Pour stocker la vitesse d'origine
    color: TColor;
    startTime: Double;    // Temps de départ
    endTime: Double;      // Temps d'arrêt
    hasStopped: Boolean;  // Indicateur d'arrêt
    isSquare: Boolean;    // Indicateur de forme (cercle ou carré)
    points: array of TVector2; // Tableau dynamique de points
  end;

const
  detectionRadius = 50;
  staticX = 300;
  staticY = 125;
  mountainLeftX = 100;
  mountainRightX = 200;
  mountainTopY = 0;
  mountainBottomY = 400;

procedure AddPoint(var obj: TObject);
var
  newPoint: TVector2;
  pointCount: Integer;
begin
  newPoint.x := obj.x;
  newPoint.y := obj.y;

  // Ajouter le point au tableau dynamique
  pointCount := Length(obj.points);
  SetLength(obj.points, pointCount + 1);
  obj.points[pointCount] := newPoint;
end;

procedure MoveObject(var obj: TObject);
var
  dx, dy, dist: Single;
  stepX, stepY: Single;
begin
  dx := obj.targetX - obj.x;
  dy := obj.targetY - obj.y;
  dist := Sqrt(dx * dx + dy * dy);

  // Si la distance est inférieure ou égale à la vitesse, arrêter l'objet
  if (dist <= obj.speed) and (not obj.hasStopped) then
  begin
    obj.speed := 0;             // Arrêter l'objet en mettant la vitesse à 0
    obj.endTime := GetTime();    // Enregistrer le temps d'arrêt
    obj.hasStopped := True;      // Marquer l'objet comme arrêté
    // Placer l'objet exactement à la position de la cible pour plus de précision
    obj.x := obj.targetX;
    obj.y := obj.targetY;
  end
  else if not obj.hasStopped then
  begin
    // Ajouter le point courant dans le tableau
    AddPoint(obj);

    // Normaliser la direction et multiplier par la vitesse
    stepX := (dx / dist) * obj.speed;
    stepY := (dy / dist) * obj.speed;

    obj.x := obj.x + Round(stepX);
    obj.y := obj.y + Round(stepY);
  end;
end;

procedure DetectProximity(var obj: TObject; staticX, staticY: Integer);
var
  distToStatic: Single;
begin
  // Calculer la distance entre l'objet et l'objet statique
  distToStatic := Sqrt(Sqr(obj.x - staticX) + Sqr(obj.y - staticY));

  // Si l'objet est dans le rayon de détection, changer sa forme en carré
  if distToStatic <= detectionRadius then
    obj.isSquare := True
  else
    obj.isSquare := False;
end;

procedure CheckMountain(var obj: TObject);
begin
  // Vérifier si l'objet est à l'intérieur de la montagne
  if (obj.x >= mountainLeftX) and (obj.x <= mountainRightX) and (obj.y >= mountainTopY) and (obj.y <= mountainBottomY) then
  begin
    if obj.speed = obj.originalSpeed then  // Réduire la vitesse seulement si elle est à son niveau d'origine
      obj.speed := obj.speed div 2;        // Réduire la vitesse de moitié
  end
  else
  begin
    // Si l'objet sort de la montagne, rétablir sa vitesse d'origine
    obj.speed := obj.originalSpeed;
  end;
end;

var
  obj1, obj2: TObject;
  timeElapsed1, timeElapsed2: string;
  i: Integer;
begin
  // Initialisation de la fenêtre Raylib
  InitWindow(800, 600, 'Moving Objects with Mountain and Speed Change');

  // Initialisation de l'objet 1
  obj1.x := 50; obj1.y := 100;
  obj1.targetX := 700; obj1.targetY := 100;
  obj1.speed := 10;
  obj1.originalSpeed := obj1.speed;  // Sauvegarder la vitesse d'origine
  obj1.color := RED;
  obj1.startTime := GetTime();  // Enregistrer le temps de départ
  obj1.hasStopped := False;     // L'objet n'est pas encore arrêté
  obj1.isSquare := False;       // Par défaut, l'objet est un cercle
  SetLength(obj1.points, 0);    // Initialisation du tableau dynamique

  // Initialisation de l'objet 2
  obj2.x := 50; obj2.y := 50;
  obj2.targetX := 700; obj2.targetY := 50;
  obj2.speed := 5;
  obj2.originalSpeed := obj2.speed;  // Sauvegarder la vitesse d'origine
  obj2.color := BLUE;
  obj2.startTime := GetTime();  // Enregistrer le temps de départ
  obj2.hasStopped := False;     // L'objet n'est pas encore arrêté
  obj2.isSquare := False;       // Par défaut, l'objet est un cercle
  SetLength(obj2.points, 0);    // Initialisation du tableau dynamique

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    // Vérifier si les objets sont dans la montagne et ajuster leur vitesse
    CheckMountain(obj1);
    CheckMountain(obj2);

    // Déplacer les objets en fonction de leur vitesse
    MoveObject(obj1);
    MoveObject(obj2);

    // Détecter la proximité des objets avec l'objet statique
    DetectProximity(obj1, staticX, staticY);
    DetectProximity(obj2, staticX, staticY);

    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessiner les points stockés pour l'objet 1
    for i := 0 to High(obj1.points) do
    begin
      DrawCircle(round(obj1.points[i].x), round(obj1.points[i].y), 3, Fade(obj1.color, 0.5)); // Dessiner les points de l'objet 1
    end;

    // Dessiner les points stockés pour l'objet 2
    for i := 0 to High(obj2.points) do
    begin
      DrawCircle(round(obj2.points[i].x), round(obj2.points[i].y), 3, Fade(obj2.color, 0.5)); // Dessiner les points de l'objet 2
    end;

    // Dessiner l'objet 1, carré s'il est proche de l'objet statique
    if obj1.isSquare then
      DrawRectangle(obj1.x - 10, obj1.y - 10, 20, 20, obj1.color)  // Objet 1 devient un carré
    else
      DrawCircle(obj1.x, obj1.y, 10, obj1.color);                  // Objet 1 reste un cercle

    // Dessiner l'objet 2, carré s'il est proche de l'objet statique
    if obj2.isSquare then
      DrawRectangle(obj2.x - 10, obj2.y - 10, 20, 20, obj2.color)  // Objet 2 devient un carré
    else
      DrawCircle(obj2.x, obj2.y, 10, obj2.color);                  // Objet 2 reste un cercle

    // Dessiner l'objet statique
    DrawCircle(staticX, staticY, 10, DARKGRAY);  // Objet statique en gris

    // Dessiner la montagne
    DrawRectangleLines(mountainLeftX, mountainTopY, mountainRightX - mountainLeftX, mountainBottomY - mountainTopY, DARKGREEN);

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



