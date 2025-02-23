
program bresenhamrondwithtwoobjectsanddistancelimitsecond;

uses raylib, sysutils, math;

type
  TVector = record
    x, y: Integer;
  end;

  TObject = record
    x, y: Integer;
    initialX, initialY: Integer;  // Stocker la position initiale
    targetX, targetY: Integer;
    speed: Integer;
    originalSpeed: Integer;  // Pour stocker la vitesse d'origine
    color: TColor;
    startTime: Double;    // Temps de départ
    endTime: Double;      // Temps d'arrêt
    hasStopped: Boolean;  // Indicateur d'arrêt
    hasMoved: Boolean;    // Indicateur de mouvement (au moins 1 déplacement)
    points: array of TVector; // Tableau dynamique de points
  end;

const
  detectionRadius = 30;
  staticX = 300;
  staticY = 125;
  mountainLeftX = 100;
  mountainRightX = 200;
  mountainTopY = 0;
  mountainBottomY = 400;
  maxMoveTime = 2;  // Temps maximum de déplacement (1,5 secondes)

procedure AddPoint(var obj: TObject);
var
  newPoint: TVector;
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
  currentTime: Double;
  distanceTraveled: Single;
  maxDistance: Single;
begin
  currentTime := GetTime() - obj.startTime;  // Temps écoulé

  // Calculer la distance maximale que l'objet peut parcourir (vitesse * 50)
  maxDistance := obj.speed * 100;

  // Calculer la distance parcourue depuis la position initiale
  distanceTraveled := Sqrt(Sqr(obj.x - obj.initialX) + Sqr(obj.y - obj.initialY));

  // Vérifier si l'objet a dépassé la distance maximale ou le temps maximum de déplacement
  if ((currentTime >= maxMoveTime) or (distanceTraveled >= maxDistance)) and (not obj.hasStopped) then
  begin
    obj.speed := 0;             // Arrêter l'objet
    obj.endTime := GetTime();    // Enregistrer le temps d'arrêt
    obj.hasStopped := True;      // Marquer l'objet comme arrêté
    Exit;
  end;

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
    // Vérifier si l'objet a bougé par rapport à sa position initiale
    if (obj.x <> obj.initialX) or (obj.y <> obj.initialY) then
      obj.hasMoved := True;  // Mettre le drapeau de mouvement à True

    // Ajouter le point courant dans le tableau
    AddPoint(obj);

    // Normaliser la direction et multiplier par la vitesse
    stepX := (dx / dist) * obj.speed;
    stepY := (dy / dist) * obj.speed;

    obj.x := obj.x + Round(stepX);
    obj.y := obj.y + Round(stepY);
  end;
end;

procedure DetectProximityAndStop(var obj: TObject; staticX, staticY: Integer);
var
  distToStatic: Single;
begin
  // Calculer la distance entre l'objet et l'objet statique
  distToStatic := Sqrt(Sqr(obj.x - staticX) + Sqr(obj.y - staticY));

  // Si l'objet est dans le rayon de détection, arrêter l'objet
  if (distToStatic <= detectionRadius) and (not obj.hasStopped) then
  begin
    obj.speed := 0;             // Arrêter l'objet
    obj.endTime := GetTime();    // Enregistrer le temps d'arrêt
    obj.hasStopped := True;      // Marquer l'objet comme arrêté
  end;
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
  InitWindow(800, 600, 'Moving Objects with Distance and Time Limit');

  // Initialisation de l'objet 1
  obj1.x := 50; obj1.y := 100;
  obj1.initialX := obj1.x;  // Stocker la position initiale
  obj1.initialY := obj1.y;
  obj1.targetX := 700; obj1.targetY := 100;
  obj1.speed := 4;
  obj1.originalSpeed := obj1.speed;  // Sauvegarder la vitesse d'origine
  obj1.color := RED;
  obj1.startTime := GetTime();  // Enregistrer le temps de départ
  obj1.hasStopped := False;     // L'objet n'est pas encore arrêté
  obj1.hasMoved := False;       // L'objet n'a pas encore bougé
  SetLength(obj1.points, 0);    // Initialisation du tableau dynamique

  // Initialisation de l'objet 2
  obj2.x := 50; obj2.y := 50;
  obj2.initialX := obj2.x;  // Stocker la position initiale
  obj2.initialY := obj2.y;
  obj2.targetX := 700; obj2.targetY := 50;
  obj2.speed := 25;
  obj2.originalSpeed := obj2.speed;  // Sauvegarder la vitesse d'origine
  obj2.color := BLUE;
  obj2.startTime := GetTime();  // Enregistrer le temps de départ
  obj2.hasStopped := False;     // L'objet n'est pas encore arrêté
  obj2.hasMoved := False;       // L'objet n'a pas encore bougé
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

    // Détecter la proximité des objets avec l'objet statique et les arrêter si nécessaire
    DetectProximityAndStop(obj1, staticX, staticY);
    DetectProximityAndStop(obj2, staticX, staticY);

    BeginDrawing();
    ClearBackground(RAYWHITE);

    // Dessiner les points stockés pour l'objet 1
    for i := 0 to High(obj1.points) do
    begin
      DrawCircle(obj1.points[i].x, obj1.points[i].y, 3, Fade(obj1.color, 0.5)); // Dessiner les points de l'objet 1
    end;

    // Dessiner les points stockés pour l'objet 2
    for i := 0 to High(obj2.points) do
    begin
      DrawCircle(obj2.points[i].x, obj2.points[i].y, 3, Fade(obj2.color, 0.5)); // Dessiner les points de l'objet 2
    end;

    // Dessiner l'objet 1
    DrawCircle(obj1.x, obj1.y, 10, obj1.color); // Objet 1 (rouge)

    // Dessiner l'objet 2
    DrawCircle(obj2.x, obj2.y, 10, obj2.color); // Objet 2 (bleu)

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

    // Afficher l'état de déplacement de chaque objet
    if obj1.hasMoved then
      DrawText('Object 1: Has moved', 10, 10, 20, obj1.color)
    else
      DrawText('Object 1: Has not moved', 10, 10, 20, obj1.color);

    if obj2.hasMoved then
      DrawText('Object 2: Has moved', 10, 40, 20, obj2.color)
    else
      DrawText('Object 2: Has not moved', 10, 40, 20, obj2.color);

    EndDrawing();
  end;

  // Fermeture de la fenêtre
  CloseWindow();
end.





