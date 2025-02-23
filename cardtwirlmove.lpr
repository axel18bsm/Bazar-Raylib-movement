
program cardtwirlmove;

uses raylib, math;

const
  ScreenWidth = 800;
  ScreenHeight = 600;
  CardWidth = 100;
  CardHeight = 150;
  CardSpeed = 0.015; // Vitesse de déplacement

//type
//  TVector2 = record
//    x, y: Single;
//  end;

var
  cardPos, startPos, endPos, controlPoint: TVector2;
  cardAngle: Single;
  t: Single;  // Paramètre d'interpolation
  cardRotationSpeed: Single;  // Vitesse de rotation
  animationEnded: Boolean;  // Détecter la fin du mouvement

// Fonction pour effectuer une interpolation quadratique pour le mouvement en courbe
function QuadraticBezier(p0, p1, p2: TVector2; t: Single): TVector2;
begin
  Result.x := (1 - t) * (1 - t) * p0.x + 2 * (1 - t) * t * p1.x + t * t * p2.x;
  Result.y := (1 - t) * (1 - t) * p0.y + 2 * (1 - t) * t * p1.y + t * t * p2.y;
end;

// Initialise la position de départ, d'arrivée, et le point de contrôle pour la courbe
procedure InitializeCardMovement();
begin
  startPos := Vector2Create(100, 500); // Point A
  endPos := Vector2Create(700, 400);   // Point B
  controlPoint :=Vector2Create(200, 200); // Point de contrôle de la courbe
  cardPos := startPos;
  cardAngle := 0;
  t := 0;
  cardRotationSpeed := 25.0; // Vitesse de rotation en degrés par frame
  animationEnded := False;
end;

// Met à jour la position et la rotation de la carte
procedure UpdateCard();
begin
  if not animationEnded then
  begin
    t := t + CardSpeed;
    if t >= 1 then
    begin
      t := 1;
      animationEnded := True;
    end;

    // Calcul de la nouvelle position avec une interpolation quadratique
    cardPos := QuadraticBezier(startPos, controlPoint, endPos, t);

    // Rotation de la carte sur elle-même
    cardAngle := cardAngle + cardRotationSpeed;
    if cardAngle >= 360 then
      cardAngle := cardAngle - 360;
  end;
end;

// Dessine la carte à l'écran en appliquant une rotation
procedure DrawCard();
begin
  BeginDrawing();
  ClearBackground(RAYWHITE);

  // Dessiner la carte en appliquant une rotation autour de son centre
  DrawText('Carte en mouvement et rotation...', 10, 10, 20, DARKGRAY);
  DrawText('Fin d\''animation si la carte atteint le point B.', 10, 40, 20, DARKGRAY);

  DrawRectanglePro(RectangleCreate(cardPos.x, cardPos.y, CardWidth, CardHeight),
                   Vector2Create(CardWidth / 2, CardHeight / 2), cardAngle, BLUE);

  EndDrawing();
end;

begin
  InitWindow(ScreenWidth, ScreenHeight, 'Carte qui virevolte en tournoyant');
  SetTargetFPS(60);

  // Initialisation du mouvement de la carte
  InitializeCardMovement();

  while not WindowShouldClose() do
  begin
    UpdateCard();
    DrawCard();
  end;

  CloseWindow();
end.
