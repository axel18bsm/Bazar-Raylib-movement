program simpleraylibexample;

{$mode objfpc}{$H+}
uses
  raylib;  // Utilisation de la bibliothèque Raylib

const
  ScreenWidth = 800;
  ScreenHeight = 450;

var
  CircleX, CircleY: Integer;  // Position du cercle
  CircleSpeed: Integer;       // Vitesse de déplacement du cercle

begin
  // Initialisation de la fenêtre
  InitWindow(ScreenWidth, ScreenHeight, 'Exemple avec Raylib en Pascal');
  SetTargetFPS(60);  // Définit la limite de FPS (images par seconde)

  // Position initiale du cercle
  CircleX := ScreenWidth div 2;
  CircleY := ScreenHeight div 2;
  CircleSpeed := 5;  // Définition de la vitesse de déplacement

  // Boucle principale
  while not WindowShouldClose() do
  begin
    // Gestion de l'entrée clavier pour déplacer le cercle
    if IsKeyDown(KEY_RIGHT) then
      CircleX := CircleX + CircleSpeed
    else if IsKeyDown(KEY_LEFT) then
      CircleX := CircleX - CircleSpeed;

    if IsKeyDown(KEY_UP) then
      CircleY := CircleY - CircleSpeed
    else if IsKeyDown(KEY_DOWN) then
      CircleY := CircleY + CircleSpeed;

    // Démarre le dessin de la fenêtre
    BeginDrawing();

    // Efface l'écran avec une couleur de fond
    ClearBackground(RAYWHITE);

    // Affiche le cercle à la position actuelle
    DrawCircle(CircleX, CircleY, 20, DARKBLUE);

    // Affiche un texte à l'écran
    DrawText('Utilisez les touches fléchées pour déplacer le cercle!', 10, 10, 20, DARKGRAY);

    // Fin du dessin
    EndDrawing();
  end;

  // Libère les ressources avant de quitter
  CloseWindow();
end.








