program changewindowcolorclic;

{$MODE OBJFPC}  // Utilisation du mode Object Pascal

uses
  raylib, sysutils;

const
  ScreenWidth = 800;
  ScreenHeight = 600;

type
  TButtonState = (Normal, Hover, Clicked);

var
  ButtonRect: TRectangle;
  ButtonState: TButtonState;
  BackgroundColor: TColor;

// Fonction pour dessiner le bouton et gérer l'état
procedure DrawButton(Button: TRectangle; Text: String; State: TButtonState);
begin
  // Changer la couleur du bouton selon l'état
  case State of
    Normal: DrawRectangle(Round(Button.X), Round(Button.Y), Round(Button.Width), Round(Button.Height), LIGHTGRAY);
    Hover: DrawRectangle(Round(Button.X), Round(Button.Y), Round(Button.Width), Round(Button.Height), GRAY);
    Clicked: DrawRectangle(Round(Button.X), Round(Button.Y), Round(Button.Width), Round(Button.Height), DARKGRAY);
  end;

  // Dessiner le texte sur le bouton
  DrawText(PChar(AnsiString(Text)), Round(Button.X) + 10, Round(Button.Y) + 10, 20, BLACK);
end;

// Fonction pour générer une nouvelle couleur aléatoire
function GetRandomColor: TColor;
begin
  Result := ColorCreate(Random(256), Random(256), Random(256), 255);  // Couleur au hasard avec opacité 100%
end;

// Initialiser le jeu
procedure InitializeGame;
begin
  // Définir la position et la taille du bouton
  ButtonRect := RectangleCreate(300, 250, 200, 50);
  ButtonState := Normal;
  BackgroundColor := WHITE;  // Couleur initiale de fond
end;

// Mise à jour de l'état du jeu
procedure UpdateGame;
var
  MousePos: TVector2;
begin
  MousePos := GetMousePosition();

  // Vérifier si la souris survole le bouton
  if CheckCollisionPointRec(MousePos, ButtonRect) then
  begin
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
    begin
      ButtonState := Clicked;
      BackgroundColor := GetRandomColor();  // Changer la couleur de fond sur clic
    end
    else
      ButtonState := Hover;
  end
  else
    ButtonState := Normal;  // Si la souris n'est pas sur le bouton
end;

// Dessiner tous les éléments à l'écran
procedure DrawGame;
begin
  BeginDrawing();
  ClearBackground(BackgroundColor);  // Effacer l'écran avec la couleur de fond actuelle

  // Dessiner le bouton avec son état actuel
  DrawButton(ButtonRect, 'Nouvelle couleur', ButtonState);

  EndDrawing();
end;

// Point d'entrée principal
begin
  InitWindow(ScreenWidth, ScreenHeight, 'Changer la couleur de fond');
  InitializeGame();
  SetTargetFPS(60);  // Régler la fréquence d'images à 60 FPS

  // Boucle principale du jeu
  while not WindowShouldClose() do
  begin
    UpdateGame();  // Mettre à jour l'état du jeu
    DrawGame();    // Dessiner le jeu
  end;

  CloseWindow();  // Fermer la fenêtre proprement
end.















