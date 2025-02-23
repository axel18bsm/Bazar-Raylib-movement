program threebuttonwindow;

{$mode objfpc}{$H+}

uses
  cmem, raylib, sysutils;

const
  ScreenWidth = 800;
  ScreenHeight = 450;
  ButtonWidth = 150;
  ButtonHeight = 50;

var
  Button1Rect, Button2Rect, Button3Rect: TRectangle;
  BackgroundColor: TColorb;
  ShowMessage: Boolean;

begin
  // Initialization
  InitWindow(ScreenWidth, ScreenHeight, 'Three Button Window');
  SetTargetFPS(60);

  // Define buttons and states
  Button1Rect := RectangleCreate(50, ScreenHeight div 2 - ButtonHeight div 2, ButtonWidth, ButtonHeight);
  Button2Rect := RectangleCreate(ScreenWidth div 2 - ButtonWidth div 2, ScreenHeight div 2 - ButtonHeight div 2, ButtonWidth, ButtonHeight);
  Button3Rect := RectangleCreate(ScreenWidth - 200, ScreenHeight div 2 - ButtonHeight div 2, ButtonWidth, ButtonHeight);
  BackgroundColor := DARKGRAY;
  ShowMessage := False;

  // Main game loop
  while not WindowShouldClose() do
  begin
    // Update logic
    if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
    begin
      if CheckCollisionPointRec(GetMousePosition(), Button1Rect) then
        CloseWindow();

      if CheckCollisionPointRec(GetMousePosition(), Button2Rect) then
        ShowMessage := not ShowMessage;

      if CheckCollisionPointRec(GetMousePosition(), Button3Rect) then
      begin
        if BackgroundColor = DARKGRAY then
          BackgroundColor := LIGHTGRAY
        else
          BackgroundColor := DARKGRAY;
      end;
    end;

    // Draw
    BeginDrawing();
    ClearBackground(BackgroundColor);

    DrawText('Button 1: Quitter', Button1Rect.x + 10, Button1Rect.y + 15, 20, WHITE);
    DrawText('Button 2: Message', Button2Rect.x + 10, Button2Rect.y + 15, 20, WHITE);
    DrawText('Button 3: Couleur', Button3Rect.x + 10, Button3Rect.y + 15, 20, WHITE);

    if ShowMessage then
      DrawText('Il fait beau', ScreenWidth div 2 - 60, ScreenHeight - 50, 20, GREEN);

    EndDrawing();
  end;

  // De-Initialization
  CloseWindow();
end.


// Gère la détection de clic sur une case et met à jour les informations
procedure HandleMouseClick();
var
  mouseX, mouseY, col, row: Integer;
begin
  mouseX := GetMouseX();
  mouseY := GetMouseY();
  if (mouseX >= 0) and (mouseX < Columns * CellSize) and (mouseY >= 0) and (mouseY < Rows * CellSize) then
  begin
    col := mouseX div CellSize;
    row := mouseY div CellSize;
    SelectedLargeRow := row;
    SelectedLargeCol := col;
    LargeCellSelected := True;  // Grand carré sélectionné
    CellSelected := True;
    StrPCopy(SelectedCellInfo, Format('Case #%d\nCouleur: %d\n%s',
      [Cells[row, col].Number, Cells[row, col].ColorIndex, ColorToRGBString(Cells[row, col].Color)]));
  end;
end;

// Gère les clics de la souris sur les boutons et les cases
procedure HandleMouseInput();
begin
  SaveButtonPressed := False;
  LoadButtonPressed := False;
  NewGridButtonPressed := False;
  EmptyGridButtonPressed := False;

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    if CheckCollisionPointRec(GetMousePosition(), SaveButtonRec) then
    begin
      SaveButtonPressed := True;
      SaveGridToFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), LoadButtonRec) then
    begin
      LoadButtonPressed := True;
      LoadGridFromFile();
    end;
    if CheckCollisionPointRec(GetMousePosition(), NewGridButtonRec) then
    begin
      NewGridButtonPressed := True;
      ResetGrid();
    end;
    if CheckCollisionPointRec(GetMousePosition(), EmptyGridButtonRec) then
    begin
      EmptyGridButtonPressed := True;
      ClearGrid(); // Appelle la procédure de réinitialisation de la grille vide
    end;
  end;


  HandleMouseClick();
  HandleSmallSquareClick();
end;

// Dessine la grille avec les informations de chaque case
procedure DrawGridWithCellInfo();
var
  xPos, yPos, j, i: Integer;
  numText, clrText: array[0..15] of Char;
begin
  for j := 0 to Rows - 1 do
    for i := 0 to Columns - 1 do
    begin
      xPos := i * CellSize;
      yPos := j * CellSize;
      DrawRectangle(xPos, yPos, CellSize, CellSize, Cells[j, i].Color);
      StrPCopy(numText, IntToStr(Cells[j, i].Number));
      DrawText(numText, xPos + TextPadding, yPos + TextPadding, SmallFontSize, BLACK);
      StrPCopy(clrText, 'Clr: ' + IntToStr(Cells[j, i].ColorIndex));
      DrawText(clrText, xPos + TextPadding, yPos + 20, SmallFontSize, BLACK);
    end;
  if CellSelected then
  begin
    xPos := SelectedCol * CellSize;
    yPos := SelectedRow * CellSize;
    DrawRectangleLinesEx(RectangleCreate(xPos, yPos, CellSize, CellSize), 3, ORANGE);
  end;
end;
procedure DrawColorPalette();
var
  xPos, yPos, colorIndex: Integer;
  numText: array[0..5] of Char;
begin
  for colorIndex := 1 to 10 do
  begin
    // Coordonnées pour chaque petit carré
    xPos := (colorIndex - 1) mod 5 * (SmallCellSize + 10) + Columns * CellSize + 30;
    yPos := ((colorIndex - 1) div 5) * (SmallCellSize + 10) + 200;  // Dessine 2 lignes (2 x 5)

    // Dessine chaque petit carré de couleur
    DrawRectangle(xPos, yPos, SmallCellSize, SmallCellSize, Colors[colorIndex]);

    // Affiche le numéro correspondant à la couleur dans le carré
    StrPCopy(numText, IntToStr(colorIndex));
    DrawText(numText, xPos + 15, yPos + 10, SmallFontSize, BLACK);
  end;
end;

// Affiche les informations de la case sélectionnée dans un rectangle dédié
procedure DrawSelectedCellInfo();
begin
  DrawRectangle(Columns * CellSize + 20, 20, InfoBoxWidth, 150, LIGHTGRAY);
  DrawRectangleLines(Columns * CellSize + 20, 20, InfoBoxWidth, 150, DARKGRAY);
  if CellSelected then
    DrawText(SelectedCellInfo, Columns * CellSize + 30, 30, SmallFontSize, BLACK)
  else
    DrawText('Cliquez sur une case pour afficher ses informations.', Columns * CellSize + 30, 30, SmallFontSize, BLACK);
end;

// Dessine les boutons de sauvegarde, chargement et "Nouvelle Grille"
procedure DrawButtons();
begin
  if SaveButtonPressed then
    DrawRectangleRec(SaveButtonRec, GRAY)
  else
    DrawRectangleRec(SaveButtonRec, LIGHTGRAY);

  if LoadButtonPressed then
    DrawRectangleRec(LoadButtonRec, GRAY)
  else
    DrawRectangleRec(LoadButtonRec, LIGHTGRAY);

  if NewGridButtonPressed then
    DrawRectangleRec(NewGridButtonRec, GRAY)
  else
    DrawRectangleRec(NewGridButtonRec, LIGHTGRAY);

  if EmptyGridButtonPressed then
    DrawRectangleRec(EmptyGridButtonRec, GRAY)
  else
    DrawRectangleRec(EmptyGridButtonRec, LIGHTGRAY);

  DrawText('Sauvegarder', Round(SaveButtonRec.x + 10), Round(SaveButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Charger', Round(LoadButtonRec.x + 10), Round(LoadButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Nouvelle Grille', Round(NewGridButtonRec.x + 10), Round(NewGridButtonRec.y + 10), SmallFontSize, BLACK);
  DrawText('Grille Vide', Round(EmptyGridButtonRec.x + 10), Round(EmptyGridButtonRec.y + 10), SmallFontSize, BLACK);

end;

// Programme principal
begin
  Colors[1] := BLUE; Colors[2] := GREEN; Colors[3] := YELLOW; Colors[4] := ORANGE;
  Colors[5] := PURPLE; Colors[6] := SKYBLUE; Colors[7] := PINK; Colors[8] := BROWN;
  Colors[9] := DARKGREEN; Colors[10] := LIME;

  screenWidth := Columns * CellSize + InfoBoxWidth + 40;
  screenHeight := Rows * CellSize + 250; // Augmenter la hauteur de la fenêtre pour le troisième bouton

  InitWindow(screenWidth, screenHeight, 'Grid with Bounce Effect');
  SetTargetFPS(60);

  InitializeCells();
  CellSelected := False;

  SaveButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 200, ButtonWidth, ButtonHeight);
  LoadButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 150, ButtonWidth, ButtonHeight);
  NewGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 100, ButtonWidth, ButtonHeight);
   EmptyGridButtonRec := RectangleCreate(screenWidth - 180, screenHeight - 50, ButtonWidth, ButtonHeight); // Ajout du bouton "Grille Vide"

  while not WindowShouldClose() do
  begin
    HandleMouseInput();

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawGridWithCellInfo();
    DrawSelectedCellInfo();
    DrawButtons();
    DrawColorPalette(); // Affiche la palette de couleurs
    EndDrawing();
  end;

  CloseWindow();
end.






















