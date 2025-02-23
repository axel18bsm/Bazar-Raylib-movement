program Button3DWithBallAnimation;

{$MODE OBJFPC}  // Enable Object Pascal mode

uses
  raylib;

const
  ScreenWidth = 800;
  ScreenHeight = 600;

type
  TButtonState = (Normal, Clicked, Bouncing);

var
  ButtonState: TButtonState;
  ButtonRect: TRectangle;
  BallX, BallY: Integer;
  BallActive: Boolean;
  BounceCounter: Integer;

// Function to draw a button with 3D effect, using integer coordinates
procedure Draw3DButton(Rect: TRectangle; Text: String; State: TButtonState);
var
  ButtonX, ButtonY, ButtonWidth, ButtonHeight: Integer;
begin
  // Convert Single to Integer for Raylib's TRectangle compatibility
  ButtonX := Round(Rect.x);
  ButtonY := Round(Rect.y);
  ButtonWidth := Round(Rect.width);
  ButtonHeight := Round(Rect.height);

  // Set colors depending on state
  if State = Clicked then
  begin
    DrawRectangle(ButtonX, ButtonY, ButtonWidth, ButtonHeight, DARKGRAY);  // Main part
    DrawRectangle(ButtonX + 5, ButtonY + 5, ButtonWidth, ButtonHeight, LIGHTGRAY);  // Lower part for 3D effect
  end
  else
  begin
    DrawRectangle(ButtonX, ButtonY, ButtonWidth, ButtonHeight, LIGHTGRAY);  // Main part
    DrawRectangle(ButtonX + 5, ButtonY + 5, ButtonWidth, ButtonHeight, DARKGRAY);  // Lower part for 3D effect
  end;
  DrawText(PChar(AnsiString(Text)), Round(Rect.X) + 20, Round(Rect.Y) + 20, 30, RED);
end;

// Initialize button and ball
procedure InitializeGame;
begin
  ButtonRect := RectangleCreate(350, 250, 100, 50);  // All values are Single initially
  ButtonState := Normal;
  BallX := ScreenWidth;
  BallY := ScreenHeight div 2;
  BallActive := False;
  BounceCounter := 0;
end;

// Handle button click and ball animation
procedure UpdateGame;
begin
  // Check if mouse is over button and button is clicked
  if (ButtonState = Normal) and (CheckCollisionPointRec(GetMousePosition(), ButtonRect)) then
  begin
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
    begin
      ButtonState := Clicked;  // Change state to clicked
      BallActive := True;  // Activate ball animation
    end;
  end;

  // Ball animation
  if BallActive then
  begin
    BallX := BallX - 5;  // Move ball leftwards
    if BallX < -20 then
      BallActive := False;  // Ball leaves screen
  end;

  // Button bounce-back effect
  if ButtonState = Clicked then
  begin
    BounceCounter := BounceCounter + 1;
    if BounceCounter > 15 then
    begin
      ButtonState := Normal;
      BounceCounter := 0;
    end;
  end;
end;

// Draw all game elements
procedure DrawGame;
begin
  BeginDrawing();
  ClearBackground(RAYWHITE);

  Draw3DButton(ButtonRect, 'Feu', ButtonState);  // Draw button

  if BallActive then
    DrawCircle(BallX, BallY, 10, RED);  // Draw the animated ball

  EndDrawing();
end;

// Entry point
begin
  InitWindow(ScreenWidth, ScreenHeight, '3D Button with Ball Animation');
  InitializeGame();

  SetTargetFPS(60);

  // Main game loop
  while not WindowShouldClose() do
  begin
    UpdateGame();
    DrawGame();
  end;

  CloseWindow();  // Clean up
end.













